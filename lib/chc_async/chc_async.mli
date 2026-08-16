(** Jane Street Async driver for {!Chc}.

    {!Chc.Protocol} is sans-IO — it parses and serialises, and never touches a socket. This replaces {!Chc.Client}'s blocking pump with an
    Async one and reuses everything else: block decoding, the writer, the protocol state machine.

    Server-side errors raise {!Chc.Error} into the enclosing monitor, as {!Chc.Client} raises them to its caller. Catch with
    [Monitor.try_with] rather than expecting an [Or_error.t]. *)

open! Core
open! Async

type t

(** Connect and complete the handshake. TLS is not handled here.

    [write_buffer_high_water] (default 4 MiB) is where a send starts applying backpressure: past it, {!Inserter.commit} and {!insert} wait
    for the socket to drain rather than growing the out buffer. *)
val connect
  :  ?port:int
  -> ?database:string
  -> ?user:string
  -> ?password:string
  -> ?client_name:string
  -> ?read_buffer_bytes:int
  -> ?compression:[ `None | `Lz4 | `Zstd ]
  -> ?write_buffer_high_water:int
  -> string
  -> t Deferred.t

val close : t -> unit Deferred.t
val server_info : t -> Chc.Protocol.server_info
val compression : t -> [ `None | `Lz4 | `Zstd ]

(** {1 Queries}

    A connection runs one statement at a time. Overlapping calls queue rather than interleave, so two [query_pipe]s on one connection are
    sequential, not concurrent — give a second query its own connection if you want both in flight. *)

(** Blocks as the server sends them. Reading is driven by the consumer: the connection stops pulling from the socket while the pipe is
    full, so a slow reader applies backpressure instead of accumulating blocks in memory.

    The first block normally carries the schema and no rows; it is passed through rather than hidden, since it is the cheapest way to
    learn a result's shape. The empty no-column block that terminates a response is not — that one is a stream marker, not data.

    Closing the reader early is fine: the rest of the response is drained in the background and the connection stays usable. *)
val query_pipe : t -> ?params:(string * Chc.Param.t) list -> string -> Chc.block Pipe.Reader.t

(** {!query_pipe} with each block decoded into rows. *)
val fetch_pipe : t -> ?params:(string * Chc.Param.t) list -> string -> 'a Chc.Row.t -> 'a Pipe.Reader.t

val query : t -> ?params:(string * Chc.Param.t) list -> string -> Chc.block list Deferred.t
val fetch : t -> ?params:(string * Chc.Param.t) list -> string -> 'a Chc.Row.t -> 'a list Deferred.t
val fetch_one : t -> ?params:(string * Chc.Param.t) list -> string -> 'a Chc.Row.t -> 'a option Deferred.t

(** Run a statement and discard its output. *)
val execute : t -> ?params:(string * Chc.Param.t) list -> string -> unit Deferred.t

val ping : t -> unit Deferred.t

(** {1 Writing} *)

(** One statement, all rows, as {!Chc.Client.insert}. Use {!Inserter} for a feed. *)
val insert : ?columns:string list -> ?batch_size:int -> t -> string -> Chc.value array array -> unit Deferred.t

(** A long-lived INSERT that stays open across flushes.

    The point is that finishing an INSERT statement is the expensive part — tens of milliseconds of server-side work, the same for any
    client — while pushing another block into one already open is microseconds. A writer fed by a stream should therefore open once and
    flush often, which is what this is for. Closing and reopening per flush costs the same as not batching at all.

    {[
      let%bind ins = Chc_async.Inserter.create conn "funding_rates" ~max_rows:50_000 in
      Pipe.iter reader ~f:(fun row ->
        let%bind () = Chc_async.Inserter.write ins (encode row) in
        Chc_async.Inserter.commit ins)
    ]}

    Flush policy is the caller's: {!commit} flushes only when a threshold is crossed, so a periodic tick that calls it is what turns time
    into a flush. That mirrors how a feed writer is usually shaped, and keeps the timer in the scheduler rather than in here. *)
module Inserter : sig
  type conn := t
  type t

  (** Opens the statement and waits for the server's schema block, which is where the column types come from. The connection is taken over
      for the inserter's lifetime: queries on it raise until {!close}.

      [max_rows] (default 65536) and [max_bytes] (default 8 MiB, counted as a rough pre-encoding estimate) are the thresholds {!commit}
      checks. *)
  val create : conn -> ?columns:string list -> ?max_rows:int -> ?max_bytes:int -> string -> t Deferred.t

  (** Buffer one row. Cheap and normally already determined; it is a [Deferred.t] because a write that crosses [max_rows] flushes, and a
      flush can block on the socket. *)
  val write : t -> Chc.value array -> unit Deferred.t

  (** Flush if a threshold has been crossed, otherwise do nothing. Safe and cheap to call on every turn of a select loop. *)
  val commit : t -> unit Deferred.t

  (** Flush whatever is buffered, thresholds or not. *)
  val flush : t -> unit Deferred.t

  (** Flush, terminate the statement, wait for the server to accept it, and hand the connection back. *)
  val close : t -> unit Deferred.t

  (** Rows buffered since the last flush. *)
  val pending_rows : t -> int

  (** Rows the server has accepted, across every flush so far. Blocks already sent stay committed even if a later one fails — a streaming
      INSERT has no rollback. *)
  val written_rows : t -> int
end
