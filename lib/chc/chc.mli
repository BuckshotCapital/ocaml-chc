(** OCaml bindings to {{:https://github.com/ClickHouse/clickhouse-c}clickhouse-c}.

    Two entry points: {!Client} runs queries and INSERTs over the native TCP protocol, and {!open_fd} decodes bare [FORMAT Native] blocks
    off a descriptor with no server involved.

    The network path binds [clickhouse-async.h], which never touches a socket, so sockets and TLS stay on the OCaml side and the same core
    works under Unix, Lwt and Eio — {!Client} is just the blocking driver over it. LZ4 and ZSTD are linked but dormant unless asked for at
    connect time. *)

(** {1 Errors} *)

module Error : sig
  type t =
    { code : int (** [CHC_ERR_*] ordinal *)
    ; server_code : int (** ClickHouse server error code, 0 if none *)
    ; msg : string
    ; server_name : string
    }

  val to_string : t -> string
end

exception Error of Error.t

(** {1 Types} *)

module Kind : sig
  (** ClickHouse type kinds, mirroring [chc_kind]. *)
  type t =
    | Void
    | Int8
    | Int16
    | Int32
    | Int64
    | Int128
    | Int256
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | UInt128
    | UInt256
    | Float32
    | Float64
    | BFloat16
    | Bool
    | Date
    | Date32
    | DateTime
    | DateTime64
    | Time
    | Time64
    | String
    | FixedString
    | Decimal32
    | Decimal64
    | Decimal128
    | Decimal256
    | UUID
    | IPv4
    | IPv6
    | Enum8
    | Enum16
    | Nullable
    | Array
    | Tuple
    | Map
    | Nested
    | LowCardinality
    | Interval
    | Point
    | Ring
    | LineString
    | Polygon
    | MultiPolygon
    | MultiLineString
    | Variant
    | Dynamic
    | JSON
    | Object
    | AggregateFunction
    | SimpleAggregateFunction
    | QBit
    | Nothing

  val to_string : t -> string
end

module Layout : sig
  (** Physical column layout, mirroring [chc_col_kind]. Several {!Kind.t}s share one layout — [Map] arrives as [Array] over a [Tuple]. *)
  type t =
    | Fixed
    | String
    | Nullable
    | Array
    | Tuple
    | Low_cardinality
    | Nothing
end

(** {1 Values} *)

(** A decoded cell.

    Wide fixed-width types that exceed OCaml's integer types are rendered to exact text rather than truncated: {!Big} for
    [Int128]/[Int256]/[UInt128]/[UInt256], {!Decimal} for the [Decimal*] family with the type's scale already applied.
    {!Uuid} and {!Ip} carry canonical text matching ClickHouse's own [toString], for [UUID] and for [IPv4]/[IPv6]
    respectively.

    {!Raw} is the fallback for anything still unmodelled, holding little-endian wire bytes rather than a guess.

    [Map] decodes as {!Arr} of two-element {!Tup}, matching its physical layout. *)
type value =
  | Null
  | Bool of bool
  | Int of int64 (** signed integers, and scaled time mantissas *)
  | Uint of int64 (** unsigned; reinterpret via [Int64.unsigned_*] *)
  | Float of float
  | Str of string
  | Raw of string (** fixed-width bytes, little-endian *)
  | Big of string (** exact decimal text for 128/256-bit integers *)
  | Decimal of string (** exact decimal text, scale applied *)
  | Uuid of string (** canonical 8-4-4-4-12 *)
  | Ip of string (** dotted quad, or RFC 5952 for IPv6 *)
  | Arr of value array
  | Tup of value array

val string_of_value : value -> string

(** Which IPv4/IPv6 implementation was compiled in: ["ipaddr"] when that library was available at build time, ["builtin"]
    otherwise. Behaviour is identical either way — both are checked against the server's own rendering — so this is for
    confirming what a deployment actually got, not for branching on. *)
val ip_backend : string

(** {1 Blocks} *)

type block

val n_rows : block -> int
val n_columns : block -> int
val column_name : block -> int -> string

(** Printable ClickHouse type, e.g. ["Array(Nullable(String))"]. *)
val column_type_name : block -> int -> string

val column_kind : block -> int -> Kind.t

(** @raise Invalid_argument on a schema-only block, which has no column tree. See {!column}. *)
val column_layout : block -> int -> Layout.t

(** Decode column [i] in full. Values are OCaml-owned, so they outlive the block; nothing aliases C memory.

    Returns [[||]] for a schema-only block. The TCP path opens every query response with one: it carries column
    names and types but zero rows and no column tree, which is exactly what makes it useful. *)
val column : block -> int -> value array

val columns : block -> value array array

(** Row-major transpose of {!columns}. Convenient, not efficient. *)
val rows : block -> value array array

(** {1 Typed row decoding}

    Turns a block's [value array]s into records, resolving columns by name so a
    change in [SELECT] order cannot silently reinterpret a row.

    {[
      type spread = { asset : string; adj : float; mkts : int }

      let spread =
        let open Chc.Row in
        let+ asset = field "asset" string
        and+ adj = field "adj_pct" float
        and+ mkts = field "mkts" int in
        { asset; adj; mkts }
      ;;

      let top = Chc.Client.fetch c "SELECT ..." spread
    ]} *)

module Row : sig
  (** Carries the column name, its ClickHouse type, the row index, what was
      expected and what was found. *)
  exception Decode_error of string

  (** How to read one cell. *)
  type 'a conv

  val string : string conv

  (** Any value, via its canonical rendering. *)
  val text : string conv

  val int : int conv
  val int64 : int64 conv

  (** Integers are promoted, since aggregates flip between [UInt64] and
      [Float64] depending on the function. *)
  val float : float conv

  (** Accepts a real [Bool], or [UInt8] 0/1. *)
  val bool : bool conv

  val uuid : string conv
  val ip : string conv
  val big : string conv
  val decimal : string conv
  val raw : string conv
  val value : value conv
  val opt : 'a conv -> 'a option conv
  val array : 'a conv -> 'a array conv
  val pair : 'a conv -> 'b conv -> ('a * 'b) conv
  val map_conv : ('a -> 'b) -> 'a conv -> 'b conv

  (** A whole-row decoder. *)
  type 'a t

  (** @raise Decode_error if no column has that name, listing the ones that do. *)
  val field : string -> 'a conv -> 'a t

  (** Positional, for columns whose generated name is awkward. *)
  val at : int -> 'a conv -> 'a t

  val const : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

(** Each column is decoded at most once per block, and only if a field names
    it — selecting ten columns and reading two costs two decodes. *)
val decode_block : block -> 'a Row.t -> 'a array

(** {1 Readers} *)

type reader

(** Stream blocks from [fd]. The descriptor is not closed by {!close}.

    Defaults suit [clickhouse local], which emits neither a [BlockInfo] prefix ([has_block_info], default [false]) nor per-column
    custom-serialization flags ([has_custom_serialization], default [false]). The TCP path sets both depending on server revision.

    [validate] (default [true]) walks each column tree checking the invariants the server enforces — monotonic array offsets, in-range
    LowCardinality keys. [chc_block_read] does not check these, and a forged block can otherwise drive reads past an inner column's bounds.
    Turn it off only for trusted input on a hot path. *)
val open_fd
  :  ?has_block_info:bool
  -> ?has_custom_serialization:bool
  -> ?read_buffer_bytes:int
  -> ?validate:bool
  -> Unix.file_descr
  -> reader

(** Next block, or [None] at clean EOF. Blocks while waiting on [fd], with the OCaml runtime lock released.

    @raise Error on I/O or protocol failure. *)
val read_block : reader -> block option

(** Release the read buffer. Idempotent; the finalizer also does this. *)
val close : reader -> unit

(** Apply to each block until EOF. *)
val iter : reader -> (block -> unit) -> unit

val fold : reader -> init:'a -> f:('a -> block -> 'a) -> 'a

(** {1 Async client}

    The sans-IO core: it never touches a socket. Feed it inbound bytes with
    {!Async.submit}, drain outbound bytes with {!Async.pending_out} /
    {!Async.consume_out}, and drive it from whatever reactor you like. {!Client}
    is a blocking driver built on this; an Lwt or Eio one would replace only the
    pump. *)

module Async : sig
  type t

  type exn_info =
    { code : int
    ; name : string
    ; display_text : string
    ; stack_trace : string
    }

  type progress =
    { rows : int
    ; bytes : int
    ; total_rows : int
    ; written_rows : int (** 0 below server revision 54420 *)
    ; written_bytes : int
    }

  type profile_info =
    { p_rows : int
    ; p_blocks : int
    ; p_bytes : int
    ; rows_before_limit : int
    ; applied_limit : bool
    ; calculated_rows_before_limit : bool
    }

  type server_info =
    { server_name : string
    ; timezone : string
    ; display_name : string
    ; version_major : int
    ; version_minor : int
    ; version_patch : int
    ; revision : int (** negotiated: [min (client, server)] *)
    }

  type packet =
    | Pong
    | End_of_stream
    | Table_columns
    | Hello
    | Data of block
    | Totals of block
    | Extremes of block
    | Log of block
    | Profile_events of block
    | Exception of exn_info
    (** Server-side errors arrive as this packet, not as a raised
            {!Error} — only transport failures raise. *)
    | Progress of progress
    | Profile_info of profile_info

  (** Allocates only; performs no I/O and cannot fail on the network. *)
  val create
    :  ?client_name:string
    -> ?database:string
    -> ?user:string
    -> ?password:string
    -> ?read_buffer_bytes:int
    -> ?compression:[ `None | `Lz4 | `Zstd ]
    -> unit
    -> t

  (** [true] once the Hello exchange completes. [false] means the input buffer
      drained mid-parse: submit more bytes and call again. Parse state survives
      the retry. *)
  val handshake : t -> bool

  (** Appends to the out buffer; never blocks. *)
  val send_query : t -> ?query_id:string -> string -> unit

  (** Appends the empty block terminating an INSERT row stream. *)
  val send_data_end : t -> unit

  (** [None] means the input buffer drained mid-parse — submit more bytes and
      retry. A Data block spread over many reads resumes at the in-progress
      column rather than re-parsing. *)
  val recv_packet : t -> packet option

  (** [submit t buf n] feeds the first [n] bytes of [buf] as inbound socket
      data. Copied internally. *)
  val submit : t -> Bytes.t -> int -> unit

  (** Bytes the client wants written to the socket; [""] when idle. *)
  val pending_out : t -> string

  (** Report how many of {!pending_out}'s bytes the socket actually accepted; a
      partial write is fine. There is no write backpressure — sends never block
      and grow the out buffer, so watch its length if you pipeline. *)
  val consume_out : t -> int -> unit

  (** Meaningful only after {!handshake} returns [true]. *)
  val server_info : t -> server_info

  (** What the client actually negotiated, read back off the client rather than
      echoed from {!create}'s argument — passing a codec that fails to take
      silently degrades to [`None], and this is what tells the two apart. *)
  val compression : t -> [ `None | `Lz4 | `Zstd ]

  val close : t -> unit
end

(** {1 Blocking client} *)

module Client : sig
  type t

  (** Connect over the native TCP protocol (default port 9000) and complete the
      handshake. TLS is not handled here: terminate it in OCaml and drive
      {!Async} directly.

      @raise Error on handshake or authentication failure. *)
  val connect
    :  ?port:int
    -> ?database:string
    -> ?user:string
    -> ?password:string
    -> ?client_name:string
    -> ?read_buffer_bytes:int
    -> ?socket_buffer_bytes:int
    -> ?compression:[ `None | `Lz4 | `Zstd ]
    -> string
    -> t

  (** Run [sql] and apply [f] to each Data block carrying columns, draining the
      response to end-of-stream. The first block normally has zero rows and
      exists to carry the schema; it is passed through rather than hidden.

      @raise Error with [server_code] set if the server returns an exception. *)
  val query_iter : t -> string -> f:(block -> unit) -> unit

  val query_fold : t -> string -> init:'a -> f:('a -> block -> 'a) -> 'a
  val query : t -> string -> block list

  (** Column names plus every row decoded, flattened across blocks. Convenient
      for small results; use {!query_iter} to stream. *)
  val query_rows : t -> string -> string array * value array array

  (** Run [sql] and decode every row. Convenient for results that fit in
      memory; use {!fetch_iter} to stream. *)
  val fetch : t -> string -> 'a Row.t -> 'a list

  (** Streaming {!fetch}: [f] is applied per row, block by block. *)
  val fetch_iter : t -> string -> 'a Row.t -> f:('a -> unit) -> unit

  (** First row only. Note this still drains the response. *)
  val fetch_one : t -> string -> 'a Row.t -> 'a option

  val ping : t -> unit

  (** Run a statement and discard its output. *)
  val execute : t -> string -> unit

  (** [insert t table rows] with [rows] row-major. Column types come from the
      schema block the server returns for the INSERT, so the wire types are
      always the server's own; [columns] restricts and orders the target
      columns exactly as it would in SQL.

      Rows are transposed and shipped in batches of [batch_size] (default
      65536) rather than as one block, and the out buffer is flushed between
      batches — sends never block, so nothing else applies backpressure.

      @raise Error on a server-side rejection, or on a column type the writer
      does not support. See {!Async.send_block}. *)
  val insert : ?columns:string list -> ?batch_size:int -> t -> string -> value array array -> unit

  val server_info : t -> Async.server_info
  val compression : t -> [ `None | `Lz4 | `Zstd ]
  val close : t -> unit
end
