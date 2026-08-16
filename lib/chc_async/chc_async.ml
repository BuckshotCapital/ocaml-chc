open! Core
open! Async

(* Reading and writing both go through one connection state machine, so a
   connection can only be doing one thing at a time. Queries take the sequencer;
   an inserter takes the whole connection for its lifetime, since its statement
   stays open across many calls and cannot be interleaved with anything. *)
type mode =
  | Idle
  | Inserting

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; p : Chc.Protocol.t
  ; buf : Bytes.t
  ; seq : unit Sequencer.t
  ; high_water : int
  ; mutable mode : mode
  ; mutable closed : bool
  }

let error ?(code = 0) ?(server_code = 0) ?(server_name = "") msg = raise (Chc.Error { Chc.Error.code; server_code; msg; server_name })

let server_error (e : Chc.Protocol.exn_info) =
  error
    ~server_code:e.Chc.Protocol.code
    ~server_name:e.Chc.Protocol.name
    (if String.is_empty e.Chc.Protocol.display_text then "server error" else e.Chc.Protocol.display_text)
;;

let check t =
  if t.closed then failwith "Chc_async: connection is closed";
  match t.mode with
  | Idle -> ()
  | Inserting -> failwith "Chc_async: connection is held by an open Inserter; use a separate connection to query"
;;

(* Hand the out buffer to Async's writer, which owns partial writes and
   scheduling. Backpressure is deliberately not automatic: only past the high
   water mark does a send wait, so an ordinary query never pays for a flush it
   does not need. *)
let flush_out t =
  let out = Chc.Protocol.pending_out t.p in
  match String.length out with
  | 0 -> Deferred.unit
  | n ->
    Writer.write t.writer out;
    Chc.Protocol.consume_out t.p n;
    if Writer.bytes_to_write t.writer > t.high_water then Writer.flushed t.writer else Deferred.unit
;;

let read_more t =
  match%bind Reader.read t.reader t.buf with
  | `Eof -> error ~code:1 (* CHC_ERR_IO *) "connection closed by peer"
  | `Ok n ->
    Chc.Protocol.submit t.p t.buf n;
    Deferred.unit
;;

(* One turn of the reactor, as Chc.Client.pump is for the blocking driver:
   everything the client wants to send goes out before we wait on a read, or a
   query sitting in the out buffer deadlocks against a server waiting for it. *)
let rec recv t =
  match Chc.Protocol.recv_packet t.p with
  | Some pkt -> return pkt
  | None ->
    let%bind () = flush_out t in
    let%bind () = Writer.flushed t.writer in
    let%bind () = read_more t in
    recv t
;;

let connect
      ?(port = 9000)
      ?(database = "default")
      ?(user = "default")
      ?(password = "")
      ?(client_name = "ocaml-chc")
      ?(read_buffer_bytes = 0)
      ?(compression = `None)
      ?(write_buffer_high_water = 4 * 1024 * 1024)
      host
  =
  let%bind sock, reader, writer = Tcp.connect (Tcp.Where_to_connect.of_host_and_port { Host_and_port.host; port }) in
  Socket.setopt sock Socket.Opt.nodelay true;
  let t =
    { reader
    ; writer
    ; p = Chc.Protocol.create ~client_name ~database ~user ~password ~read_buffer_bytes ~compression ()
    ; buf = Bytes.create 65536
    ; seq = Sequencer.create ~continue_on_error:true ()
    ; high_water = write_buffer_high_water
    ; mode = Idle
    ; closed = false
    }
  in
  let rec drive () =
    if Chc.Protocol.handshake t.p
    then Deferred.unit
    else (
      let%bind () = flush_out t in
      let%bind () = Writer.flushed t.writer in
      let%bind () = read_more t in
      drive ())
  in
  match%bind Monitor.try_with ~run:`Now (fun () -> drive ()) with
  | Ok () -> return t
  | Error exn ->
    Chc.Protocol.close t.p;
    let%bind () = Writer.close t.writer in
    let%bind () = Reader.close t.reader in
    raise exn
;;

let close t =
  if t.closed
  then Deferred.unit
  else (
    t.closed <- true;
    Chc.Protocol.close t.p;
    let%bind () = Writer.close t.writer in
    Reader.close t.reader)
;;

let server_info t = Chc.Protocol.server_info t.p
let compression t = Chc.Protocol.compression t.p

(* Read to the end of the response, discarding. What makes an abandoned pipe
   safe: the statement finishes and the connection is reusable. *)
let rec drain t =
  match%bind recv t with
  | Chc.Protocol.End_of_stream -> Deferred.unit
  | Chc.Protocol.Exception e -> server_error e
  | _ -> drain t
;;

let query_pipe t ?(params = []) sql =
  check t;
  Pipe.create_reader ~close_on_exception:false (fun w ->
    Throttle.enqueue t.seq (fun () ->
      Chc.Protocol.send_query t.p ~params sql;
      let rec loop () =
        match%bind recv t with
        | Chc.Protocol.End_of_stream -> Deferred.unit
        | Chc.Protocol.Exception e -> server_error e
        (* A response ends with an empty block that has no columns at all — a
           stream terminator, not data. The schema block (columns, no rows) is
           passed through, since it is the cheapest way to learn a result's
           shape; this one would only break a decoder. *)
        | Chc.Protocol.Data b when Chc.n_columns b = 0 -> loop ()
        | Chc.Protocol.Data b -> if Pipe.is_closed w then drain t else Pipe.write w b >>= loop
        | _ -> loop ()
      in
      loop ()))
;;

let fetch_pipe t ?(params = []) sql d = Pipe.concat_map_list (query_pipe t ~params sql) ~f:(fun b -> Array.to_list (Chc.decode_block b d))
let query t ?(params = []) sql = Pipe.to_list (query_pipe t ~params sql)
let fetch t ?(params = []) sql d = Pipe.to_list (fetch_pipe t ~params sql d)

let fetch_one t ?(params = []) sql d =
  (* Still drains the response, as the blocking client does. *)
  let%map rows = fetch t ~params sql d in
  List.hd rows
;;

let execute t ?(params = []) sql = Pipe.drain (query_pipe t ~params sql)

let ping t =
  let%map (_ : Chc.block list) = query t "SELECT 1" in
  ()
;;

(* ---------------------------------------------------------------------- *)
(* Writing                                                                *)
(* ---------------------------------------------------------------------- *)

(* Open an INSERT and take the schema block the server answers it with, which is
   where the column names and types come from — always the server's own. *)
let open_insert t ?columns table =
  let collist =
    match columns with
    | None -> ""
    | Some cs -> " (" ^ String.concat ~sep:", " cs ^ ")"
  in
  Chc.Protocol.send_query t.p (sprintf "INSERT INTO %s%s VALUES" table collist);
  let rec await () =
    match%bind recv t with
    | Chc.Protocol.Data b ->
      let n = Chc.n_columns b in
      return (Array.init n ~f:(Chc.column_name b), Array.init n ~f:(Chc.column_type_name b))
    | Chc.Protocol.Exception e -> server_error e
    | Chc.Protocol.End_of_stream -> failwith "Chc_async: server closed the stream before sending a schema"
    | _ -> await ()
  in
  await ()
;;

let rec finish t =
  match%bind recv t with
  | Chc.Protocol.End_of_stream -> Deferred.unit
  | Chc.Protocol.Exception e -> server_error e
  | _ -> finish t
;;

let send_rows t ~names ~types ~rows ~start ~len =
  let n_cols = Array.length names in
  let columns = Array.init n_cols ~f:(fun c -> Array.init len ~f:(fun r -> rows.(start + r).(c))) in
  Chc.Protocol.send_block t.p ~names ~types ~columns ~n_rows:len;
  flush_out t
;;

let insert ?columns ?(batch_size = 65536) t table rows =
  check t;
  Throttle.enqueue t.seq (fun () ->
    let%bind names, types = open_insert t ?columns table in
    let n_cols = Array.length names in
    Array.iteri rows ~f:(fun r row ->
      if Array.length row <> n_cols then failwithf "Chc_async.insert: row %d has %d values, expected %d" r (Array.length row) n_cols ());
    let total = Array.length rows in
    let rec batches start =
      if start >= total
      then Deferred.unit
      else (
        let len = min batch_size (total - start) in
        let%bind () = send_rows t ~names ~types ~rows ~start ~len in
        batches (start + len))
    in
    (* Whatever happens, terminate the stream: the server is mid-statement and
       would reject the next query otherwise. *)
    match%bind Monitor.try_with ~run:`Now (fun () -> batches 0) with
    | Ok () ->
      Chc.Protocol.send_data_end t.p;
      finish t
    | Error exn ->
      let%bind () =
        match%map
          Monitor.try_with ~run:`Now (fun () ->
            Chc.Protocol.send_data_end t.p;
            finish t)
        with
        | Ok () | Error _ -> ()
      in
      raise exn)
;;

module Inserter = struct
  type conn = t

  type t =
    { conn : conn
    ; names : string array
    ; types : string array
    ; max_rows : int
    ; max_bytes : int
    ; mutable buf : Chc.value array list (* reversed *)
    ; mutable n : int
    ; mutable bytes : int
    ; mutable written : int
    ; mutable closed : bool
    }

  (* Rough, and deliberately so: the point is to bound the flush size, not to
     predict the wire exactly. Strings dominate, everything else is small. *)
  let rec size_of (v : Chc.value) =
    match v with
    | Chc.Null -> 1
    | Chc.Bool _ -> 1
    | Chc.Int _ | Chc.Uint _ | Chc.Float _ -> 8
    | Chc.Str s | Chc.Raw s | Chc.Big s | Chc.Decimal s | Chc.Uuid s | Chc.Ip s -> String.length s + 8
    | Chc.Arr a | Chc.Tup a -> Array.fold a ~init:8 ~f:(fun acc v -> acc + size_of v)
  ;;

  let create (conn : conn) ?columns ?(max_rows = 65536) ?(max_bytes = 8 * 1024 * 1024) table =
    check conn;
    let%bind names, types = Throttle.enqueue conn.seq (fun () -> open_insert conn ?columns table) in
    conn.mode <- Inserting;
    return { conn; names; types; max_rows; max_bytes; buf = []; n = 0; bytes = 0; written = 0; closed = false }
  ;;

  let check_open t = if t.closed then failwith "Chc_async.Inserter: already closed"

  let flush t =
    check_open t;
    if t.n = 0
    then Deferred.unit
    else (
      let rows = Array.of_list_rev t.buf in
      let len = t.n in
      t.buf <- [];
      t.n <- 0;
      t.bytes <- 0;
      let%map () = send_rows t.conn ~names:t.names ~types:t.types ~rows ~start:0 ~len in
      t.written <- t.written + len)
  ;;

  let write t row =
    check_open t;
    let n_cols = Array.length t.names in
    if Array.length row <> n_cols then failwithf "Chc_async.Inserter.write: row has %d values, expected %d" (Array.length row) n_cols ();
    t.buf <- row :: t.buf;
    t.n <- t.n + 1;
    t.bytes <- t.bytes + Array.fold row ~init:0 ~f:(fun acc v -> acc + size_of v);
    (* A row that crosses a threshold flushes here rather than waiting for a
       commit that might not come until the next tick. *)
    if t.n >= t.max_rows || t.bytes >= t.max_bytes then flush t else Deferred.unit
  ;;

  let commit t =
    check_open t;
    if t.n >= t.max_rows || t.bytes >= t.max_bytes then flush t else Deferred.unit
  ;;

  let close t =
    if t.closed
    then Deferred.unit
    else (
      let%bind () = flush t in
      t.closed <- true;
      Chc.Protocol.send_data_end t.conn.p;
      let%bind () = flush_out t.conn in
      let%map () = finish t.conn in
      t.conn.mode <- Idle)
  ;;

  let pending_rows t = t.n
  let written_rows t = t.written
end
