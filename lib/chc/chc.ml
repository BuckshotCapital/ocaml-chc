module Error = struct
  type t =
    { code : int
    ; server_code : int
    ; msg : string
    ; server_name : string
    }

  let to_string t =
    if t.server_code <> 0
    then Printf.sprintf "%s (server code %d%s)" t.msg t.server_code (if t.server_name = "" then "" else ": " ^ t.server_name)
    else t.msg
  ;;
end

exception Error of Error.t

(* The C side raises this by name; registration must happen before any stub
   runs, which module initialisation guarantees. *)
let () = Callback.register_exception "chc.error" (Error { code = 0; server_code = 0; msg = ""; server_name = "" })

let () =
  Printexc.register_printer (function
    | Error e -> Some ("Chc.Error: " ^ Error.to_string e)
    | _ -> None)
;;

module Kind = struct
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

  (* Index = the chc_kind ordinal. A _Static_assert in chc_stubs.c pins
     CHC_KIND_COUNT to the length of this table, so a vendored header bump that
     inserts a kind fails the build instead of silently shifting every kind
     after the insertion point. *)
  let table =
    [| Void
     ; Int8
     ; Int16
     ; Int32
     ; Int64
     ; Int128
     ; Int256
     ; UInt8
     ; UInt16
     ; UInt32
     ; UInt64
     ; UInt128
     ; UInt256
     ; Float32
     ; Float64
     ; BFloat16
     ; Bool
     ; Date
     ; Date32
     ; DateTime
     ; DateTime64
     ; Time
     ; Time64
     ; String
     ; FixedString
     ; Decimal32
     ; Decimal64
     ; Decimal128
     ; Decimal256
     ; UUID
     ; IPv4
     ; IPv6
     ; Enum8
     ; Enum16
     ; Nullable
     ; Array
     ; Tuple
     ; Map
     ; Nested
     ; LowCardinality
     ; Interval
     ; Point
     ; Ring
     ; LineString
     ; Polygon
     ; MultiPolygon
     ; MultiLineString
     ; Variant
     ; Dynamic
     ; JSON
     ; Object
     ; AggregateFunction
     ; SimpleAggregateFunction
     ; QBit
     ; Nothing
    |]
  ;;

  let () = assert (Array.length table = 55)
  let of_int i = if i < 0 || i >= Array.length table then invalid_arg (Printf.sprintf "Chc.Kind.of_int: %d out of range" i) else table.(i)

  let to_string = function
    | Void -> "Void"
    | Int8 -> "Int8"
    | Int16 -> "Int16"
    | Int32 -> "Int32"
    | Int64 -> "Int64"
    | Int128 -> "Int128"
    | Int256 -> "Int256"
    | UInt8 -> "UInt8"
    | UInt16 -> "UInt16"
    | UInt32 -> "UInt32"
    | UInt64 -> "UInt64"
    | UInt128 -> "UInt128"
    | UInt256 -> "UInt256"
    | Float32 -> "Float32"
    | Float64 -> "Float64"
    | BFloat16 -> "BFloat16"
    | Bool -> "Bool"
    | Date -> "Date"
    | Date32 -> "Date32"
    | DateTime -> "DateTime"
    | DateTime64 -> "DateTime64"
    | Time -> "Time"
    | Time64 -> "Time64"
    | String -> "String"
    | FixedString -> "FixedString"
    | Decimal32 -> "Decimal32"
    | Decimal64 -> "Decimal64"
    | Decimal128 -> "Decimal128"
    | Decimal256 -> "Decimal256"
    | UUID -> "UUID"
    | IPv4 -> "IPv4"
    | IPv6 -> "IPv6"
    | Enum8 -> "Enum8"
    | Enum16 -> "Enum16"
    | Nullable -> "Nullable"
    | Array -> "Array"
    | Tuple -> "Tuple"
    | Map -> "Map"
    | Nested -> "Nested"
    | LowCardinality -> "LowCardinality"
    | Interval -> "Interval"
    | Point -> "Point"
    | Ring -> "Ring"
    | LineString -> "LineString"
    | Polygon -> "Polygon"
    | MultiPolygon -> "MultiPolygon"
    | MultiLineString -> "MultiLineString"
    | Variant -> "Variant"
    | Dynamic -> "Dynamic"
    | JSON -> "JSON"
    | Object -> "Object"
    | AggregateFunction -> "AggregateFunction"
    | SimpleAggregateFunction -> "SimpleAggregateFunction"
    | QBit -> "QBit"
    | Nothing -> "Nothing"
  ;;
end

module Layout = struct
  type t =
    | Fixed
    | String
    | Nullable
    | Array
    | Tuple
    | Low_cardinality
    | Nothing

  (* chc_col_kind starts at 1, not 0. *)
  let of_int = function
    | 1 -> Fixed
    | 2 -> String
    | 3 -> Nullable
    | 4 -> Array
    | 5 -> Tuple
    | 6 -> Low_cardinality
    | 7 -> Nothing
    | i -> invalid_arg (Printf.sprintf "Chc.Layout.of_int: %d out of range" i)
  ;;
end

type value =
  | Null
  | Bool of bool
  | Int of int64
  | Uint of int64
  | Float of float
  | Str of string
  | Raw of string
  | Arr of value array
  | Tup of value array

let hex s = String.concat "" (List.map (Printf.sprintf "%02x") (List.of_seq (Seq.map Char.code (String.to_seq s))))

let rec string_of_value = function
  | Null -> "NULL"
  | Bool b -> string_of_bool b
  | Int i -> Int64.to_string i
  | Uint u -> Printf.sprintf "%Lu" u
  | Float f -> string_of_float f
  | Str s -> s
  | Raw s -> "0x" ^ hex s
  | Arr a -> "[" ^ String.concat ", " (Array.to_list (Array.map string_of_value a)) ^ "]"
  | Tup a -> "(" ^ String.concat ", " (Array.to_list (Array.map string_of_value a)) ^ ")"
;;

(* -------------------------------------------------------------------------- *)
(* Stubs                                                                      *)
(* -------------------------------------------------------------------------- *)

type block_handle
type reader_handle

(* Unix.file_descr is an int at runtime on Unix, so the stub reads it with
   Int_val — same convention the stdlib's own unix stubs use. *)
external reader_open : Unix.file_descr -> bool -> bool -> int -> reader_handle = "chc_stub_reader_open"
external reader_close : reader_handle -> unit = "chc_stub_reader_close"
external read_block_raw : reader_handle -> block_handle option = "chc_stub_read_block"
external blk_n_rows : block_handle -> int = "chc_stub_block_n_rows"
external blk_n_columns : block_handle -> int = "chc_stub_block_n_columns"
external blk_column_name : block_handle -> int -> string = "chc_stub_block_column_name"
external blk_column_ptr : block_handle -> int -> nativeint = "chc_stub_block_column_ptr"
external blk_type_ptr : block_handle -> int -> nativeint = "chc_stub_block_type_ptr"

(* Every column/type stub takes the block as its first argument purely to keep
   it reachable by the GC — the pointers below are interior to it. *)
external ty_kind : block_handle -> nativeint -> int = "chc_stub_type_kind"
external ty_child : block_handle -> nativeint -> int -> nativeint = "chc_stub_type_child"
external ty_format : block_handle -> nativeint -> string = "chc_stub_type_format"
external col_layout : block_handle -> nativeint -> int = "chc_stub_col_layout"
external col_n_rows : block_handle -> nativeint -> int = "chc_stub_col_n_rows"
external col_validate : block_handle -> nativeint -> unit = "chc_stub_col_validate"
external col_fixed : block_handle -> nativeint -> string * int = "chc_stub_col_fixed"
external col_strings : block_handle -> nativeint -> string array = "chc_stub_col_strings"
external col_null_map : block_handle -> nativeint -> string = "chc_stub_col_null_map"
external col_nullable_inner : block_handle -> nativeint -> nativeint = "chc_stub_col_nullable_inner"
external col_array_offsets : block_handle -> nativeint -> int array = "chc_stub_col_array_offsets"
external col_array_values : block_handle -> nativeint -> nativeint = "chc_stub_col_array_values"
external col_tuple_arity : block_handle -> nativeint -> int = "chc_stub_col_tuple_arity"
external col_tuple_child : block_handle -> nativeint -> int -> nativeint = "chc_stub_col_tuple_child"
external col_lc_keys : block_handle -> nativeint -> int array = "chc_stub_col_lc_keys"
external col_lc_dict : block_handle -> nativeint -> nativeint = "chc_stub_col_lc_dict"

(* -------------------------------------------------------------------------- *)
(* Fixed-width decoding                                                       *)
(* -------------------------------------------------------------------------- *)

let u32_mask = 0xFFFFFFFFL

let decode_fixed (kind : Kind.t) (data : string) (elem : int) (n : int) : value array =
  let off i = i * elem in
  let slice i = String.sub data (off i) elem in
  let i64 i = String.get_int64_le data (off i) in
  let i32 i = Int64.of_int32 (String.get_int32_le data (off i)) in
  let u32 i = Int64.logand (i32 i) u32_mask in
  match kind with
  | Kind.Int8 -> Array.init n (fun i -> Int (Int64.of_int (String.get_int8 data (off i))))
  | Kind.Int16 -> Array.init n (fun i -> Int (Int64.of_int (String.get_int16_le data (off i))))
  | Kind.Int32 -> Array.init n (fun i -> Int (i32 i))
  | Kind.Int64 -> Array.init n (fun i -> Int (i64 i))
  | Kind.UInt8 -> Array.init n (fun i -> Uint (Int64.of_int (String.get_uint8 data (off i))))
  | Kind.UInt16 -> Array.init n (fun i -> Uint (Int64.of_int (String.get_uint16_le data (off i))))
  | Kind.UInt32 -> Array.init n (fun i -> Uint (u32 i))
  | Kind.UInt64 -> Array.init n (fun i -> Uint (i64 i))
  | Kind.Bool -> Array.init n (fun i -> Bool (String.get_uint8 data (off i) <> 0))
  | Kind.Float32 -> Array.init n (fun i -> Float (Int32.float_of_bits (String.get_int32_le data (off i))))
  | Kind.Float64 -> Array.init n (fun i -> Float (Int64.float_of_bits (i64 i)))
  (* Days since 1970-01-01, then seconds, then scaled counts. Left as raw
     integers: calendar conversion is a caller policy, not a wire concern. *)
  | Kind.Date -> Array.init n (fun i -> Uint (Int64.of_int (String.get_uint16_le data (off i))))
  | Kind.Date32 -> Array.init n (fun i -> Int (i32 i))
  | Kind.DateTime -> Array.init n (fun i -> Uint (u32 i))
  | Kind.DateTime64 -> Array.init n (fun i -> Int (i64 i))
  | Kind.Time -> Array.init n (fun i -> Int (i32 i))
  | Kind.Time64 -> Array.init n (fun i -> Int (i64 i))
  | Kind.Enum8 -> Array.init n (fun i -> Int (Int64.of_int (String.get_int8 data (off i))))
  | Kind.Enum16 -> Array.init n (fun i -> Int (Int64.of_int (String.get_int16_le data (off i))))
  | Kind.IPv4 -> Array.init n (fun i -> Uint (u32 i))
  | Kind.Decimal32 -> Array.init n (fun i -> Int (i32 i))
  | Kind.Decimal64 -> Array.init n (fun i -> Int (i64 i))
  | Kind.FixedString -> Array.init n (fun i -> Str (slice i))
  (* 128/256-bit integers and decimals, UUID, IPv6, BFloat16: no lossless OCaml
     representation, so hand back the wire bytes. *)
  | _ -> Array.init n (fun i -> Raw (slice i))
;;

(* -------------------------------------------------------------------------- *)
(* Type descent                                                               *)
(* -------------------------------------------------------------------------- *)

(* The column tree and the type tree are not isomorphic: a Map column decodes
   as Array(Tuple(K, V)) but its type node is Map(K, V) with two children and
   no intermediate tuple. Tanon bridges that gap. *)
type tynode =
  | Tnode of nativeint
  | Tanon of tynode list

let kind_of blk = function
  | Tnode t -> Kind.of_int (ty_kind blk t)
  | Tanon _ -> Kind.Tuple
;;

let nullable_inner_ty blk ty =
  match ty with
  | Tnode t when Kind.of_int (ty_kind blk t) = Kind.Nullable -> Tnode (ty_child blk t 0)
  | _ -> ty
;;

let lc_dict_ty blk ty =
  match ty with
  | Tnode t when Kind.of_int (ty_kind blk t) = Kind.LowCardinality -> Tnode (ty_child blk t 0)
  | _ -> ty
;;

let array_values_ty blk ty =
  match ty with
  | Tnode t ->
    (match Kind.of_int (ty_kind blk t) with
     | Kind.Map -> Tanon [ Tnode (ty_child blk t 0); Tnode (ty_child blk t 1) ]
     | _ -> Tnode (ty_child blk t 0))
  | Tanon _ -> ty
;;

let tuple_child_ty blk ty i =
  match ty with
  | Tnode t -> Tnode (ty_child blk t i)
  | Tanon l -> List.nth l i
;;

(* -------------------------------------------------------------------------- *)
(* Column decoding                                                            *)
(* -------------------------------------------------------------------------- *)

let rec decode_col blk ty col : value array =
  let n = col_n_rows blk col in
  match Layout.of_int (col_layout blk col) with
  | Layout.Nothing -> Array.make n Null
  | Layout.Fixed ->
    let data, elem = col_fixed blk col in
    decode_fixed (kind_of blk ty) data elem n
  | Layout.String -> Array.map (fun s -> Str s) (col_strings blk col)
  | Layout.Nullable ->
    let nulls = col_null_map blk col in
    let inner = decode_col blk (nullable_inner_ty blk ty) (col_nullable_inner blk col) in
    Array.init n (fun i -> if String.get_uint8 nulls i <> 0 then Null else inner.(i))
  | Layout.Array ->
    let offs = col_array_offsets blk col in
    let vals = decode_col blk (array_values_ty blk ty) (col_array_values blk col) in
    (* Offsets are cumulative exclusive ends, so slicing is stateful —
         Array.init leaves evaluation order unspecified, hence the explicit
         loop. *)
    let out = Array.make n Null in
    let prev = ref 0 in
    for i = 0 to n - 1 do
      let e = offs.(i) in
      out.(i) <- Arr (Array.sub vals !prev (e - !prev));
      prev := e
    done;
    out
  | Layout.Tuple ->
    let arity = col_tuple_arity blk col in
    let children = Array.init arity (fun c -> decode_col blk (tuple_child_ty blk ty c) (col_tuple_child blk col c)) in
    Array.init n (fun r -> Tup (Array.init arity (fun c -> children.(c).(r))))
  | Layout.Low_cardinality ->
    let keys = col_lc_keys blk col in
    let dict = decode_col blk (lc_dict_ty blk ty) (col_lc_dict blk col) in
    Array.init n (fun i -> dict.(keys.(i)))
;;

(* -------------------------------------------------------------------------- *)
(* Public block API                                                           *)
(* -------------------------------------------------------------------------- *)

type block = block_handle

let n_rows = blk_n_rows
let n_columns = blk_n_columns
let column_name = blk_column_name
let column_type_name b i = ty_format b (blk_type_ptr b i)
let column_kind b i = Kind.of_int (ty_kind b (blk_type_ptr b i))

(* The TCP path opens a query response with a schema-only block: column names
   and types are present, n_rows is 0, and clickhouse-c allocates no column
   tree at all, so chc_block_column hands back NULL. Treat that as an empty
   column rather than letting a null pointer reach the decoder — but only when
   the block really has no rows, since a null tree over a non-empty block would
   be a genuine protocol bug worth surfacing. *)
let is_schema_only b i = blk_column_ptr b i = 0n

let column_layout b i =
  if is_schema_only b i
  then invalid_arg (Printf.sprintf "Chc.column_layout: column %d is schema-only (block has no rows)" i)
  else Layout.of_int (col_layout b (blk_column_ptr b i))
;;

let column b i =
  let ptr = blk_column_ptr b i in
  if ptr <> 0n
  then decode_col b (Tnode (blk_type_ptr b i)) ptr
  else if blk_n_rows b = 0
  then [||]
  else invalid_arg (Printf.sprintf "Chc.column: column %d has no data tree but the block has %d rows" i (blk_n_rows b))
;;

let columns b = Array.init (n_columns b) (column b)

let rows b =
  let cols = columns b in
  let nc = Array.length cols in
  Array.init (n_rows b) (fun r -> Array.init nc (fun c -> cols.(c).(r)))
;;

(* -------------------------------------------------------------------------- *)
(* Public reader API                                                          *)
(* -------------------------------------------------------------------------- *)

type reader =
  { h : reader_handle
  ; validate : bool
  ; mutable closed : bool
  }

let open_fd ?(has_block_info = false) ?(has_custom_serialization = false) ?(read_buffer_bytes = 0) ?(validate = true) fd =
  let h = reader_open fd has_block_info has_custom_serialization read_buffer_bytes in
  { h; validate; closed = false }
;;

let read_block r =
  if r.closed then invalid_arg "Chc.read_block: reader is closed";
  match read_block_raw r.h with
  | None -> None
  | Some b ->
    if r.validate
    then
      for i = 0 to blk_n_columns b - 1 do
        col_validate b (blk_column_ptr b i)
      done;
    Some b
;;

let close r =
  if not r.closed
  then (
    r.closed <- true;
    reader_close r.h)
;;

let iter r f =
  let rec go () =
    match read_block r with
    | None -> ()
    | Some b ->
      f b;
      go ()
  in
  go ()
;;

let fold r ~init ~f =
  let rec go acc =
    match read_block r with
    | None -> acc
    | Some b -> go (f acc b)
  in
  go init
;;

(* -------------------------------------------------------------------------- *)
(* Async client — sans-IO                                                     *)
(* -------------------------------------------------------------------------- *)

module Async = struct
  type handle

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
    ; written_rows : int
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
    ; revision : int
    }

  (* Declaration order is load-bearing: the PKT_CONST_* and PKT_TAG_* enums in
     chc_stubs.c mirror it. OCaml numbers constant and non-constant
     constructors in two independent sequences, so the two groups below are
     each numbered from zero. Do not reorder without updating the stubs. *)
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
    | Progress of progress
    | Profile_info of profile_info

  (* Six arguments, hence the bytecode trampoline alongside the native entry. *)
  external create_raw
    :  string
    -> string
    -> string
    -> string
    -> int
    -> int
    -> handle
    = "chc_stub_async_create_bytecode" "chc_stub_async_create"

  external send_block_raw : handle -> string array -> string array -> value array array -> int -> unit = "chc_stub_async_send_block"
  external close_raw : handle -> unit = "chc_stub_async_close"
  external handshake_raw : handle -> bool = "chc_stub_async_handshake"
  external send_query_raw : handle -> string -> string -> unit = "chc_stub_async_send_query"
  external send_data_end_raw : handle -> unit = "chc_stub_async_send_data_end"
  external submit_raw : handle -> Bytes.t -> int -> unit = "chc_stub_async_submit"
  external pending_out_raw : handle -> string = "chc_stub_async_pending_out"
  external consume_out_raw : handle -> int -> unit = "chc_stub_async_consume_out"
  external server_info_raw : handle -> server_info = "chc_stub_async_server_info"
  external compression_raw : handle -> int = "chc_stub_async_compression"
  external recv_packet_raw : handle -> packet option = "chc_stub_async_recv_packet"

  type t =
    { h : handle
    ; mutable closed : bool
    }

  (* Ordinals match chc_compression. *)
  let int_of_compression = function
    | `None -> 0
    | `Lz4 -> 1
    | `Zstd -> 2
  ;;

  let create
        ?(client_name = "ocaml-chc")
        ?(database = "default")
        ?(user = "default")
        ?(password = "")
        ?(read_buffer_bytes = 0)
        ?(compression = `None)
        ()
    =
    { h = create_raw client_name database user password read_buffer_bytes (int_of_compression compression); closed = false }
  ;;

  let check t = if t.closed then invalid_arg "Chc.Async: client is closed"

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      close_raw t.h)
  ;;

  let handshake t =
    check t;
    handshake_raw t.h
  ;;

  let send_query t ?(query_id = "") sql =
    check t;
    send_query_raw t.h sql query_id
  ;;

  let send_data_end t =
    check t;
    send_data_end_raw t.h
  ;;

  let submit t buf len =
    check t;
    submit_raw t.h buf len
  ;;

  let pending_out t =
    check t;
    pending_out_raw t.h
  ;;

  let consume_out t n =
    check t;
    consume_out_raw t.h n
  ;;

  let server_info t =
    check t;
    server_info_raw t.h
  ;;

  let recv_packet t =
    check t;
    recv_packet_raw t.h
  ;;

  let compression t =
    check t;
    match compression_raw t.h with
    | 0 -> `None
    | 1 -> `Lz4
    | 2 -> `Zstd
    | n -> invalid_arg (Printf.sprintf "Chc.Async.compression: unknown value %d" n)
  ;;

  let send_block t ~names ~types ~columns ~n_rows =
    check t;
    send_block_raw t.h names types columns n_rows
  ;;
end

(* -------------------------------------------------------------------------- *)
(* Client — blocking driver over Unix sockets                                 *)
(* -------------------------------------------------------------------------- *)

module Client = struct
  type t =
    { sock : Unix.file_descr
    ; a : Async.t
    ; buf : Bytes.t
    ; mutable closed : bool
    ; (* Set when an INSERT fails and the stream could not be wound back to a
         clean point. The server is then mid-statement and would reject the
         next Query packet, so refuse up front with a comprehensible error
         rather than surfacing a protocol violation later. *)
      mutable poisoned : bool
    }

  let server_error where (e : Async.exn_info) =
    raise (Error { code = 0; server_code = e.code; msg = (if e.display_text = "" then where else e.display_text); server_name = e.name })
  ;;

  let eof_error () = raise (Error { code = 1 (* CHC_ERR_IO *); server_code = 0; msg = "connection closed by peer"; server_name = "" })

  (* One turn of the reactor: flush everything the client wants to send before
     reading, otherwise a query sitting in the out buffer deadlocks against a
     server that is waiting for it. *)
  let pump t =
    let out = Async.pending_out t.a in
    let len = String.length out in
    if len > 0
    then (
      let n = Unix.write_substring t.sock out 0 len in
      Async.consume_out t.a n)
    else (
      let n = Unix.read t.sock t.buf 0 (Bytes.length t.buf) in
      if n = 0 then eof_error ();
      Async.submit t.a t.buf n)
  ;;

  let resolve host port =
    match Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] with
    | [] -> invalid_arg (Printf.sprintf "Chc.Client.connect: cannot resolve %s:%d" host port)
    | ai :: _ -> ai
  ;;

  let connect
        ?(port = 9000)
        ?(database = "default")
        ?(user = "default")
        ?(password = "")
        ?(client_name = "ocaml-chc")
        ?(read_buffer_bytes = 0)
        ?(socket_buffer_bytes = 65536)
        ?(compression = `None)
        host
    =
    let ai = resolve host port in
    let sock = Unix.socket ai.Unix.ai_family ai.Unix.ai_socktype ai.Unix.ai_protocol in
    (try Unix.connect sock ai.Unix.ai_addr with
     | e ->
       Unix.close sock;
       raise e);
    Unix.setsockopt sock Unix.TCP_NODELAY true;
    let a = Async.create ~client_name ~database ~user ~password ~read_buffer_bytes ~compression () in
    let t = { sock; a; buf = Bytes.create socket_buffer_bytes; closed = false; poisoned = false } in
    let rec drive () =
      if Async.handshake t.a
      then ()
      else (
        pump t;
        drive ())
    in
    (try drive () with
     | e ->
       Async.close a;
       Unix.close sock;
       raise e);
    t
  ;;

  let check t =
    if t.closed then invalid_arg "Chc.Client: connection is closed";
    if t.poisoned then invalid_arg "Chc.Client: connection was left mid-INSERT by an earlier failure; reconnect"
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      Async.close t.a;
      Unix.close t.sock)
  ;;

  let server_info t =
    check t;
    Async.server_info t.a
  ;;

  let compression t =
    check t;
    Async.compression t.a
  ;;

  (* Drains the response stream to End_of_stream, handing every Data block that
     carries columns to [f]. The first such block normally has zero rows and
     exists to carry the schema, so it is passed through rather than hidden. *)
  let query_iter t sql ~f =
    check t;
    Async.send_query t.a sql;
    let rec loop () =
      match Async.recv_packet t.a with
      | None ->
        pump t;
        loop ()
      | Some Async.End_of_stream -> ()
      | Some (Async.Data b) ->
        if n_columns b > 0 then f b;
        loop ()
      | Some (Async.Exception e) -> server_error "query failed" e
      | Some _ -> loop ()
    in
    loop ()
  ;;

  let query_fold t sql ~init ~f =
    let acc = ref init in
    query_iter t sql ~f:(fun b -> acc := f !acc b);
    !acc
  ;;

  let query t sql = List.rev (query_fold t sql ~init:[] ~f:(fun acc b -> b :: acc))

  (* Every row of every block, decoded, with the column names from the header. *)
  let query_rows t sql =
    let names = ref [||] in
    let out =
      query_fold t sql ~init:[] ~f:(fun acc b ->
        if Array.length !names = 0 then names := Array.init (n_columns b) (column_name b);
        if n_rows b = 0 then acc else List.rev_append (Array.to_list (rows b)) acc)
    in
    !names, Array.of_list (List.rev out)
  ;;

  let ping t =
    check t;
    ignore (query t "SELECT 1")
  ;;

  (* INSERT drives the protocol in the other direction: the server answers the
     query with a schema-only block describing the target columns, and we reply
     with Data blocks shaped to match. Taking the types from that reply rather
     than asking the caller for them means the wire types are always the
     server's own. *)
  let insert ?columns ?(batch_size = 65536) t table rows =
    check t;
    let collist =
      match columns with
      | None -> ""
      | Some cs -> " (" ^ String.concat ", " cs ^ ")"
    in
    Async.send_query t.a (Printf.sprintf "INSERT INTO %s%s VALUES" table collist);
    let rec await_schema () =
      match Async.recv_packet t.a with
      | None ->
        pump t;
        await_schema ()
      | Some (Async.Data b) -> b
      | Some (Async.Exception e) -> server_error "insert failed" e
      | Some Async.End_of_stream -> invalid_arg "Chc.Client.insert: server closed the stream before sending a schema"
      | Some _ -> await_schema ()
    in
    let schema = await_schema () in
    let n_cols = n_columns schema in
    let names = Array.init n_cols (column_name schema) in
    let types = Array.init n_cols (column_type_name schema) in
    let total = Array.length rows in
    Array.iteri
      (fun r row ->
         if Array.length row <> n_cols
         then invalid_arg (Printf.sprintf "Chc.Client.insert: row %d has %d values, expected %d" r (Array.length row) n_cols))
      rows;
    (* Transpose into columns a batch at a time: the wire format is columnar,
       and one giant block would hold the whole input in memory twice. *)
    let rec send_batch start =
      if start < total
      then (
        let n = min batch_size (total - start) in
        let columns = Array.init n_cols (fun c -> Array.init n (fun r -> rows.(start + r).(c))) in
        Async.send_block t.a ~names ~types ~columns ~n_rows:n;
        (* Flush so the out buffer does not grow without bound — sends never
           block, so nothing else applies backpressure. *)
        while String.length (Async.pending_out t.a) > 0 do
          pump t
        done;
        send_batch (start + n))
    in
    let rec finish () =
      match Async.recv_packet t.a with
      | None ->
        pump t;
        finish ()
      | Some Async.End_of_stream -> ()
      | Some (Async.Exception e) -> server_error "insert failed" e
      | Some _ -> finish ()
    in
    let sent_end = ref false in
    (try
       send_batch 0;
       Async.send_data_end t.a;
       sent_end := true
     with
     | e ->
       (* The server is mid-INSERT waiting for Data. Terminate the stream so the
          connection stays usable; blocks it already accepted stay committed,
          which is inherent to a streaming insert. If even that fails there is
          no way back, so poison the connection. *)
       (try
          if not !sent_end then Async.send_data_end t.a;
          finish ()
        with
        | _ -> t.poisoned <- true);
       raise e);
    finish ()
  ;;

  let execute t sql = query_iter t sql ~f:(fun _ -> ())
end
