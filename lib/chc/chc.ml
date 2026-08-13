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
  | Big of string
  | Decimal of string
  | Uuid of string
  | Ip of string
  | Arr of value array
  | Tup of value array

let hex s = String.concat "" (List.map (Printf.sprintf "%02x") (List.of_seq (Seq.map Char.code (String.to_seq s))))

(* -------------------------------------------------------------------------- *)
(* Wide fixed-width types                                                     *)
(* -------------------------------------------------------------------------- *)

(* Int128/256 and Decimal128/256 exceed every OCaml integer type, so they are
   rendered to exact decimal text rather than truncated. Long division by 10
   over the base-256 digits: 32 bytes is at most 78 iterations, which is
   nothing next to the I/O that produced the value. *)
let bignum_to_string ~signed (le : string) =
  let n = String.length le in
  if n = 0
  then "0"
  else (
    let b = Bytes.create n in
    (* to big-endian, which is the natural order for long division *)
    for i = 0 to n - 1 do
      Bytes.set b i le.[n - 1 - i]
    done;
    let negative = signed && Char.code (Bytes.get b 0) land 0x80 <> 0 in
    if negative
    then (
      for i = 0 to n - 1 do
        Bytes.set b i (Char.unsafe_chr (lnot (Char.code (Bytes.get b i)) land 0xff))
      done;
      let rec carry i =
        if i >= 0
        then (
          let v = Char.code (Bytes.get b i) + 1 in
          Bytes.set b i (Char.unsafe_chr (v land 0xff));
          if v > 0xff then carry (i - 1))
      in
      carry (n - 1));
    let digits = Buffer.create 48 in
    let rec divide () =
      let rem = ref 0 in
      let quotient_nonzero = ref false in
      for i = 0 to n - 1 do
        let cur = (!rem lsl 8) lor Char.code (Bytes.get b i) in
        let q = cur / 10 in
        rem := cur mod 10;
        Bytes.set b i (Char.unsafe_chr q);
        if q <> 0 then quotient_nonzero := true
      done;
      Buffer.add_char digits (Char.unsafe_chr (Char.code '0' + !rem));
      if !quotient_nonzero then divide ()
    in
    divide ();
    let d = Buffer.contents digits in
    let len = String.length d in
    let out = Bytes.create (len + if negative then 1 else 0) in
    if negative then Bytes.set out 0 '-';
    let off = if negative then 1 else 0 in
    for i = 0 to len - 1 do
      Bytes.set out (off + i) d.[len - 1 - i]
    done;
    Bytes.unsafe_to_string out)
;;

(* Decimals are a scaled integer mantissa; the scale lives in the type, not the
   value, so it has to be threaded in from the column. *)
let decimal_to_string ~scale le =
  let d = bignum_to_string ~signed:true le in
  if scale <= 0
  then d
  else (
    let negative = String.length d > 0 && d.[0] = '-' in
    let digits = if negative then String.sub d 1 (String.length d - 1) else d in
    let digits = if String.length digits <= scale then String.make (scale + 1 - String.length digits) '0' ^ digits else digits in
    let cut = String.length digits - scale in
    let int_part = String.sub digits 0 cut in
    (* ClickHouse renders a decimal in its shortest exact form — trailing zeros
       in the fraction go, and the point goes with them if nothing is left.
       Verified against toString and against TSV column output, which agree. *)
    let frac = String.sub digits cut scale in
    let last = ref (String.length frac) in
    while !last > 0 && frac.[!last - 1] = '0' do
      decr last
    done;
    let frac = String.sub frac 0 !last in
    let body = if frac = "" then int_part else int_part ^ "." ^ frac in
    if negative && body <> "0" then "-" ^ body else body)
;;

(* ClickHouse stores a UUID as two UInt64s, each little-endian on the wire:
   bytes 0..7 are the high half, 8..15 the low half. *)
let uuid_to_string le =
  if String.length le <> 16
  then "0x" ^ hex le
  else (
    let hi = String.get_int64_le le 0
    and lo = String.get_int64_le le 8 in
    Printf.sprintf
      "%08Lx-%04Lx-%04Lx-%04Lx-%012Lx"
      (Int64.shift_right_logical hi 32)
      (Int64.logand (Int64.shift_right_logical hi 16) 0xFFFFL)
      (Int64.logand hi 0xFFFFL)
      (Int64.shift_right_logical lo 48)
      (Int64.logand lo 0xFFFFFFFFFFFFL))
;;

(* RFC 5952: lowercase, no leading zeros, longest run of zero groups collapsed
   to "::", and IPv4-mapped addresses rendered in dotted-quad tail form — which
   is what ClickHouse's own toString does. *)

(* --- text back to wire bytes, for the write path --- *)

(* Parse a decimal integer into [width] little-endian bytes, two's complement.
   Overflow is an error rather than a silent wrap: a column that cannot hold the
   value should say so. *)
let bignum_of_string ~width (s : string) =
  let negative = String.length s > 0 && s.[0] = '-' in
  let start = if negative || (String.length s > 0 && s.[0] = '+') then 1 else 0 in
  let b = Bytes.make width '\000' in
  for i = start to String.length s - 1 do
    let c = s.[i] in
    if c < '0' || c > '9' then invalid_arg (Printf.sprintf "Chc: %S is not an integer" s);
    let carry = ref (Char.code c - Char.code '0') in
    for k = width - 1 downto 0 do
      let v = (Char.code (Bytes.get b k) * 10) + !carry in
      Bytes.set b k (Char.unsafe_chr (v land 0xff));
      carry := v lsr 8
    done;
    if !carry <> 0 then invalid_arg (Printf.sprintf "Chc: %S overflows a %d-byte column" s width)
  done;
  if negative
  then (
    for i = 0 to width - 1 do
      Bytes.set b i (Char.unsafe_chr (lnot (Char.code (Bytes.get b i)) land 0xff))
    done;
    let rec carry i =
      if i >= 0
      then (
        let v = Char.code (Bytes.get b i) + 1 in
        Bytes.set b i (Char.unsafe_chr (v land 0xff));
        if v > 0xff then carry (i - 1))
    in
    carry (width - 1));
  let le = Bytes.create width in
  for i = 0 to width - 1 do
    Bytes.set le i (Bytes.get b (width - 1 - i))
  done;
  Bytes.unsafe_to_string le
;;

(* Drop the point and pad or truncate the fraction to the column's scale, which
   turns the text back into the integer mantissa the wire carries. *)
let mantissa_of_decimal ~scale s =
  let negative = String.length s > 0 && s.[0] = '-' in
  let body = if negative || (String.length s > 0 && s.[0] = '+') then String.sub s 1 (String.length s - 1) else s in
  let int_part, frac_part =
    match String.index_opt body '.' with
    | None -> body, ""
    | Some i -> String.sub body 0 i, String.sub body (i + 1) (String.length body - i - 1)
  in
  let int_part = if int_part = "" then "0" else int_part in
  let frac =
    if String.length frac_part >= scale then String.sub frac_part 0 scale else frac_part ^ String.make (scale - String.length frac_part) '0'
  in
  (if negative then "-" else "") ^ int_part ^ frac
;;

let uuid_of_string s =
  let h = String.concat "" (String.split_on_char '-' s) in
  if String.length h <> 32 then invalid_arg (Printf.sprintf "Chc: %S is not a UUID" s);
  let b = Bytes.create 16 in
  Bytes.set_int64_le b 0 (Int64.of_string ("0x" ^ String.sub h 0 16));
  Bytes.set_int64_le b 8 (Int64.of_string ("0x" ^ String.sub h 16 16));
  Bytes.unsafe_to_string b
;;

(* Accepts the forms ClickHouse emits: full groups, a single "::" run, and a
   dotted-quad tail for IPv4-mapped addresses. *)

(* BFloat16 is the top half of a Float32. *)
let bfloat16_to_float u16 = Int32.float_of_bits (Int32.shift_left (Int32.of_int u16) 16)

(* Which IP implementation dune selected: "ipaddr" or "builtin". Behaviour is
   identical either way — this exists so a deployment can confirm what it got. *)
let ip_backend = Chc_ip.backend

let rec string_of_value = function
  | Null -> "NULL"
  | Bool b -> string_of_bool b
  | Int i -> Int64.to_string i
  | Uint u -> Printf.sprintf "%Lu" u
  | Float f -> string_of_float f
  | Str s -> s
  | Raw s -> "0x" ^ hex s
  | Big s -> s
  | Decimal s -> s
  | Uuid s -> s
  | Ip s -> s
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
external ty_decimal_scale : block_handle -> nativeint -> int = "chc_stub_type_decimal_scale"
external type_info_raw : string -> int * int * int = "chc_stub_type_info"
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

(* [scale] comes from the column's type rather than the value, so decimals need
   it passed in; it is ignored for every other kind. *)
let decode_fixed (kind : Kind.t) ~scale (data : string) (elem : int) (n : int) : value array =
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
  | Kind.BFloat16 -> Array.init n (fun i -> Float (bfloat16_to_float (String.get_uint16_le data (off i))))
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
  | Kind.FixedString -> Array.init n (fun i -> Str (slice i))
  (* Wider than any OCaml integer: rendered to exact decimal text. *)
  | Kind.Int128 | Kind.Int256 -> Array.init n (fun i -> Big (bignum_to_string ~signed:true (slice i)))
  | Kind.UInt128 | Kind.UInt256 -> Array.init n (fun i -> Big (bignum_to_string ~signed:false (slice i)))
  | Kind.Decimal32 | Kind.Decimal64 | Kind.Decimal128 | Kind.Decimal256 ->
    Array.init n (fun i -> Decimal (decimal_to_string ~scale (slice i)))
  | Kind.UUID -> Array.init n (fun i -> Uuid (uuid_to_string (slice i)))
  | Kind.IPv4 -> Array.init n (fun i -> Ip (Chc_ip.v4_to_string (u32 i)))
  | Kind.IPv6 -> Array.init n (fun i -> if elem = 16 then Ip (Chc_ip.v6_to_string (slice i)) else Raw (slice i))
  (* Anything still unmodelled keeps its wire bytes rather than being guessed at. *)
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
    let scale =
      match ty with
      | Tnode t -> ty_decimal_scale blk t
      | Tanon _ -> 0
    in
    decode_fixed (kind_of blk ty) ~scale data elem n
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
(* Typed row decoding                                                         *)
(* -------------------------------------------------------------------------- *)

module Row = struct
  exception Decode_error of string

  let fail fmt = Printf.ksprintf (fun m -> raise (Decode_error m)) fmt

  (* A cell converter: what it wants, and how to read it. Errors carry
     [expected] rather than a bare "no" so a mismatch names both sides. *)
  type 'a conv =
    { expected : string
    ; read : value -> 'a option
    }

  let conv expected read = { expected; read }
  let map_conv f c = { c with read = (fun v -> Option.map f (c.read v)) }

  let string =
    conv "String" (function
      | Str s -> Some s
      | _ -> None)
  ;;

  (* Anything at all, via its canonical rendering. Useful for columns whose type
     you do not want to commit to. *)
  let text = conv "any value" (fun v -> Some (string_of_value v))

  let int64 =
    conv "an integer" (function
      | Int i | Uint i -> Some i
      | _ -> None)
  ;;

  let int = map_conv Int64.to_int int64

  (* Integers are promoted, since aggregates flip between UInt64 and Float64
     depending on the function used. *)
  let float =
    conv "a number" (function
      | Float f -> Some f
      | Int i -> Some (Int64.to_float i)
      | Uint u -> Some (Int64.to_float u)
      | _ -> None)
  ;;

  (* ClickHouse has a real Bool, but UInt8 0/1 is still the common idiom. *)
  let bool =
    conv "Bool" (function
      | Bool b -> Some b
      | Uint 0L | Int 0L -> Some false
      | Uint 1L | Int 1L -> Some true
      | _ -> None)
  ;;

  let uuid =
    conv "UUID" (function
      | Uuid u -> Some u
      | _ -> None)
  ;;

  let ip =
    conv "an IP address" (function
      | Ip a -> Some a
      | _ -> None)
  ;;

  let big =
    conv "a wide integer" (function
      | Big b -> Some b
      | _ -> None)
  ;;

  let decimal =
    conv "a Decimal" (function
      | Decimal d -> Some d
      | _ -> None)
  ;;

  let raw =
    conv "raw bytes" (function
      | Raw r -> Some r
      | _ -> None)
  ;;

  let value = conv "any value" (fun v -> Some v)

  let opt c =
    { expected = c.expected ^ " or NULL"
    ; read =
        (function
          | Null -> Some None
          | v -> Option.map Option.some (c.read v))
    }
  ;;

  let array c =
    { expected = "an array of " ^ c.expected
    ; read =
        (function
          | Arr a ->
            let out = Array.make (Array.length a) None in
            let ok = ref true in
            Array.iteri
              (fun i v ->
                 match c.read v with
                 | Some x -> out.(i) <- Some x
                 | None -> ok := false)
              a;
            if !ok then Some (Array.map Option.get out) else None
          | _ -> None)
    }
  ;;

  let pair a b =
    { expected = Printf.sprintf "a tuple of (%s, %s)" a.expected b.expected
    ; read =
        (function
          | Tup [| x; y |] ->
            (match a.read x, b.read y with
             | Some u, Some v -> Some (u, v)
             | _ -> None)
          | _ -> None)
    }
  ;;

  (* Columns are decoded once per block and only if a field actually names
     them, so selecting ten columns and reading two costs two decodes. *)
  type ctx =
    { blk : block
    ; index : (string, int) Hashtbl.t
    ; cache : value array option array
    }

  type 'a t = ctx -> int -> 'a

  let ctx_of_block blk =
    let n = n_columns blk in
    let index = Hashtbl.create (2 * n) in
    for i = n - 1 downto 0 do
      (* downto so the leftmost wins on duplicate names *)
      Hashtbl.replace index (column_name blk i) i
    done;
    { blk; index; cache = Array.make n None }
  ;;

  let column_of ctx i =
    match ctx.cache.(i) with
    | Some c -> c
    | None ->
      let c = column ctx.blk i in
      ctx.cache.(i) <- Some c;
      c
  ;;

  let at i c ctx =
    let n = n_columns ctx.blk in
    if i < 0 || i >= n then fail "Chc.Row: column %d out of range (block has %d)" i n;
    let col = column_of ctx i in
    let name = column_name ctx.blk i in
    fun row ->
      match c.read col.(row) with
      | Some x -> x
      | None ->
        fail
          "Chc.Row: column %S (%s) row %d: expected %s, got %s"
          name
          (column_type_name ctx.blk i)
          row
          c.expected
          (string_of_value col.(row))
  ;;

  let field name c ctx =
    match Hashtbl.find_opt ctx.index name with
    | Some i -> at i c ctx
    | None ->
      let available = String.concat ", " (List.init (n_columns ctx.blk) (column_name ctx.blk)) in
      fail "Chc.Row: no column %S; block has [%s]" name available
  ;;

  let const x _ctx _row = x

  let map f d ctx =
    let g = d ctx in
    fun row -> f (g row)
  ;;

  let both a b ctx =
    let ga = a ctx
    and gb = b ctx in
    fun row -> ga row, gb row
  ;;

  let ( let+ ) d f = map f d
  let ( and+ ) = both
end

let decode_block b d =
  let ctx = Row.ctx_of_block b in
  let get = d ctx in
  Array.init (n_rows b) get
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
(* Query parameters                                                           *)
(* -------------------------------------------------------------------------- *)

(* ClickHouse substitutes {name:Type} placeholders server-side, but the wire
   protocol carries each value as a Field literal that the server parses with
   Field::restoreFromDump — quoting included. clickhouse-c deliberately passes
   the string through untouched, so building that literal correctly is this
   module's whole job. Interpolating a user string into SQL, or into a literal,
   by hand is the thing this exists to avoid. *)
module Param = struct
  (* Every value crosses the wire as a *quoted string literal*, whatever the
     placeholder's declared type: the server casts from text. Measured against
     26.5 — `{v:Int64}` rejects the bare literal `42` with "Couldn't restore
     Field from dump" and accepts `'42'`. (clickhouse-client.h documents the
     opposite, saying to send `42` and `[1,2,3]` unquoted. It does not work.)

     So a value has two renderings: its ClickHouse *text* form, and the escaped
     literal wrapping it. Inside an array, strings must already be quoted, so a
     nested string is escaped once for the array text and again for the literal
     that carries the whole array. Keeping the two apart is the reason this is a
     variant and not a string. *)
  type t =
    | S of string
    | Lit of string (* numbers and bools: identical in both contexts *)
    | A of t list
    | T of t list
    | Null

  (* Escaped content, without the surrounding quotes. *)
  let esc s =
    let b = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
         match c with
         | '\'' -> Buffer.add_string b "\\'"
         | '\\' -> Buffer.add_string b "\\\\"
         | '\n' -> Buffer.add_string b "\\n"
         | '\r' -> Buffer.add_string b "\\r"
         | '\t' -> Buffer.add_string b "\\t"
         | '\000' -> Buffer.add_string b "\\0"
         | c -> Buffer.add_char b c)
      s;
    Buffer.contents b
  ;;

  let quote s = "'" ^ esc s ^ "'"

  (* Inside a container, where a string needs its own quotes. *)
  let rec nested = function
    | S s -> quote s
    | Lit l -> l
    | Null -> "NULL"
    | A xs -> "[" ^ String.concat "," (List.map nested xs) ^ "]"
    | T xs -> "(" ^ String.concat "," (List.map nested xs) ^ ")"
  ;;

  let string s = S s
  let int i = Lit (string_of_int i)
  let int64 i = Lit (Int64.to_string i)
  let float f = Lit (Printf.sprintf "%.17g" f)
  let bool b = Lit (if b then "true" else "false")
  let null = Null
  let array ps = A ps
  let tuple ps = T ps

  let opt f = function
    | None -> Null
    | Some v -> f v
  ;;

  let unsafe_literal s = Lit s

  (* The server unescapes the carried literal twice: once parsing the Field,
     once parsing the value's own text form. Measured against 26.5 — a
     top-level String containing a backslash came back corrupted when escaped
     only once, while the identical bytes inside an Array survived, because the
     array path had already escaped its elements twice. Escaping everything to
     the same depth is what makes the two agree. *)
  let to_literal = function
    | S s -> quote (esc s)
    | other -> quote (nested other)
  ;;
end

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

  (* Mirrors the COL_PLAIN / COL_LC tags in chc_stubs.c. LowCardinality needs a
     dictionary and per-row indices rather than a flat buffer, and building that
     wants a hash table — which OCaml has and C does not. *)
  type encoded_column =
    | Plain of value array
    | Lc of value array * int array

  external send_block_raw : handle -> string array -> string array -> encoded_column array -> int -> unit = "chc_stub_async_send_block"
  external close_raw : handle -> unit = "chc_stub_async_close"
  external handshake_raw : handle -> bool = "chc_stub_async_handshake"
  external send_query_raw : handle -> string -> string -> unit = "chc_stub_async_send_query"
  external send_query_ex_raw : handle -> string -> string -> (string * string) array -> unit = "chc_stub_async_send_query_ex"
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

  let send_query t ?(query_id = "") ?(params = []) sql =
    check t;
    match params with
    | [] -> send_query_raw t.h sql query_id
    | ps -> send_query_ex_raw t.h sql query_id (Array.of_list (List.map (fun (k, v) -> k, Param.to_literal v) ps))
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

  (* The C encoder writes fixed-width cells straight from Int/Uint/Float/Raw.
     The wide types arrive as text, so they are turned into wire bytes here —
     in OCaml, where the parsing is memory-safe and testable, rather than in the
     stub. Columns of any other type are passed through untouched. *)
  let normalize_wide type_name cells =
    let bare =
      let p = "Nullable(" in
      let n = String.length type_name in
      if n > String.length p && String.sub type_name 0 (String.length p) = p && type_name.[n - 1] = ')'
      then String.sub type_name (String.length p) (n - String.length p - 1)
      else type_name
    in
    let kind_ordinal, elem, scale = type_info_raw bare in
    let convert =
      match Kind.of_int kind_ordinal with
      | Kind.UUID ->
        Some
          (function
            | Uuid u -> Raw (uuid_of_string u)
            | Str u -> Raw (uuid_of_string u)
            | v -> v)
      | Kind.IPv4 ->
        Some
          (function
            | Ip a -> Uint (Chc_ip.v4_of_string a)
            | Str a -> Uint (Chc_ip.v4_of_string a)
            | v -> v)
      | Kind.IPv6 ->
        Some
          (function
            | Ip a -> Raw (Chc_ip.v6_of_string a)
            | Str a -> Raw (Chc_ip.v6_of_string a)
            | v -> v)
      | Kind.Int128 | Kind.Int256 | Kind.UInt128 | Kind.UInt256 ->
        Some
          (function
            | Big d -> Raw (bignum_of_string ~width:elem d)
            | Str d -> Raw (bignum_of_string ~width:elem d)
            | Int i -> Raw (bignum_of_string ~width:elem (Int64.to_string i))
            | Uint u -> Raw (bignum_of_string ~width:elem (Printf.sprintf "%Lu" u))
            | v -> v)
      | Kind.Decimal32 | Kind.Decimal64 | Kind.Decimal128 | Kind.Decimal256 ->
        Some
          (function
            | Decimal d -> Raw (bignum_of_string ~width:elem (mantissa_of_decimal ~scale d))
            | Str d -> Raw (bignum_of_string ~width:elem (mantissa_of_decimal ~scale d))
            | v -> v)
      | _ -> None
    in
    match convert with
    | None -> cells
    | Some f -> Array.map (fun v -> if v = Null then Null else f v) cells
  ;;

  (* Dictionary-encode a column. Slot 0 is reserved for the type's default —
     NULL when the inner type is Nullable, otherwise the empty string — which is
     the convention the reader and the server both assume. *)
  let dictionary_encode ~nullable_inner cells =
    let default = if nullable_inner then Null else Str "" in
    let index = Hashtbl.create 64 in
    Hashtbl.replace index default 0;
    let dict = ref [ default ] in
    let next = ref 1 in
    let keys =
      Array.map
        (fun v ->
           match Hashtbl.find_opt index v with
           | Some i -> i
           | None ->
             let i = !next in
             incr next;
             Hashtbl.replace index v i;
             dict := v :: !dict;
             i)
        cells
    in
    Array.of_list (List.rev !dict), keys
  ;;

  let lc_inner type_name =
    let p = "LowCardinality(" in
    let n = String.length type_name in
    if n > String.length p && String.sub type_name 0 (String.length p) = p && type_name.[n - 1] = ')'
    then Some (String.sub type_name (String.length p) (n - String.length p - 1))
    else None
  ;;

  let send_block t ~names ~types ~columns ~n_rows =
    check t;
    let encoded =
      Array.mapi
        (fun c cells ->
           match lc_inner types.(c) with
           | Some inner ->
             let nullable_inner = String.length inner >= 9 && String.sub inner 0 9 = "Nullable(" in
             let dict, keys = dictionary_encode ~nullable_inner (normalize_wide inner cells) in
             Lc (dict, keys)
           | None -> Plain (normalize_wide types.(c) cells))
        columns
    in
    send_block_raw t.h names types encoded n_rows
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
  let query_iter t ?(params = []) sql ~f =
    check t;
    Async.send_query t.a ~params sql;
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

  let query_fold t ?(params = []) sql ~init ~f =
    let acc = ref init in
    query_iter t ~params sql ~f:(fun b -> acc := f !acc b);
    !acc
  ;;

  let query t ?(params = []) sql = List.rev (query_fold t ~params sql ~init:[] ~f:(fun acc b -> b :: acc))

  (* Every row of every block, decoded, with the column names from the header. *)
  let query_rows t ?(params = []) sql =
    let names = ref [||] in
    let out =
      query_fold t ~params sql ~init:[] ~f:(fun acc b ->
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

  let execute t ?(params = []) sql = query_iter t ~params sql ~f:(fun _ -> ())

  (* Schema-only blocks decode to nothing, so they fall out naturally here
     rather than needing a special case at the call site. *)
  let fetch_iter t ?(params = []) sql d ~f = query_iter t ~params sql ~f:(fun b -> Array.iter f (decode_block b d))

  let fetch t ?(params = []) sql d =
    let acc = ref [] in
    fetch_iter t ~params sql d ~f:(fun x -> acc := x :: !acc);
    List.rev !acc
  ;;

  let fetch_one t ?(params = []) sql d =
    match fetch t ~params sql d with
    | [] -> None
    | x :: _ -> Some x
  ;;
end
