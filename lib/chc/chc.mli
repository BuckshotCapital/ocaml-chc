(** OCaml bindings to {{:https://github.com/ClickHouse/clickhouse-c}clickhouse-c}.

    Stage 1: decoding the ClickHouse Native wire format from a file descriptor — a [clickhouse local --format Native] pipe, a captured
    [.native] dump, or anything else emitting bare blocks. No TCP, no compression, no codecs linked.

    The TCP client is deliberately absent: the eventual network path binds [clickhouse-async.h], which never touches a socket, so sockets
    and TLS stay on the OCaml side and the same core works under Unix, Lwt and Eio. *)

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

    Types with no faithful OCaml counterpart yet — 128/256-bit integers and decimals, [UUID], [IPv6], [BFloat16] — surface as {!Raw} holding
    their little-endian wire bytes rather than being lossily coerced. [Map] decodes as {!Arr} of two-element {!Tup}, matching its physical
    layout. *)
type value =
  | Null
  | Bool of bool
  | Int of int64 (** signed integers, and scaled decimal/time mantissas *)
  | Uint of int64 (** unsigned; reinterpret via [Int64.unsigned_*] *)
  | Float of float
  | Str of string
  | Raw of string (** fixed-width bytes, little-endian *)
  | Arr of value array
  | Tup of value array

val string_of_value : value -> string

(** {1 Blocks} *)

type block

val n_rows : block -> int
val n_columns : block -> int
val column_name : block -> int -> string

(** Printable ClickHouse type, e.g. ["Array(Nullable(String))"]. *)
val column_type_name : block -> int -> string

val column_kind : block -> int -> Kind.t
val column_layout : block -> int -> Layout.t

(** Decode column [i] in full. Values are OCaml-owned, so they outlive the block; nothing aliases C memory. *)
val column : block -> int -> value array

val columns : block -> value array array

(** Row-major transpose of {!columns}. Convenient, not efficient. *)
val rows : block -> value array array

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
