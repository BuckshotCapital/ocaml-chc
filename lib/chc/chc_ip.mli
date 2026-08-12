(** IPv4 and IPv6 text conversion.

    Two interchangeable implementations, chosen by dune's [(select)] on whether
    [ipaddr] is installed. This signature is the whole point of the arrangement:
    because both backends deal in [string], the choice cannot reach {!Chc.value}
    and the dependency stays genuinely optional. A [(select)] that changed a
    public type would be far worse than hand-rolled code — the API would differ
    by what happened to be installed, invisibly.

    Both backends are held to ClickHouse's own rendering, which the test suite
    checks by comparing against the server's [toString]. *)

(** ["ipaddr"] or ["builtin"]. Surfaced as {!Chc.ip_backend}. *)
val backend : string

(** The argument is the wire [UInt32], zero-extended into an [int64]. *)
val v4_to_string : int64 -> string

(** @raise Invalid_argument unless [s] is a dotted quad. *)
val v4_of_string : string -> int64

(** [s] is the 16 network-order wire bytes. Produces RFC 5952 form — lowercase,
    longest zero run collapsed to ["::"] — with a dotted-quad tail for
    IPv4-mapped addresses.

    @raise Invalid_argument unless [String.length s = 16]. *)
val v6_to_string : string -> string

(** Inverse of {!v6_to_string}, returning 16 network-order bytes.

    @raise Invalid_argument unless [s] is an IPv6 address. *)
val v6_of_string : string -> string
