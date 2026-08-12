(* ipaddr-backed. Selected when the library is installed; see lib/chc/dune.

   ipaddr's rendering was checked against ClickHouse's before adopting it, and
   agrees on every form the test suite covers, IPv4-mapped addresses included. *)

let backend = "ipaddr"

(* Normalise ipaddr's exception to the one the fallback raises, so callers see
   one failure mode regardless of which backend is compiled in. *)
let parse what f s =
  try f s with
  | Ipaddr.Parse_error (msg, _) -> invalid_arg (Printf.sprintf "Chc: %S is not %s (%s)" s what msg)
;;

let v4_to_string u = Ipaddr.V4.to_string (Ipaddr.V4.of_int32 (Int64.to_int32 u))

(* to_int32 is signed; the wire value is unsigned, so mask back up. *)
let v4_of_string s = Int64.logand (Int64.of_int32 (Ipaddr.V4.to_int32 (parse "an IPv4 address" Ipaddr.V4.of_string_exn s))) 0xFFFFFFFFL

let v6_to_string s =
  if String.length s <> 16 then invalid_arg (Printf.sprintf "Chc: IPv6 needs 16 bytes, got %d" (String.length s));
  Ipaddr.V6.to_string (parse "an IPv6 address" Ipaddr.V6.of_octets_exn s)
;;

let v6_of_string s = Ipaddr.V6.to_octets (parse "an IPv6 address" Ipaddr.V6.of_string_exn s)
