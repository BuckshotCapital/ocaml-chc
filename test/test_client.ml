(* Native TCP client, exercised against a live ClickHouse.

   Opt-in and instance-agnostic: everything here runs against `numbers()` and
   literals, so it works on any server and hardcodes nothing about one. Set
   CHC_TEST_HOST to enable; without it the test reports skipped and passes, so
   a checkout with no server still builds green.

     CHC_TEST_HOST=... CHC_TEST_PASSWORD=... dune test *)

let failures = ref 0

let check name cond =
  if cond
  then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_eq name ~expected ~actual =
  if expected = actual
  then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n    expected: %s\n    actual:   %s\n" name expected actual)
;;

let env k default =
  match Sys.getenv_opt k with
  | Some v when v <> "" -> v
  | _ -> default
;;

let show = Chc.string_of_value

let one_value c sql =
  let _, rows = Chc.Client.query_rows c sql in
  if Array.length rows = 0 then "<no rows>" else show rows.(0).(0)
;;

let run host =
  let c =
    Chc.Client.connect
      ~port:(int_of_string (env "CHC_TEST_PORT" "9000"))
      ~user:(env "CHC_TEST_USER" "default")
      ~password:(env "CHC_TEST_PASSWORD" "")
      ~database:(env "CHC_TEST_DATABASE" "default")
      host
  in
  print_endline "handshake";
  let si = Chc.Client.server_info c in
  check "server name non-empty" (String.length si.Chc.Async.server_name > 0);
  check "revision negotiated" (si.Chc.Async.revision > 0);
  Printf.printf
    "  (server %s %d.%d.%d rev %d)\n"
    si.Chc.Async.server_name
    si.Chc.Async.version_major
    si.Chc.Async.version_minor
    si.Chc.Async.version_patch
    si.Chc.Async.revision;
  print_endline "scalars over the wire";
  check_eq "int" ~expected:"-5" ~actual:(one_value c "SELECT toInt32(-5)");
  check_eq "uint64" ~expected:"42" ~actual:(one_value c "SELECT toUInt64(42)");
  check_eq "float" ~expected:"1.5" ~actual:(one_value c "SELECT toFloat64(1.5)");
  check_eq "string" ~expected:"hello" ~actual:(one_value c "SELECT 'hello'");
  check_eq "utf8" ~expected:"ünïcødé" ~actual:(one_value c "SELECT 'ünïcødé'");
  check_eq "null" ~expected:"NULL" ~actual:(one_value c "SELECT CAST(NULL, 'Nullable(Int64)')");
  print_endline "composites over the wire";
  check_eq "array" ~expected:"[1, 2, 3]" ~actual:(one_value c "SELECT [1, 2, 3]");
  check_eq "tuple" ~expected:"(1, x)" ~actual:(one_value c "SELECT (toUInt8(1), 'x')");
  check_eq "map" ~expected:"[(a, 1), (b, 2)]" ~actual:(one_value c "SELECT map('a', 1, 'b', 2)");
  check_eq "lowcardinality" ~expected:"z" ~actual:(one_value c "SELECT toLowCardinality('z')");
  check_eq "nested nullable array" ~expected:"[[a, NULL], [b]]" ~actual:(one_value c "SELECT [['a', NULL], ['b']]");
  print_endline "column metadata";
  let names, _ = Chc.Client.query_rows c "SELECT 1 AS a, 'x' AS b" in
  check_eq "column names" ~expected:"a,b" ~actual:(String.concat "," (Array.to_list names));
  let types = ref [] in
  Chc.Client.query_iter c "SELECT toInt32(1) AS a, toLowCardinality('x') AS b" ~f:(fun b ->
    if !types = [] then types := List.init (Chc.n_columns b) (Chc.column_type_name b));
  check_eq "column types" ~expected:"Int32,LowCardinality(String)" ~actual:(String.concat "," !types);
  print_endline "multi-block streaming";
  let rows = ref 0 in
  let blocks = ref 0 in
  Chc.Client.query_iter c "SELECT number FROM numbers(250000)" ~f:(fun b ->
    incr blocks;
    rows := !rows + Chc.n_rows b);
  check_eq "250k rows streamed" ~expected:"250000" ~actual:(string_of_int !rows);
  check "arrived in several blocks" (!blocks > 1);
  Printf.printf "  (%d blocks)\n" !blocks;
  print_endline "sum check across blocks";
  let total = ref 0L in
  Chc.Client.query_iter c "SELECT toInt64(number) FROM numbers(100000)" ~f:(fun b ->
    Array.iter
      (function
        | Chc.Int i -> total := Int64.add !total i
        | _ -> ())
      (Chc.column b 0));
  check_eq "sum 0..99999" ~expected:"4999950000" ~actual:(Int64.to_string !total);
  print_endline "server exception path";
  (match Chc.Client.query_rows c "SELECT this_function_does_not_exist(1)" with
   | _ ->
     incr failures;
     print_endline "  FAIL bad SQL did not raise"
   | exception Chc.Error e ->
     check "raises Chc.Error" true;
     check "carries server_code" (e.Chc.Error.server_code <> 0);
     check "carries a message" (String.length e.Chc.Error.msg > 0);
     Printf.printf "  (server_code %d, %s)\n" e.Chc.Error.server_code e.Chc.Error.server_name);
  print_endline "connection survives a failed query";
  check_eq "still usable" ~expected:"7" ~actual:(one_value c "SELECT 7");
  print_endline "empty result";
  let _, empty = Chc.Client.query_rows c "SELECT 1 WHERE 0" in
  check_eq "no rows" ~expected:"0" ~actual:(string_of_int (Array.length empty));
  Chc.Client.close c
;;

let () =
  match Sys.getenv_opt "CHC_TEST_HOST" with
  | None | Some "" ->
    print_endline "skipped: set CHC_TEST_HOST to run the live client tests";
    exit 0
  | Some host ->
    run host;
    print_endline "";
    if !failures = 0
    then print_endline "all checks passed"
    else (
      Printf.printf "%d check(s) failed\n" !failures;
      exit 1)
;;
