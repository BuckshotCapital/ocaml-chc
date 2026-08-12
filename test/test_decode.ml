(* Round-trip against a real ClickHouse: `clickhouse local` emits FORMAT Native
   bytes, we decode them and assert on the values. No server, no socket — this
   exercises the whole marshalling layer against genuine wire output. *)

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

let clickhouse_bin =
  match Sys.getenv_opt "CLICKHOUSE_BIN" with
  | Some b -> b
  | None -> "clickhouse"
;;

(* Run a query and hand back an fd positioned at the start of the Native bytes.
   A temp file rather than a pipe: no plumbing, and it keeps the test focused
   on decoding rather than on I/O scheduling. *)
let native_fd sql =
  let tmp = Filename.temp_file "chc_test" ".native" in
  let cmd =
    Printf.sprintf
      "%s local --format Native --query %s > %s 2>/dev/null"
      (Filename.quote clickhouse_bin)
      (Filename.quote sql)
      (Filename.quote tmp)
  in
  let rc = Sys.command cmd in
  if rc <> 0
  then (
    (try Sys.remove tmp with
     | _ -> ());
    failwith (Printf.sprintf "`%s local` exited %d — is clickhouse on PATH?" clickhouse_bin rc));
  let fd = Unix.openfile tmp [ Unix.O_RDONLY ] 0 in
  ( fd
  , fun () ->
      Unix.close fd;
      try Sys.remove tmp with
      | _ -> () )
;;

let read_all sql =
  let fd, cleanup = native_fd sql in
  let r = Chc.open_fd fd in
  let blocks = List.rev (Chc.fold r ~init:[] ~f:(fun acc b -> b :: acc)) in
  (* Force decoding before the blocks are dropped; values are OCaml-owned so
     they stay valid after cleanup. *)
  let decoded = List.map (fun b -> Chc.n_rows b, Chc.columns b) blocks in
  let names =
    match blocks with
    | [] -> [||]
    | b :: _ -> Array.init (Chc.n_columns b) (Chc.column_name b)
  in
  let types =
    match blocks with
    | [] -> [||]
    | b :: _ -> Array.init (Chc.n_columns b) (Chc.column_type_name b)
  in
  Chc.close r;
  cleanup ();
  names, types, decoded
;;

(* Concatenate a column across blocks — clickhouse-local may split output. *)
let col_all decoded i = Array.concat (List.map (fun (_, cols) -> cols.(i)) decoded)
let show v = Chc.string_of_value v

let () =
  print_endline "scalars";
  let _, types, d = read_all "SELECT toInt32(-5) AS a, toUInt64(42) AS b, toFloat64(1.5) AS c, 'hello' AS s, toBool(true) AS t" in
  check_eq "type a" ~expected:"Int32" ~actual:types.(0);
  check_eq "type b" ~expected:"UInt64" ~actual:types.(1);
  check_eq "value a" ~expected:"-5" ~actual:(show (col_all d 0).(0));
  check_eq "value b" ~expected:"42" ~actual:(show (col_all d 1).(0));
  check_eq "value c" ~expected:"1.5" ~actual:(show (col_all d 2).(0));
  check_eq "value s" ~expected:"hello" ~actual:(show (col_all d 3).(0));
  check_eq "value t" ~expected:"true" ~actual:(show (col_all d 4).(0));
  print_endline "multi-row + nullable";
  let _, types, d = read_all "SELECT number AS n, if(number % 2 = 0, NULL, toInt64(number * 10)) AS maybe FROM numbers(6)" in
  check_eq "nullable type" ~expected:"Nullable(Int64)" ~actual:types.(1);
  let n = col_all d 0
  and maybe = col_all d 1 in
  check "6 rows" (Array.length n = 6);
  check_eq "n row 3" ~expected:"3" ~actual:(show n.(3));
  check_eq "null at 0" ~expected:"NULL" ~actual:(show maybe.(0));
  check_eq "value at 3" ~expected:"30" ~actual:(show maybe.(3));
  print_endline "strings";
  let _, _, d = read_all "SELECT arrayJoin(['', 'a', 'ünïcødé', 'x']) AS s" in
  let s = col_all d 0 in
  check_eq "empty string" ~expected:"" ~actual:(show s.(0));
  check_eq "utf8 string" ~expected:"ünïcødé" ~actual:(show s.(2));
  print_endline "array";
  let _, types, d = read_all "SELECT [1, 2, 3] AS a UNION ALL SELECT [] UNION ALL SELECT [7]" in
  check_eq "array type" ~expected:"Array(UInt8)" ~actual:types.(0);
  let a = col_all d 0 in
  (* UNION ALL does not promise an order, so compare the sorted rendering. *)
  let sorted = List.sort compare (Array.to_list (Array.map show a)) in
  check_eq "arrays" ~expected:"[1, 2, 3]; [7]; []" ~actual:(String.concat "; " sorted);
  print_endline "nested array of nullable string";
  let _, types, d = read_all "SELECT [['a', NULL], ['b']] AS a" in
  check_eq "nested type" ~expected:"Array(Array(Nullable(String)))" ~actual:types.(0);
  check_eq "nested value" ~expected:"[[a, NULL], [b]]" ~actual:(show (col_all d 0).(0));
  print_endline "tuple";
  let _, types, d = read_all "SELECT (toUInt8(1), 'x', toFloat64(2.5)) AS t" in
  check_eq "tuple type" ~expected:"Tuple(UInt8, String, Float64)" ~actual:types.(0);
  check_eq "tuple value" ~expected:"(1, x, 2.5)" ~actual:(show (col_all d 0).(0));
  print_endline "map";
  let _, types, d = read_all "SELECT map('a', 1, 'b', 2) AS m" in
  check_eq "map type" ~expected:"Map(String, UInt8)" ~actual:types.(0);
  check_eq "map value" ~expected:"[(a, 1), (b, 2)]" ~actual:(show (col_all d 0).(0));
  print_endline "low cardinality";
  let _, types, d = read_all "SELECT toLowCardinality(arrayJoin(['x', 'y', 'x', 'x'])) AS lc" in
  check_eq "lc type" ~expected:"LowCardinality(String)" ~actual:types.(0);
  let lc = col_all d 0 in
  check_eq "lc values" ~expected:"x,y,x,x" ~actual:(String.concat "," (Array.to_list (Array.map show lc)));
  print_endline "low cardinality nullable";
  let _, _, d = read_all "SELECT toLowCardinality(if(number = 1, NULL, 'v')) AS lc FROM numbers(3)" in
  check_eq "lc nullable" ~expected:"v,NULL,v" ~actual:(String.concat "," (Array.to_list (Array.map show (col_all d 0))));
  Printf.printf "ip backend: %s\n" Chc.ip_backend;
  check "ip backend is one of the two" (Chc.ip_backend = "ipaddr" || Chc.ip_backend = "builtin");
  print_endline "wide types render as ClickHouse does";
  (* clickhouse-local renders the same canonical text the server does, so the
     second column is the oracle for the first. *)
  let agrees expr =
    let _, _, d = read_all (Printf.sprintf "SELECT %s AS v, toString(%s) AS s" expr expr) in
    check_eq ("toString " ^ expr) ~expected:(show (col_all d 1).(0)) ~actual:(show (col_all d 0).(0))
  in
  agrees "toUUID('61f0c404-5cb3-11e7-907b-a6006ad3dba0')";
  agrees "toIPv4('192.168.1.1')";
  agrees "toIPv6('2001:db8::1')";
  agrees "toIPv6('::ffff:192.168.1.1')";
  agrees "toInt128('-170141183460469231731687303715884105728')";
  agrees "toUInt256('115792089237316195423570985008687907853269984665640564039457584007913129639935')";
  agrees "toDecimal64('1.2345', 4)";
  agrees "toDecimal32(-1.5, 2)";
  agrees "toDecimal128('-9.87654321', 8)";
  let _, _, d = read_all "SELECT toUUID('61f0c404-5cb3-11e7-907b-a6006ad3dba0')" in
  check
    "uuid uses the Uuid constructor"
    (match (col_all d 0).(0) with
     | Chc.Uuid _ -> true
     | _ -> false);
  let _, _, d = read_all "SELECT toDecimal64('1.2345', 4)" in
  check
    "decimal uses the Decimal constructor"
    (match (col_all d 0).(0) with
     | Chc.Decimal _ -> true
     | _ -> false);
  print_endline "larger stream";
  let _, _, d = read_all "SELECT number AS n FROM numbers(100000)" in
  let n = col_all d 0 in
  check "100k rows" (Array.length n = 100_000);
  check_eq "last row" ~expected:"99999" ~actual:(show n.(99_999));
  print_endline "";
  if !failures = 0
  then print_endline "all checks passed"
  else (
    Printf.printf "%d check(s) failed\n" !failures;
    exit 1)
;;
