(* Async driver, exercised against a live ClickHouse.

   Same opt-in as test_client: set CHC_TEST_HOST to run, otherwise report
   skipped and pass. Instance-agnostic — numbers(), literals, and tables it
   creates and drops itself. *)

open! Core
open! Async

let failures = ref 0

let check name cond =
  if cond
  then printf "  ok   %s\n" name
  else (
    incr failures;
    printf "  FAIL %s\n" name)
;;

let check_eq name ~expected ~actual =
  if String.equal expected actual
  then printf "  ok   %s\n" name
  else (
    incr failures;
    printf "  FAIL %s\n    expected: %s\n    actual:   %s\n" name expected actual)
;;

let env k default =
  match Sys.getenv k with
  | Some v when not (String.is_empty v) -> v
  | _ -> default
;;

let int_row =
  let open Chc.Row in
  let+ x = at 0 int in
  x
;;

let text_row =
  let open Chc.Row in
  let+ x = at 0 text in
  x
;;

let one ?(params = []) conn sql =
  match%map Chc_async.fetch_one conn ~params sql text_row with
  | Some x -> x
  | None -> "<no rows>"
;;

let connect () =
  Chc_async.connect
    ~port:(Int.of_string (env "CHC_TEST_PORT" "9000"))
    ~user:(env "CHC_TEST_USER" "default")
    ~password:(env "CHC_TEST_PASSWORD" "")
    ~database:(env "CHC_TEST_DATABASE" "default")
    (env "CHC_TEST_HOST" "127.0.0.1")
;;

let run () =
  let%bind conn = connect () in
  print_endline "handshake";
  let si = Chc_async.server_info conn in
  check "server name non-empty" (not (String.is_empty si.Chc.Protocol.server_name));
  check "revision negotiated" (si.Chc.Protocol.revision > 0);
  print_endline "handshake rejection";
  let%bind rejected =
    Monitor.try_with ~run:`Now (fun () ->
      Chc_async.connect
        ~port:(Int.of_string (env "CHC_TEST_PORT" "9000"))
        ~user:(env "CHC_TEST_USER" "default")
        ~password:(env "CHC_TEST_PASSWORD" "")
        ~database:"no such database"
        (env "CHC_TEST_HOST" "127.0.0.1"))
  in
  let%bind () =
    match rejected with
    | Ok c ->
      incr failures;
      print_endline "  FAIL unknown database did not raise";
      Chc_async.close c
    | Error exn ->
      (match Monitor.extract_exn exn with
       | Chc.Error e ->
         check "raises Chc.Error into the monitor" true;
         check "carries server_code" (e.Chc.Error.server_code <> 0);
         check "carries the server's text" (String.is_substring e.Chc.Error.msg ~substring:"no such database")
       | other ->
         incr failures;
         printf "  FAIL unexpected exception %s\n" (Exn.to_string other));
      return ()
  in
  print_endline "scalars";
  let%bind v = one conn "SELECT toInt32(-5)" in
  check_eq "int" ~expected:"-5" ~actual:v;
  let%bind v = one conn "SELECT 'hello'" in
  check_eq "string" ~expected:"hello" ~actual:v;
  print_endline "streaming through a pipe";
  let%bind rows, blocks =
    Pipe.fold (Chc_async.query_pipe conn "SELECT number FROM numbers(250000)") ~init:(0, 0) ~f:(fun (r, b) blk ->
      return (r + Chc.n_rows blk, b + 1))
  in
  check_eq "250k rows streamed" ~expected:"250000" ~actual:(Int.to_string rows);
  check "arrived in several blocks" (blocks > 1);
  printf "  (%d blocks)\n" blocks;
  print_endline "the consumer drives the read";
  (* Take three blocks and walk away. The rest of the response is drained in
     the background, so the connection has to still be usable afterwards. *)
  let pipe = Chc_async.query_pipe conn "SELECT number FROM numbers(2000000)" in
  let%bind taken = Pipe.read_exactly pipe ~num_values:3 in
  check
    "read the first blocks"
    (match taken with
     | `Exactly q -> Queue.length q = 3
     | _ -> false);
  Pipe.close_read pipe;
  let%bind v = one conn "SELECT 11" in
  check_eq "connection usable after abandoning a pipe" ~expected:"11" ~actual:v;
  print_endline "typed fetch and params";
  let%bind got = Chc_async.fetch conn "SELECT number AS n FROM numbers(4)" int_row in
  check_eq "fetch" ~expected:"0,1,2,3" ~actual:(String.concat ~sep:"," (List.map got ~f:Int.to_string));
  let%bind v = one conn ~params:[ "v", Chc.Param.string "o'brien\\x" ] "SELECT {v:String}" in
  check_eq "param survives as data" ~expected:"o'brien\\x" ~actual:v;
  let%bind () = Chc_async.ping conn in
  check "ping" true;
  print_endline "server exception path";
  let%bind r = Monitor.try_with ~run:`Now (fun () -> one conn "SELECT this_function_does_not_exist(1)") in
  (match r with
   | Ok _ ->
     incr failures;
     print_endline "  FAIL bad SQL did not raise"
   | Error exn ->
     (match Monitor.extract_exn exn with
      | Chc.Error e ->
        check "raises Chc.Error into the monitor" true;
        check "carries server_code" (e.Chc.Error.server_code <> 0)
      | other ->
        incr failures;
        printf "  FAIL unexpected exception %s\n" (Exn.to_string other)));
  let%bind v = one conn "SELECT 7" in
  check_eq "connection survives a failed query" ~expected:"7" ~actual:v;
  print_endline "insert";
  let tbl = sprintf "chc_async_%d" (Unix.getpid () |> Pid.to_int) in
  let%bind () = Chc_async.execute conn (sprintf "DROP TABLE IF EXISTS %s" tbl) in
  let%bind () =
    Chc_async.execute conn (sprintf "CREATE TABLE %s (id UInt32, tags Array(String), venue LowCardinality(String)) ENGINE = Memory" tbl)
  in
  let%bind () =
    Chc_async.insert
      conn
      tbl
      [| [| Chc.Uint 1L; Chc.Arr [| Chc.Str "a" |]; Chc.Str "hyperliquid" |]; [| Chc.Uint 2L; Chc.Arr [||]; Chc.Str "aster" |] |]
  in
  let%bind v = one conn (sprintf "SELECT count() FROM %s" tbl) in
  check_eq "rows inserted" ~expected:"2" ~actual:v;
  let%bind v = one conn (sprintf "SELECT tags[1] FROM %s WHERE id = 1" tbl) in
  check_eq "composite survived" ~expected:"a" ~actual:v;
  let%bind () = Chc_async.execute conn (sprintf "DROP TABLE IF EXISTS %s" tbl) in
  print_endline "inserter holds one statement open across many flushes";
  (* MergeTree, so system.parts can testify: one open statement squashes into
     one part, whereas a statement per flush would leave one part per flush. *)
  let itbl = tbl ^ "_ins" in
  let%bind () = Chc_async.execute conn (sprintf "DROP TABLE IF EXISTS %s" itbl) in
  let%bind () =
    Chc_async.execute
      conn
      (sprintf "CREATE TABLE %s (ts DateTime, venue LowCardinality(String), rate Float64) ENGINE = MergeTree ORDER BY (venue, ts)" itbl)
  in
  let%bind writer = connect () in
  let%bind ins = Chc_async.Inserter.create writer itbl ~max_rows:500 in
  let flushes = 40
  and per = 500 in
  let%bind () =
    Deferred.repeat_until_finished 0 (fun i ->
      if i >= flushes * per
      then return (`Finished ())
      else (
        let%bind () = Chc_async.Inserter.write ins [| Chc.Uint 1755000000L; Chc.Str "hyperliquid"; Chc.Float (Int.to_float i) |] in
        let%map () = Chc_async.Inserter.commit ins in
        `Repeat (i + 1)))
  in
  check_eq
    "rows written before close"
    ~expected:(Int.to_string (flushes * per))
    ~actual:(Int.to_string (Chc_async.Inserter.written_rows ins));
  print_endline "  a query on the held connection is refused";
  let%bind r = Monitor.try_with ~run:`Now (fun () -> one writer "SELECT 1") in
  check
    "query during an open Inserter raises"
    (match r with
     | Error _ -> true
     | Ok _ -> false);
  let%bind () = Chc_async.Inserter.close ins in
  let%bind v = one writer "SELECT 3" in
  check_eq "connection released after close" ~expected:"3" ~actual:v;
  let%bind v = one conn (sprintf "SELECT count() FROM %s" itbl) in
  check_eq "every row landed" ~expected:(Int.to_string (flushes * per)) ~actual:v;
  let%bind v = one conn (sprintf "SELECT count() FROM system.parts WHERE table = '%s' AND active" itbl) in
  check_eq "one open statement made one part" ~expected:"1" ~actual:v;
  let%bind () = Chc_async.execute conn (sprintf "DROP TABLE IF EXISTS %s" itbl) in
  let%bind () = Chc_async.close writer in
  let%bind () = Chc_async.close conn in
  print_endline "";
  if !failures = 0
  then (
    print_endline "all checks passed";
    return 0)
  else (
    printf "%d check(s) failed\n" !failures;
    return 1)
;;

let () =
  match Sys.getenv "CHC_TEST_HOST" with
  | None | Some "" ->
    (* Stdlib's, not the one [open Async] shadows it with: that one writes
       through an Async writer, which never flushes because the scheduler is
       never started on this path — a skipped run would print nothing. *)
    Stdlib.print_endline "skipped: set CHC_TEST_HOST to run the live Async tests";
    Stdlib.exit 0
  | Some _ -> Stdlib.exit (Thread_safe.block_on_async_exn run)
;;
