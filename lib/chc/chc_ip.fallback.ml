(* Hand-rolled fallback, selected when ipaddr is not installed; see lib/chc/dune.

   Held to the same behaviour as the ipaddr backend by chc_ip.mli and by the
   test suite, which compares both against the server's own toString. *)

let backend = "builtin"

let v4_to_string u =
  let b k = Int64.to_int (Int64.logand (Int64.shift_right_logical u k) 0xFFL) in
  Printf.sprintf "%d.%d.%d.%d" (b 24) (b 16) (b 8) (b 0)
;;

let v6_to_string be =
  if String.length be <> 16
  then invalid_arg (Printf.sprintf "Chc: IPv6 needs 16 bytes, got %d" (String.length be))
  else (
    let group i = (Char.code be.[2 * i] lsl 8) lor Char.code be.[(2 * i) + 1] in
    let g = Array.init 8 group in
    let mapped = g.(0) = 0 && g.(1) = 0 && g.(2) = 0 && g.(3) = 0 && g.(4) = 0 && g.(5) = 0xffff in
    if mapped
    then Printf.sprintf "::ffff:%d.%d.%d.%d" (Char.code be.[12]) (Char.code be.[13]) (Char.code be.[14]) (Char.code be.[15])
    else (
      let best_at = ref (-1)
      and best_len = ref 0
      and at = ref (-1)
      and len = ref 0 in
      for i = 0 to 7 do
        if g.(i) = 0
        then (
          if !at < 0 then at := i;
          incr len;
          if !len > !best_len
          then (
            best_len := !len;
            best_at := !at))
        else (
          at := -1;
          len := 0)
      done;
      if !best_len < 2 then best_at := -1;
      let hexes = Array.to_list (Array.map (Printf.sprintf "%x") g) in
      if !best_at < 0
      then String.concat ":" hexes
      else (
        let take from len = List.filteri (fun i _ -> i >= from && i < from + len) hexes in
        String.concat ":" (take 0 !best_at) ^ "::" ^ String.concat ":" (take (!best_at + !best_len) (8 - !best_at - !best_len)))))
;;

let v4_of_string s =
  match String.split_on_char '.' s with
  | [ a; b; c; d ] ->
    let p x =
      let v = int_of_string x in
      if v < 0 || v > 255 then invalid_arg (Printf.sprintf "Chc: %S is not an IPv4 address" s);
      Int64.of_int v
    in
    Int64.logor (Int64.shift_left (p a) 24) (Int64.logor (Int64.shift_left (p b) 16) (Int64.logor (Int64.shift_left (p c) 8) (p d)))
  | _ -> invalid_arg (Printf.sprintf "Chc: %S is not an IPv4 address" s)
;;

let v6_of_string s =
  let bad () = invalid_arg (Printf.sprintf "Chc: %S is not an IPv6 address" s) in
  let expand part =
    if part = ""
    then []
    else (
      let chunks = String.split_on_char ':' part in
      List.concat_map
        (fun c ->
           if String.contains c '.'
           then (
             let v = v4_of_string c in
             let hi = Int64.to_int (Int64.shift_right_logical v 16) land 0xffff in
             let lo = Int64.to_int v land 0xffff in
             [ hi; lo ])
           else if c = ""
           then bad ()
           else (
             let v = int_of_string ("0x" ^ c) in
             if v < 0 || v > 0xffff then bad ();
             [ v ]))
        chunks)
  in
  let groups =
    match
      (* split on the single "::" run, if any *)
      let rec find i = if i + 1 >= String.length s then None else if s.[i] = ':' && s.[i + 1] = ':' then Some i else find (i + 1) in
      find 0
    with
    | None ->
      let g = expand s in
      if List.length g <> 8 then bad ();
      g
    | Some i ->
      let left = expand (String.sub s 0 i) in
      let right = expand (String.sub s (i + 2) (String.length s - i - 2)) in
      let fill = 8 - List.length left - List.length right in
      if fill < 0 then bad ();
      left @ List.init fill (fun _ -> 0) @ right
  in
  let b = Bytes.create 16 in
  List.iteri
    (fun i g ->
       Bytes.set b (2 * i) (Char.unsafe_chr ((g lsr 8) land 0xff));
       Bytes.set b ((2 * i) + 1) (Char.unsafe_chr (g land 0xff)))
    groups;
  Bytes.unsafe_to_string b
;;
