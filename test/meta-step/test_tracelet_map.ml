open Batteries

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter.Mega_step
open Tracelet

open Program_samples

let check_point map x =
  let id = (Ident x) in
  let tracelet = Ident_map.find id map in
  assert (tracelet.point = id)

(* let check_contains ?(expected=true) map x1 x2 = 
   let id1 = (Ident x1) 
   and id2 = (Ident x2) 
   in
   let tracelet = Ident_map.find id1 map in
   let contains block = 
    (List.exists (fun (Clause (Var (id, _), _)) -> id = id1) block.clauses)
    || (List.mem id2 block.app_ids)
    || (List.mem id2 block.cond_ids)
   in
   if either_block tracelet.blocks contains then
    assert expected
   else
    assert (not expected) *)

let check_tracelet tracelet (expectd_direct_cids, expectd_app_ids, expectd_cond_ids) = 
  let direct_cids = direct_cids_of tracelet in
  (* print_endline @@ String.concat " " direct_cids; *)
  let _ = assert (direct_cids = expectd_direct_cids) in

  let app_ids = app_ids_of tracelet in
  (* print_endline @@ String.concat " " app_ids; *)
  let _ = assert (app_ids = expectd_app_ids) in

  let cond_ids = cond_ids_of tracelet in
  (* print_endline @@ String.concat " " cond_ids; *)
  let _ = assert (cond_ids = expectd_cond_ids) in

  assert true

let check_ids map x expects =
  let idx = (Ident x) in
  let tracelet = Ident_map.find idx map in
  check_tracelet tracelet expects


let check_partial map x expects =
  let tracelet = search_id (Ident x) map in
  check_tracelet tracelet expects

let m1 = tracelet_map_of_expr e1
let _ = check_point m1 "b"
let _ = check_ids m1 name_main (["t"], [], [])
let _ = check_partial m1 "a" ([], [], [])

let m2 = tracelet_map_of_expr e2
let _ = check_point m2 "b1"
let _ = check_point m2 "b2"
let _ = check_ids m2 "b1" (["z"; "b"], ["t"], [])
let _ = check_partial m2 "t" (["z"; "b"], [], [])
let _ = check_ids m2 "b2" (["a"], [], [])
let m3 = tracelet_map_of_expr e3
let _ = check_point m3 "w2"
let _ = check_point m3 "w1"
let _ = check_point m3 "b"
let _ = check_ids m3 "w1" (["c"], [], ["b"])
let _ = check_partial m3 "b" (["c"], [], [])
let _ = check_ids m3 "b" (["z"; "x"; "y"], [], [])
let _ = check_partial m3 "x" (["z"], [], [])
let _ = check_partial m3 "y" ([], [], [])
let _ = check_ids m3 name_main (["wb"], [], ["w2"])
let _ = check_partial m3 "w2" (["wb"], [], [])

let m4 = tracelet_map_of_expr e4
let _ = check_point m4 "b"
let _ = check_point m4 "b2"
let _ = check_ids m4 "b" (["z"; "x"], [], ["b2"])
let _ = check_ids m4 "b2" (["z2"; "x2"; "y2"], ["u2"], [])
let _ = check_partial m4 "u2" (["y2"], [], [])
let _ = check_partial m4 "x" (["z"], [], [])

let m5 = tracelet_map_of_expr e5
let _ = check_point m5 "id"
let _ = check_point m5 "f"
let _ = check_point m5 "g"
let _ = check_ids m5 name_main (["c"], ["b1"; "b2"], [])
let _ = check_ids m5 "f" ([], [], [])
let _ = check_ids m5 "g" ([], ["h"], [])

let export = ()