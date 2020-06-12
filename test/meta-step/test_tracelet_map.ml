open Batteries

open Odefa_ast
open Ast

open Odefa_parser
open Odefa_symbolic_interpreter.Mega_step
open Tracelet

let parse s = 
  s
  |> IO.input_string
  |> Parser.parse_program

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

let check_ids map x (expectd_cids, expectd_app_ids, expectd_cond_ids) =
  let idx = (Ident x) in
  let tracelet = Ident_map.find idx map in

  let cids = direct_cid_of_tracelet tracelet in
  (* print_endline @@ String.concat " " cids; *)
  let _ = assert (cids = expectd_cids) in

  let app_ids_of block = 
    List.map (fun (Ident id) -> id) block.app_ids in
  let app_ids = list_in_block tracelet.blocks app_ids_of in
  let _ = assert (app_ids = expectd_app_ids) in

  let cond_ids_of block = 
    List.map (fun (Ident id) -> id) block.cond_ids in
  let cond_ids = list_in_block tracelet.blocks cond_ids_of in
  let _ = assert (cond_ids = expectd_cond_ids) in

  assert true

let check_partial map b x expected_ss =
  let tracelet = Ident_map.find (Ident b) map in
  let ss = debug_pred_ids_of_tracelet (Ident x) tracelet in
  (* let direct_ids, app_ids, cond_ids = ss
     print_endline @@ String.concat " " direct_ids;
     print_endline @@ String.concat " " app_ids;
     print_endline @@ String.concat " " cond_ids *)
  assert (ss = expected_ss)


let e1 = parse "
b = fun t -> (
  a = 1
);
t = 1
"

let m1 = tracelet_map_of_expr e1
let _ = check_point m1 "b"
let _ = check_ids m1 name_main (["t"], [], [])
let _ = check_partial m1 "b" "a" ([], [], [])

let e2 = parse "
b1 = fun t1 -> (
  z = 1;
  b2 = fun t2 -> (
    a = 1
  );
  b = true;
  t = b2 z
)
"

let m2 = tracelet_map_of_expr e2
let _ = check_point m2 "b1"
let _ = check_point m2 "b2"
let _ = check_ids m2 "b1" (["z"; "b"], ["t"], [])
let _ = check_partial m2 "b1" "t" (["z"; "b"], [], [])
let _ = check_ids m2 "b2" (["a"], [], [])

let e3 = parse "
wb = true;
w2 = wb ? (
  w1 = fun d1 -> (
    c = true;
    b = c ? (
      z = 1;
      x = 1
    ) : (
      y = 2
    )
  )
) : (
  t = 1
)
"

let m3 = tracelet_map_of_expr e3
let _ = check_point m3 "w2"
let _ = check_point m3 "w1"
let _ = check_point m3 "b"
let _ = check_ids m3 "w1" (["c"], [], ["b"])
let _ = check_partial m3 "w1" "b" (["c"], [], [])
let _ = check_ids m3 "b" (["z"; "x"; "y"], [], [])
let _ = check_partial m3 "b" "x" (["z"], [], [])
let _ = check_partial m3 "b" "y" ([], [], [])
let _ = check_ids m3 name_main (["wb"], [], ["w2"])
let _ = check_partial m3 name_main "w2" (["wb"], [], [])

let e4 = parse "
c = true;
b = c ? (
  z = 1;
  x = 1
) : (
  b2 = c ? (
    z2 = 1;
    x2 = 1;
  ) : (
    y2 = 2;
    id = fun x2 -> (
      r = x2
    );
    u2 = id y2
  )
)
"

let m4 = tracelet_map_of_expr e4
let _ = check_point m4 "b"
let _ = check_point m4 "b2"
let _ = check_ids m4 "b" (["z"; "x"], [], ["b2"])
let _ = check_ids m4 "b2" (["z2"; "x2"; "y2"], ["u2"], [])
let _ = check_partial m4 "b2" "u2" (["y2"], [], [])
let _ = check_partial m4 "b" "x" (["z"], [], [])

let export = ()