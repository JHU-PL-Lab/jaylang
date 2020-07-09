open Batteries

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter
open Odefa_symbolic_interpreter.Middle_step
open Oracle

open Program_samples
open Utils

let print_constraints cs = 
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list Constraint.pp) cs)

let test_constraints oracle e pt =
  let oracle', m, f = Tunnel.run_deterministic oracle e (Ident pt) in
  print_trace @@ ident_list_of_trace f m;
  let _, c = Tunnel.gen_clauses_frames oracle' m f in
  print_constraints c;
  try
    let s = List.fold_left (fun s c -> Solver.add c s) Solver.empty c in
    match Solver.solve s with
    | _ -> print_endline "SAT"
  with
  | _ ->
    print_endline "UNSAT"

let test_shortest e pt =
  let shortest_oracle = make_list_oracle [] in
  test_constraints shortest_oracle e pt
;;

let test_choice choices e pt =
  let oracle = make_list_oracle choices in
  test_constraints oracle e pt
;;

(* 
let test_shortest e pt =
  let m, f = Tunnel.run_shortest e (Ident pt) in
  print_trace @@ ident_list_of_trace f m;
  let c = Tunnel.gen_shortest_clauses_frames m f in
  print_constraints c;
  try
    let s = List.fold_left (fun s c -> Solver.add c s) Solver.empty c in
    match Solver.solve s with
    | _ -> print_endline "SAT"
  with
  | _ ->
    print_endline "UNSAT"

;; *)
(* test_shortest e1 "t";; *)

test_shortest e3 "t";;

test_shortest e4 "z";;

test_shortest e6 "x";;



let export = ()


