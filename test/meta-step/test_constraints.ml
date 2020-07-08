open Batteries

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter
open Odefa_symbolic_interpreter.Middle_step

open Program_samples

let print_constraints cs = 
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list Constraint.pp) cs)

let test_shortest e pt =
  let m, f = Tunnel.run_shortest e (Ident pt) in
  let c = Tunnel.get_shortest_clauses m f in
  print_constraints c

;;
(* test_shortest e1 "t";; *)

test_shortest e3 "t";;

test_shortest e4 "z";;

test_shortest e6 "x";;



let export = ()


