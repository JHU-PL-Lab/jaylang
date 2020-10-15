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

let test_choice choices e pt =
  let oracle = make_list_oracle choices in
  test_constraints oracle e pt

let test_naive choices e pt =
  let oracle = make_list_oracle choices in
  let oracle', m, f = Tunnel.run_deterministic oracle e (Ident pt) in
  print_trace @@ ident_list_of_trace f m;
  let c = Tunnel.naive_eval oracle' m f in
  print_constraints c;
  try
    let s = List.fold_left (fun s c -> Solver.add c s) Solver.empty c in
    match Solver.solve s with
    | _ -> print_endline "SAT"
  with
  | _ ->
    print_endline "UNSAT"
;;
(* test_shortest e1 "t";;

   test_shortest e3 "t";;

   test_shortest e4 "z";;

   test_shortest e6 "x";;

   test_shortest e7 "x";;

   test_shortest e8 "rf";;

   test_choice [] e9 "rf";;

   test_choice [1] e9 "rf";;

   test_choice [2] e9 "rf";;

   test_choice [] e10 "target";;

   test_choice [0] e10_2 "target";;

   test_choice [1] e10_2 "target";;

*)

(* test_choice [] e11 "target";;

   test_choice [1] e11 "target";;

   test_choice [] e12 "target";;



   test_choice [0; 1] e19 "target";;

   test_choice [1; 1] e19 "target";; *)

(* test_naive [] e21 "target";; *)

(* tf *)
(* test_choice [2] e19_2 "target";;  *)

(* th *)
(* test_choice [1] e19_2 "target";; *)

(* tg *)
(* test_choice [0] e19_2 "target";; *)

(* test_shortest e18 "r2";;

   test_choice [1] e18 "r2";; *)

;;
open Naive

let test_walk program oracle =
  let pt = BatOption.get oracle.x_to in
  let map = Tunnel.annotate program pt in
  let cs = Naive.walk oracle map program Tunnel.empty_relstk in
  print_constraints cs;
  try
    let s = List.fold_left (fun s c -> Solver.add c s) Solver.empty cs in
    match Solver.solve s with
    | _ -> print_endline "SAT"
  with
  | _ ->
    print_endline "UNSAT"
;;

test_walk
  e22
  ({block_id = Ident "g"; 
    x_to = Some (Ident "target"); 
    path = CallOut (Ident "three");
    inner = [];
    outer = Some
        {block_id = Tracelet.id_main;
         x_to = Some (Ident "b1");
         path = Main;
         inner = [
           {block_id = Ident "f";
            x_to = None;
            path = CallIn;
            inner = [];
            outer = None};
           {block_id = Ident "f";
            x_to = None;
            path = CallIn;
            inner = [];
            outer = None}
         ];
         outer = None}
   }) ;;

(* 
test_walk
  e1 
  ({block_id = Tracelet.id_main; 
    x_to = Some (Ident "t"); 
    path = Main;
    inner = []; 
    outer = None}) ;;

test_walk
  e3
  ({block_id = Ident "w2"; 
    x_to = Some (Ident "t");
    path = Main;
    inner = [];
    outer = Some {block_id = Tracelet.id_main; 
                  x_to = Some (Ident "w2"); 
                  path = Choice false;
                  inner = []; 
                  outer = None}}) ;;

test_walk
  e3
  ({block_id = Ident "w2"; 
    x_to = Some (Ident "w1"); 
    path = Main;
    inner = [];
    outer = Some {block_id = Tracelet.id_main; 
                  x_to = Some (Ident "w2"); 
                  path = Choice true;
                  inner = []; 
                  outer = None}}) ;;

test_walk
  e6 
  ({block_id = Tracelet.id_main; 
    x_to = Some (Ident "x"); 
    path = Main;
    inner = [ oracle_of_naive_call "f" ]; 
    outer = None}) ;;

test_walk
  e7
  ({block_id = Tracelet.id_main; 
    x_to = Some (Ident "p2"); 
    path = Main;
    inner = [ 
      {block_id = Ident "z";
       x_to = None;
       path = CallIn;
       inner = [
         {block_id = Ident "f";
          x_to = None;
          path = CallIn;
          inner = [];
          outer = None}
       ];
       outer = None
      }
    ]; 
    outer = None}) ;;

test_walk
  e7
  ({block_id = Ident "z"; 
    x_to = Some (Ident "x"); 
    path = CallOut (Ident "u");
    inner = [ 
      {block_id = Ident "f";
       x_to = None;
       path = CallIn;
       inner = [];
       outer = None}
    ];
    outer = Some
        {block_id = Tracelet.id_main;
         x_to = Some (Ident "p");
         path = Main;
         inner = [];
         outer = None}
   }) ;; *)

let export = ()


