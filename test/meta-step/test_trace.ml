open Batteries

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter.Middle_step
open Tracelet
open Tunnel
open Oracle

open Program_samples
open Utils

let test_shortest_trace e pt expected =
  let map, trace = run_shortest e (Ident pt) in
  let trace = ident_list_of_trace trace map in
  (* print_trace trace; *)
  let trace_name = List.map (fun (Ident name) -> name) trace in
  assert (trace_name = expected);;

let test_chosen_trace e pt choices expected =
  let oracle = make_list_oracle choices in
  let _, map, trace = run_deterministic oracle e (Ident pt) in
  let trace = ident_list_of_trace trace map in
  (* print_trace trace; *)
  let trace_name = List.map (fun (Ident name) -> name) trace in
  assert (trace_name = expected);;

test_shortest_trace e1 "t" [name_main; "t"];;

test_shortest_trace e3 "w1" [name_main; "w2"; "w1"];;

test_shortest_trace e4 "x" [name_main; "b"; "x"];;

test_shortest_trace e5 "h" [name_main; "b2"; "g"; "h"];;

test_shortest_trace e6 "rf" [name_main; "a"; "f"; "rf"];;

test_shortest_trace e7 "rf" [name_main; "p"; "z"; "a"; "f"; "rf"];;

test_shortest_trace e8 "rf" [name_main; "c2"; "f"; "rf"];;

test_shortest_trace e9 "rf" [name_main; "c3"; "f"; "rf"];;

test_shortest_trace e10 "target" [name_main; "target"];;

test_shortest_trace e11 "target" [name_main; "b"; "target"];;

test_shortest_trace e11_2 "target" [name_main; "b"; "target"];;

test_shortest_trace e12 "target" [name_main; "b"; "target"];;

test_shortest_trace e13 "target" [name_main; "target"];;

test_shortest_trace e14 "target" [name_main; "target"];;

test_shortest_trace e15 "r1" [name_main; "p"; "g"; "r"; "r1"];;

(* test_shortest_trace e16 "r1" [name_main; "p"; "g"; "r"; "r1"];; *)
test_shortest_trace e16 "r" [name_main; "p"; "g"; "r"];;

test_shortest_trace e17 "r1" [name_main; "p"; "g"; "r"; "r1"];;


test_shortest_trace e18 "r" [name_main; "z"; "f0"; "r"];;

test_shortest_trace e18 "r1" [name_main; "z"; "f0"; "r"; "r1"];;

test_shortest_trace e18 "r2" [name_main; "z"; "f0"; "r"; "r2"];;

test_chosen_trace e18 "r2" [] 
  [name_main; "z"; 
   "f0"; "r"; "r2"];;

test_chosen_trace e18 "r2" [1] 
  [name_main; "z"; 
   "f0"; "r"; "r2"; 
   "f0"; "r"; "r2"];;

test_chosen_trace e18 "r2" [1; 1] 
  [name_main; "z"; 
   "f0"; "r"; "r2"; 
   "f0"; "r"; "r2";
   "f0"; "r"; "r2"];;

test_chosen_trace e18 "r1" [] 
  [name_main; "z"; 
   "f0"; "r"; "r1";];;

test_chosen_trace e18 "r1" [1] 
  [name_main; "z"; 
   "f0"; "r"; "r2";
   "f0"; "r"; "r1";];;

test_chosen_trace e18 "r1" [1; 1] 
  [name_main; "z"; 
   "f0"; "r"; "r2";
   "f0"; "r"; "r2";
   "f0"; "r"; "r1";];;

let export = ()