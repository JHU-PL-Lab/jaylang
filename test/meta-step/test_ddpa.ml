open Batteries

open Odefa_ast
open Ast

open Odefa_ddpa
open Odefa_symbolic_interpreter.Mega_step
(* open Tracelet *)
open Tunnel

open Program_samples

let conf : (module Ddpa_context_stack.Context_stack) = 
  (module Ddpa_single_element_stack.Stack)

let cfg_of e =
  let module Stack = (val conf) in
  let module Analysis = Ddpa_analysis.Make(Stack) in
  e
  |> Analysis.create_initial_analysis
  |> Analysis.perform_full_closure
  |> Analysis.cfg_of_analysis

let e1 = parse "
b = fun t -> (
  a = 1
);
t = 1
"

let g1 = cfg_of e1

let map1 = pred_map e1 g1 (Ident "t")

(* inner variant
   a `block` is for a single block
   a tracelet may contains one or more blocks

   we need to separate when a block is needed and when a tracelet is needed.
   It's natural that we need a tracelet since we are back tracing.
   Then, we need to remember which block (then/else) we are using.

   Does the first_id of a fun block or a cond block differ?
   Yes, because how it exit from the top is difference.
   Thus, a block is not an atomic element, at least for tracing.

   Whenever an id is given, the tracelet can handle the block.

   Whenever we first get a tracelet from an id and then ask for block info 
   of the tracelet, we lost the id, which means we don't know then/else.

*)

(* 
block is different, however, the source of difference is the tracelet


static tracelet
dynamic tracelet
- true/false
- (potential) cache
- heuristics


(* e1 *)
find_tracelet m "a" = "b"
find_tracelet m "t" = "main0"

(* e2 *)
find_tracelet m "z" = "b1"
find_tracelet m "b2" = "b1"
find_tracelet m "a" = "b2"
find_tracelet m "b" = "b1"

(* e3 *)
find_tracelet m "z" = "b"
find_tracelet m "b" = "w1"
find_tracelet m "w1" = "w2"

(* e4 *)
find_tracelet m "r" = "id"
find_tracelet m "id" = "b2"

find_tailored_tracelet = 
| Partial -> 
 *)

(* type source_block = 
   | WholeFun of block
   | PartialFun of block
   | WholeCond of 
   | PartialCond of block *)

(* plain source -> tracelet_map *)
(* analysis -> annonataion tracelet *)
(* running -> dynamic tracelet *)

let export = ()