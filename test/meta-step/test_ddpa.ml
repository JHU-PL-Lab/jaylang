open Batteries

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter.Mega_step
open Tracelet

open Program_samples

let print_info def_id_info = 
  let entries = 
    List.map (fun (id, dsts) -> 
        let dst_s = String.concat ", " dsts in
        id ^ ": " ^ dst_s)
      def_id_info in
  print_endline @@ String.concat "\n" entries

let eq_id_def def1 def2 =
  List.for_all
    (fun (id, dst1) -> 
       match List.assoc_opt id def2 with
       | Some dst2 -> List.for_all (fun dst -> List.mem dst dst2) dst1
       | None -> List.is_empty dst1
    )
    def1

let debug_print_info tl_name map expected =
  let id_dst = 
    Ident_map.find (Ident tl_name) map
    |> debug_def_ids_of
  in 
  print_info id_dst;
  assert (eq_id_def id_dst expected)

let m6 = Tunnel.annotate e6 (Ident "x")
;;
debug_print_info name_main m6 [("a", ["f"])];;

let m7 = Tunnel.annotate e7 (Ident "x")
;;
debug_print_info name_main m7 [("p", ["z"])];;
debug_print_info "z" m7 [("a", ["f"])];;

(* let m8 = Tunnel.annotate e8 (Ident "rf") *)

(* let map1 = pred_map e1 g1 (Ident "t") *)

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