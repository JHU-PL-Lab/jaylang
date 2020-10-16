open Batteries
open Odefa_ast
open Ast
(* 
   open Odefa_ddpa
   open Ddpa_abstract_ast
   open Ddpa_graph

   open Ddpa_helper

*)

open Middle_step
open Tracelet
module Relstack = Mega_constraint.Relstack

type lookup_stack = Ident.t list 
[@@deriving show {with_path = false}]

module Phi = struct
  open Ast_pp

  type phi =
    | Bind_id_value of Relative_stack.t * lookup_stack * value
    | Bind_id_id of Relative_stack.t * lookup_stack * lookup_stack
    | Bind_id_bop of Relative_stack.t * lookup_stack * binary_operator * lookup_stack
    | Bind_id_input of Relative_stack.t * lookup_stack
    | Concretize of Relative_stack.t 
  [@@deriving show {with_path = false}]
end
open Phi

let lookup_main program x_target =
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
  let map = Tunnel.annotate program x_target in
  let x_first = Ast_helper.first_var program in

  let defined x block = 
    match Tracelet.clause_of_x block x with
    | Some tc -> tc
    | None -> failwith "no such x" in

  let rec lookup xs0 block rel_stack =
    let x_lookup, xs = List.hd xs0, List.tl xs0 in

    (* Inductive on definition site *)
    let c = defined x_lookup block in 
    let (Clause (Var (x, _), rhs)) = c.clause in

    match rhs with
    (* Value Discovery Main *)
    | Value_body(Value_int i) when block.point = id_main -> 
      add_phi (Bind_id_value(rel_stack, [x], (Value_int i)))
    | Value_body(Value_bool b) when block.point = id_main  ->
      add_phi (Bind_id_value(rel_stack, [x], (Value_bool b)))
    | Input_body when block.point = id_main  ->
      add_phi (Bind_id_input(rel_stack, [x]))

    (* Value Discovery Non-Main *)
    | Value_body(Value_int i) -> 
      add_phi (Bind_id_value(rel_stack, [x], (Value_int i)));
      lookup [x_first] block rel_stack
    | Value_body(Value_bool b) ->
      add_phi (Bind_id_value(rel_stack, [x], (Value_bool b)));
      lookup [x_first] block rel_stack
    | Input_body ->
      add_phi (Bind_id_input(rel_stack, [x]));
      lookup [x_first] block rel_stack

    (* Value Discard *)
    | Value_body(Value_function f) -> 
      add_phi (Bind_id_id(rel_stack, x::xs, xs));
      lookup xs block rel_stack

    (* Alias *)
    | Var_body (Var (x', _)) ->
      add_phi (Bind_id_id(rel_stack, x::xs, x'::xs));
      lookup (x'::xs) block rel_stack

    (* Binop *)
    | Binary_operation_body (Var (x1, _), bop, Var (x2, _)) ->
      add_phi (Bind_id_bop(rel_stack, [x1], bop, [x2]));
      lookup (x1::xs) block rel_stack;
      lookup (x2::xs) block rel_stack

    (* Fun Enter Parameter *)

    (* Fun Enter Non-Local *)

    (* Fun Exit *)

    (* Cond Top *)

    (* Cond Bottom *)

    | _ -> ()

  in

  let block0 = Tracelet.take_until x_target map in
  lookup [x_target] block0 Relstack.empty_relstk;
  !phi_set
