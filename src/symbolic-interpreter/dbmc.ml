open Batteries
open Odefa_ast
open Ast
open Ast_helper
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
    | Bind_id_value of Relative_stack.t * Ident.t * value
    | Bind_id_id of Relative_stack.t * lookup_stack * lookup_stack
    | Bind_cid_cid of Relative_stack.t * lookup_stack * Relative_stack.t * lookup_stack
    | Bind_this_fun of Relative_stack.t * lookup_stack * Ident.t
    | Bind_id_bop of Relative_stack.t * Ident.t * Ident.t * binary_operator * Ident.t
    | Bind_id_input of Relative_stack.t * Ident.t
    | Concretize of Relative_stack.t 
    | Exclusive of phi list
  [@@deriving show {with_path = false}]
end
open Phi

type def_site =
  | At_clause of tl_clause
  | At_main_first
  | At_fun_para of bool * fun_block
  | At_condition

let lookup_main program x_target =
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
  let map = Tunnel.annotate program x_target in
  let x_first = Ast_helper.first_var program in

  let defined x block = 
    match Tracelet.clause_of_x block x, block.source_block with
    | Some tc, _ -> At_clause tc
    | None, Main mb -> At_main_first
    | None, Fun fb -> At_fun_para (fb.para = x, fb)
    | None, CondChosen cb -> At_condition
    | _, _ -> failwith "defined"
  in

  let rec lookup xs0 block rel_stack =
    let x, xs = List.hd xs0, List.tl xs0 in

    (* x can either be 
       1. the id of the clause, while the stack is singleton or not
       2. the argument of this fun
       3. the argument of furthur funs *)

    (* Inductive on definition site *)
    match defined x block with
    | At_clause tc -> (
        begin
          (* log_clause tc.clause; *)
          let (Clause (Var _, rhs)) = tc.clause in
          match rhs with 
          (* Value Discovery Main *)
          | Value_body v when block.point = id_main -> 
            add_phi (Bind_id_value(rel_stack, x, v))
          | Input_body when block.point = id_main  ->
            add_phi (Bind_id_input(rel_stack, x))

          (* Value Discovery Non-Main *)
          | Value_body v when List.is_empty xs -> 
            add_phi (Bind_id_value(rel_stack, x, v));
            lookup [x_first] block rel_stack
          | Input_body ->
            add_phi (Bind_id_input(rel_stack, x));
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
            add_phi (Bind_id_bop(rel_stack, x, x1, bop, x2));
            lookup (x1::xs) block rel_stack;
            lookup (x2::xs) block rel_stack

          (* Fun Exit *)
          | Appl_body (Var (xf, _), Var (xv, _)) -> (
              lookup [xf] block rel_stack;
              match tc.cat with
              | App funs -> (
                  List.iter (fun fun_id ->
                      let fblock = Ident_map.find fun_id map in
                      let x' = Tracelet.ret_of fblock in
                      let rel_stack' = Relstack.push rel_stack x in
                      add_phi (Bind_cid_cid(rel_stack, [x], rel_stack', [x']));
                      lookup [x'] fblock rel_stack' 
                    ) funs
                )
              | _ -> failwith "fun exit clauses"
            )
          | _ -> failwith "error case for non-fun clauses"
        end
      )

    (* Fun Enter Parameter *)
    | At_fun_para (true, fb) ->
      List.iter (fun callsite -> 
          let callsite_block, tc = block_with_clause_of_id map callsite in
          log_clause tc.clause;
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let rel_stack' = Relstack.co_pop rel_stack x' in
            add_phi (Bind_cid_cid (rel_stack, x::xs, rel_stack', x'''::xs));
            add_phi (Bind_this_fun (rel_stack, [x''], block.point));
            lookup [x''] callsite_block rel_stack';
            lookup (x'''::xs) callsite_block rel_stack'
          | _ -> failwith "incorrect callsite in fun para"
        )
        [List.hd fb.callsites]

    (* Fun Enter Non-Local *)
    | At_fun_para (false, fb) ->
      List.iter (fun callsite -> 
          let callsite_block, tc = block_with_clause_of_id map callsite in
          log_clause tc.clause;
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let rel_stack' = Relstack.co_pop rel_stack x' in
            add_phi (Bind_cid_cid (rel_stack, x::xs, rel_stack', x'''::xs));
            add_phi (Bind_this_fun (rel_stack, [x''], block.point));
            lookup [x''] callsite_block rel_stack';
            lookup (x''::x::xs) callsite_block rel_stack'
          | _ -> failwith "incorrect callsite in fun non-local"
        )
        [List.hd fb.callsites]

    (* Cond Top *)

    (* Cond Bottom *)
    | At_main_first
    | At_condition -> 
      failwith "def_at"
  in
  let block0 = Tracelet.take_until x_target map in
  lookup [x_target] block0 Relstack.empty_relstk;
  !phi_set
