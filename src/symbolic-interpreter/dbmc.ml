open Batteries
open Odefa_ast
open Ast
(* 
open Ast_helper
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
    | Bind_id_id of Relative_stack.t * lookup_stack * Relative_stack.t * lookup_stack
    | Bind_this_fun of Relative_stack.t * Ident.t * Ident.t
    | Bind_id_bop of Relative_stack.t * Ident.t * Ident.t * binary_operator * Ident.t
    | Bind_id_input of Relative_stack.t * Ident.t
    | And of phi * phi
    | Concretize of Relative_stack.t 
    | Exclusive of phi list
  [@@deriving show {with_path = false}]
end

module SMT_Phi = struct
  open Interpreter_types
  open Constraint
  open Phi
  let ground_true = Constraint_value(SpecialSymbol SSymTrue, Bool true)

  let smt_value_of_value = function
    | Value_int n -> Constraint.Int n
    | Value_bool b -> Constraint.Bool b
    | Value_function _ 
    | Value_record _ -> failwith "smt_value"

  let sym_of_id_stk x stk = Symbol(x, stk)

  let rec smt_phi_of_phi = function
    | Bind_id_value (stk, x, v) -> Constraint_value (Symbol(x, stk), smt_value_of_value v)
    | Bind_id_input (stk, x) -> Constraint_alias (Symbol(x, stk), Symbol(x, stk))
    | Bind_id_bop (stk, y, x1, op, x2) -> Constraint_binop (Symbol(y, stk), Symbol(x1, stk), op, Symbol(x2, stk))
    | Bind_this_fun (stk, x, f) -> Constraint_alias (Symbol(x, stk), Symbol(f, Relative_stack.empty))
    | Bind_id_id (s1, xs1, s2, xs2) -> Constraint_ids (s1, xs1, s2, xs2)
    | And (p1, p2) -> Constraint_and (smt_phi_of_phi p1, smt_phi_of_phi p2)
    | Concretize stk -> Constraint_stack (Relative_stack.stackize stk)
    | Exclusive phis -> Constraint_exclusive (List.map smt_phi_of_phi phis)

  type choices = Ident_set.t list

end

open Phi
type def_site =
  | At_clause of tl_clause
  | At_fun_para of bool * fun_block
  | At_condition of cond_running_block

let lookup_main program x_target =
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
  let map = Tunnel.annotate program x_target in
  let x_first = Ast_helper.first_var program in

  let defined x block = 
    match Tracelet.clause_of_x block x, block.source_block with
    | Some tc, _ -> At_clause tc
    | None, Main mb -> failwith "no x in main block"
    | None, Fun fb -> At_fun_para (fb.para = x, fb)
    | None, CondChosen cb -> At_condition cb
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
          let (Clause (Var (x_def, _), rhs)) = tc.clause in
          match rhs with 
          (* Value Discovery Main *)
          | Value_body v when block.point = id_main -> 
            (match v with
             | Value_function vf -> ()
             | _ -> add_phi (Bind_id_value(rel_stack, x, v))
            );
            add_phi (Concretize rel_stack)
          | Input_body when block.point = id_main  ->
            add_phi (Bind_id_input(rel_stack, x));
            add_phi (Concretize rel_stack)

          (* Value Discovery Non-Main *)
          | Value_body v when List.is_empty xs ->
            (match v with
             | Value_function vf -> add_phi (Bind_this_fun(rel_stack, x, x_def))
             | _ -> add_phi (Bind_id_value(rel_stack, x, v))
            );
            lookup [x_first] block rel_stack
          | Input_body ->
            add_phi (Bind_id_input(rel_stack, x));
            lookup [x_first] block rel_stack

          (* Value Discard *)
          | Value_body(Value_function f) -> 
            add_phi (Bind_id_id(rel_stack, x::xs, rel_stack, xs));
            lookup xs block rel_stack

          (* Alias *)
          | Var_body (Var (x', _)) ->
            add_phi (Bind_id_id(rel_stack, x::xs, rel_stack, x'::xs));
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
                  let phis = List.map (fun fun_id ->
                      let fblock = Ident_map.find fun_id map in
                      let x' = Tracelet.ret_of fblock in
                      let rel_stack' = Relstack.push rel_stack x in
                      lookup [x'] fblock rel_stack';
                      And (
                        Bind_id_id(rel_stack, [x], rel_stack', [x']),
                        Bind_this_fun (rel_stack, xf, fblock.point)
                      )
                    ) funs in
                  add_phi (Exclusive phis)
                )
              | _ -> failwith "fun exit clauses"
            )
          (* Cond Bottom *)
          | Conditional_body (Var (x', _), e1, e2) -> (
              lookup [x'] block rel_stack;
              let cond_tracelet = Ident_map.find x' map in
              let cond_block = match cond_tracelet.source_block with
                | CondChosen c -> c
                | _ -> failwith "block in conditional_body"
              in
              let phis = List.map (fun beta ->
                  let ctracelet = 
                    if beta = cond_block.choice then
                      cond_tracelet
                    else
                      let flipped_block : cond_running_block = 
                        { cond_block with 
                          choice = not cond_block.choice;
                          block = cond_block.other_block;
                          other_block = cond_block.block
                        } in
                      {
                        cond_tracelet with
                        source_block = CondChosen flipped_block
                      } 
                  in
                  let x_ret = Tracelet.ret_of ctracelet in 
                  lookup [x_ret] ctracelet rel_stack;
                  And (
                    Bind_id_value(rel_stack, x', Value_bool beta),
                    Bind_id_id (rel_stack, [x], rel_stack, [x_ret])
                  )
                )
                  [true; false] in
              add_phi (Exclusive phis)
            )
          | _ -> failwith "error clause cases"
        end
      )

    (* Fun Enter Parameter *)
    | At_fun_para (true, fb) ->
      let phis = List.map (fun callsite -> 
          let callsite_block, tc = block_with_clause_of_id map callsite in
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let rel_stack' = Relstack.co_pop rel_stack x' in
            lookup [x''] callsite_block rel_stack';
            lookup (x'''::xs) callsite_block rel_stack';
            And (
              Bind_id_id (rel_stack, x::xs, rel_stack', x'''::xs),
              Bind_this_fun (rel_stack', x'', block.point)
            )
          | _ -> failwith "incorrect callsite in fun para"
        ) fb.callsites in
      add_phi (Exclusive phis)

    (* Fun Enter Non-Local *)
    | At_fun_para (false, fb) ->
      let phis = List.map (fun callsite -> 
          let callsite_block, tc = block_with_clause_of_id map callsite in
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let rel_stack' = Relstack.co_pop rel_stack x' in
            lookup [x''] callsite_block rel_stack';
            lookup (x''::x::xs) callsite_block rel_stack';
            And (
              Bind_id_id (rel_stack, x::xs, rel_stack', x'''::xs),
              Bind_this_fun (rel_stack, x'', block.point)
            )
          | _ -> failwith "incorrect callsite in fun non-local"
        )
          fb.callsites in
      add_phi (Exclusive phis)

    (* Cond Top *)
    | At_condition cb -> 
      let condsite_block = Ident_map.find block.outer_point map in
      let x2 = cb.cond in
      add_phi (Bind_id_value(rel_stack, x2, Value_bool cb.choice));
      lookup [x2] condsite_block rel_stack;
      lookup (x::xs) condsite_block rel_stack

  in
  let block0 = Tracelet.take_until x_target map in
  lookup [x_target] block0 Relstack.empty_relstk;
  !phi_set
