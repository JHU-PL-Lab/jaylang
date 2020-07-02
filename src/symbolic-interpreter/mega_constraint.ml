open Batteries

open Odefa_ast
open Ast
open Odefa_ddpa
open Ddpa_abstract_ast
open Ddpa_utils
open Interpreter_types

type search_result = {
  result_sym : symbol;
  constraints : Constraint.t list;
  relstack : Relative_stack.t;
  (* dynamic scoping *)
  env: symbol Ident_map.t;
}


module Relstack = struct
  let empty_relstk = Relative_stack.empty

  open Relative_stack

  let is_stack_empty (Relative_stack(_co_stk, stk)) = 
    List.is_empty stk

  let push (Relative_stack(co_stk, stk)) x : t =
    Relative_stack(co_stk, x :: stk)

  let co_pop (Relative_stack(co_stk, stk)) x : t =
    match stk with
    | x'::stk' ->
      if equal_ident x x' then
        Relative_stack(co_stk, stk')
      else
        failwith "dismatch"
    | [] ->
      Relative_stack(x :: co_stk, stk)
end
open Relstack

let lift_use_symbol f phi =
  let open Constraint in 
  match phi with
  | Constraint_value(s, v) -> Constraint_value(s, v)
  | Constraint_alias(s1, s2) -> Constraint_alias(s1, f s2)
  | Constraint_binop(s1, s2, op, s3) -> Constraint_binop(s1, f s2, op, f s3)
  | Constraint_projection(s1, s2, id) -> Constraint_projection(s1, f s2, id)
  | Constraint_type(s, t) -> failwith "not here"
  | Constraint_stack(cstk) -> Constraint_stack(cstk)

let lift_constraints_env env phis =
  let update_use_symbol = function
    | Symbol(x, stk) -> (
        match Ident_map.Exceptionless.find x env with
        | Some s' -> s'
        | None -> Symbol(x, stk)
      )
    | SpecialSymbol t -> SpecialSymbol t
  in
  List.map (lift_use_symbol update_use_symbol) phis

let join_results ?(left_stack=false) main_rs sub_rs = 
  List.cartesian_product main_rs sub_rs
  |> List.map (fun (main_r, sub_r) -> 
      let sub_phis = sub_r.constraints
      and main_env = main_r.env in
      let sub_phis' = lift_constraints_env main_env sub_phis in
      {main_r with
       constraints = main_r.constraints @ sub_phis'})

let get_value x env =
  match x with 
  | Symbol(x_id, _) -> (
      match Ident_map.find x_id env with
      | Clause(_, Value_body(v)) -> v
      | _ -> failwith "get_value"
    )
  | _ -> failwith "special symbol"

let constraint_of_value x env =
  let rhs = match get_value x env with
    | Value_function f -> Constraint.Function f
    | Value_int n -> Constraint.Int n
    | Value_bool b -> Constraint.Bool b
    | Value_record(Record_value _m) -> failwith "record"
  in
  Constraint.Constraint_value(x, rhs)

let constraint_of_stack stk =
  Constraint.Constraint_stack(Relative_stack.stackize stk)

let constraint_of_input x =
  Constraint.Constraint_binop(
    SpecialSymbol SSymTrue, x, Binary_operator_equal_to, x)

let constraint_of_alias x y =
  Constraint.Constraint_alias(x, y)

let constraint_of_funexit x_accept x_return stk =
  let sym_return = Symbol(x_return, stk) in
  let stk' = co_pop stk x_accept in
  let sym_accept = Symbol(x_accept, stk') in
  constraint_of_alias sym_accept sym_return

let constraint_of_funenter para arg callsite stk =
  let sym_para = Symbol(para, stk) in
  let stk' = co_pop stk callsite in
  let sym_arg =  Symbol(arg, stk') in
  constraint_of_alias sym_para sym_arg

let constraint_of_bool x (b : bool) =
  Constraint.Constraint_value(x, Constraint.Bool b)

let constraint_of_clause clause_mapping first_vav =       
  fun ?(is_demo=false) clause stk env -> 
  let first_var_constraint x = 
    if equal_ident x first_vav then
      [constraint_of_stack stk]
    else
      []
  in

  let def_sym x = Symbol(x, stk)
  and use_sym x = 
    if is_demo then
      Symbol(x, stk)
    else
      Ident_map.find x env in
  match clause with
  (* Input : x == input *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_input_body)) -> 
    (first_var_constraint x) @ [constraint_of_input (def_sym x)]
  (* Alias : x == x' *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_var_body(Abs_var x'))) -> 
    [constraint_of_alias (def_sym x) (use_sym x')]
  (* Binop : x = x' op x'' *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_binary_operation_body(Abs_var x', op, Abs_var x''))) ->
    [Constraint.Constraint_binop((def_sym x), (use_sym x'), op, (use_sym x''))]
  (* Discard / Discovery *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_value_body _)) ->
    (first_var_constraint x) @ [constraint_of_value (def_sym x) clause_mapping]
  (* Callsite . ignored . *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_appl_body _)) ->
    []
  (* Cond_site . ignored . *)
  | Unannotated_clause(Abs_clause(Abs_var x, Abs_conditional_body (Abs_var x1, _e_then, _e_else))) ->
    []
  (* CondTop *)
  | Nonbinding_enter_clause(Abs_value_bool b, Abs_clause(_, Abs_conditional_body(Abs_var x1, _e_then, _e_else))) ->
    (* record b *)
    (* ignore @@ failwith "where"; *)
    [constraint_of_bool (def_sym x1) b]
  (* CondBtmTrue / CondBtmFalse *)
  | Binding_exit_clause(Abs_var outer_var, Abs_var ret_var, Abs_clause(_, Abs_conditional_body(Abs_var x1, e_then, e_else))) ->
    let Abs_var x1ret = retv e_then
    and Abs_var x2ret = retv e_else in
    if equal_ident ret_var x1ret then
      [constraint_of_bool (def_sym x1) true ; (constraint_of_alias (def_sym outer_var) (def_sym x1ret))]
    else if equal_ident ret_var x2ret then
      [constraint_of_bool (def_sym x1) false; (constraint_of_alias (def_sym outer_var) (def_sym x2ret))]
    else
      assert false
  (* FunEnter / FunEnterNonLocal *)
  (* 
        f = fun para -> ( 
          r = 3
        );
        arg = 1;
        er = f arg;
      *)
  (* 
        if equal_ident para x_target then
          (* FunEnter *)
          (* xf (for constraits) *)
          (* relstack pop *)
          arg
        else
          (* FunEnterNonLocal : *)
          (* c <- xf *)
          x_target
      *)
  | Binding_enter_clause(Abs_var para, Abs_var arg, Abs_clause(Abs_var er, Abs_appl_body(Abs_var e1, Abs_var e2))) ->
    [constraint_of_funenter para arg er stk]
  (* if equal_ident para x_target then
     (* FunEnter *)
     (* xf (for constraits) *)
     (* relstack pop *)
     []
     else
     (* FunEnterNonLocal : *)
     (* c <- xf *)
     [] *)
  (* FunExit *)
  | Binding_exit_clause(Abs_var _para, Abs_var ret_var, Abs_clause(Abs_var er, Abs_appl_body(Abs_var _xf, _))) ->
    [constraint_of_funexit er ret_var stk]
  | Start_clause _ | End_clause _ ->
    []
  | _ ->
    failwith "else in constraint"



module Memoized = struct
  (* what is the key to the step?
     for a full block, we could use End(x) or Start(x).
     for a partial block (when returning from another block), the returning point is where 
     we should use as a key.
     ```
      f = fun x -> ...target...
      a = 1
      t1 = f a
      t2 = f a
     ```

     Admitted that
     1. starting from cfg rather than ddpa_cfg may be better
     2. there may be duplication e.g. `a=1`
  *)

  type mega_step = {
    (* x = a *)
    plain : Constraint.t list;
    (* x = e1 e2 *)
    cond_indirect : Ident.t list;
    (* x = c ? e1 : e2 *)
    fun_indirect : Ident.t list;
    (* dangling CondTop/FunEnter *)
    continue_indirect : Ident.t list;
  }

  (* a placeholder value which will be set immediately *)
  let empty_id = Ident("_")

  let empty_step_info = {
    plain = [];
    cond_indirect = [];
    fun_indirect = [];
    continue_indirect = [];
  }

  (* 
    It's actually a new cfg with meta steps
    Node: program point
    - plain is the value in the Node
    Edge: possible search move
    - Cond_indirect: 
    - Fun_indirect:
    - Continue_inriect: 

    Noting we can have soundness even with meta step.
    ```
      x = 1
      y = +inf
      target = x
    ```
    we will have sth like [x[]=1;target[]=x[]; y->Indirect].
    Unless we can expand y to values without Indirect, we won't finish the constraint generation.
     *)


  (* 
      key: start from this program point
      value: immediate phis and other phis 
      The whole map works as a mega_step for search.
      *)
  type meta_step_map = mega_step Ident_map.t
end

(* TODO: 
    how may it allow optimization later.
    we need a flow-sensitive mixture cond_block with condition, which helps to
    escape from impossible traces
*)

