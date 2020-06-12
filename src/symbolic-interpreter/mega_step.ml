open Batteries
(* open Jhupllib *)
open Odefa_ast
open Odefa_ddpa

open Ast;;
(* open Ast_pp *)
open Ddpa_abstract_ast
(* open Ddpa_graph *)
open Ddpa_utils
open Interpreter_types;;
(* open Logger_utils *)

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



(* let lift_stack stk result = 
   let new_sym = lift_stack_in_symbol stk result.result_sym in
   {
    result_sym = new_sym;
    result_clause = result.result_clause;
    constraints = lift_stack_in_constraints (lift_stack_in_symbol stk) result.constraints;
   } *)

(* let lift_constraints cond_constraints rs =
   List.map (fun r -> { r with constraints = 
    r.constraints @ cond_constraints}) rs *)

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

let heading_var_of_clause = function
  | Unannotated_clause(Abs_clause(Abs_var x, _)) -> Some x
  | Nonbinding_enter_clause(_, Abs_clause(Abs_var x, Abs_conditional_body(_, _, _))) -> Some x
  | Binding_enter_clause(_, _, Abs_clause(Abs_var x, Abs_appl_body(Abs_var _, _))) -> Some x
  | _ -> None

let heading_var_of_clause_exn clause = 
  match heading_var_of_clause clause with
  | Some x -> x
  | None -> failwith "must have a var"

let has_condition_clause clauses =
  List.exists (function 
      (* main condition exp *)
      | Unannotated_clause(Abs_clause(Abs_var _, Abs_conditional_body _)) -> true
      | _ -> false
    ) clauses

let has_application_clause clauses =
  List.exists (function 
      (* callsite exp *)
      | Unannotated_clause(Abs_clause(Abs_var _, Abs_appl_body _)) -> true
      | _ -> false
    ) clauses

let all_fun_start_clauses clauses =
  (* FunTop exp *)
  List.for_all (function
      | Binding_enter_clause(Abs_var _, Abs_var _, _) -> true
      | _ -> false
    ) clauses

(* This is acturally an analysis-to-symbolic-execution adapting function,
   with which the symbolic execution can decouple with the analysis.
   It makes the analysis configurable to the symbolic execution.
*)
(* 
exising
  clause_map:       id -> clause
  heavily used

  clause_pred_map:  id -> clause
  never used

  func_para_map:    para:id -> fun
  used in FunEnter rule

  fun_ret_map:      ret:id -> fun
  used in FunReturn rule

  first_var:        id
  used in several rules

  hybrid_table:     id -> value
 *)

let rec enum_all_functions_in_expr expr : function_value Enum.t =
  let Expr(clauses) = expr in
  Enum.concat @@ Enum.map enum_all_functions_in_clause @@ List.enum clauses
and enum_all_functions_in_clause clause : function_value Enum.t =
  let Clause(_,body) = clause in
  enum_all_functions_in_body body
and enum_all_functions_in_body body : function_value Enum.t =
  match body with
  | Value_body v ->
    enum_all_functions_in_value v
  | Var_body _
  | Input_body
  | Appl_body _
  | Binary_operation_body (_, _, _) ->
    Enum.empty ()
  | Conditional_body (_, e1, e2) ->
    Enum.append
      (enum_all_functions_in_expr e1) (enum_all_functions_in_expr e2)
  | Match_body (_, _)
  | Projection_body (_, _) ->
    Enum.empty ()
and enum_all_functions_in_value value : function_value Enum.t =
  match value with
  | Value_record _ -> Enum.empty ()
  | Value_function(Function_value(_,e) as f) ->
    Enum.append (Enum.singleton f) @@ enum_all_functions_in_expr e
  | Value_int _
  | Value_bool _ -> Enum.empty ()

(* is this the same as Ast_tools.flatten 
   No. expr_flatten returns expr list
   while flatten return clause list.
*)
let rec expr_flatten ((Expr clauses) as expr) : expr list =
  expr ::
  (clauses
   |>
   List.map
     (fun (Clause(_,b)) ->
        match b with
        | Value_body (Value_function(Function_value(_,e))) -> expr_flatten e
        | Value_body _
        | Var_body _
        | Input_body
        | Appl_body (_, _)
        | Match_body (_, _)
        | Projection_body (_, _)
        | Binary_operation_body (_, _, _) -> []
        | Conditional_body (_, e1, e2) ->
          e1 :: e2 :: expr_flatten e1 @ expr_flatten e2
     )
   |> List.concat
  )

let clause_predecessor_mapping e =
  e 
  |>expr_flatten
  |> List.enum
  |> Enum.map
    (fun (Expr clauses) ->
       let c1 = List.enum clauses in
       let c2 = List.enum clauses in
       Enum.drop 1 c1;
       Enum.combine (c1,c2)
       |> Enum.map
         (fun (Clause(Var(x,_),_),clause) -> (x,clause))
    )
  |> Enum.concat
  |> Ident_map.of_enum

let clause_mapping e =
  e
  |> Ast_tools.flatten
  |> List.enum
  |> Enum.fold
    (fun map (Clause(Var(x,_),_) as c) ->
       Ident_map.add x c map
    )
    Ident_map.empty

let first_var e =
  e
  |> (fun (Expr cls) -> cls)
  |> List.first
  |> (fun (Clause(Var(x,_),_)) -> x)


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
module Tracelet = struct

  let name_main = "0_main"

  let id_main = Ident name_main

  type trace_clause = 
    | App | Fun | Cond | Direct of clause

  type block = {
    clauses : (ident * trace_clause) list;
    app_ids : ident list;
    cond_ids : ident list;
  }

  let empty_block = {
    clauses = []; app_ids = []; cond_ids = []
  }

  type blocks = 
    | WholeFun of block
    | PartialFun of block
    | WholeCond of { 
        cond : ident;
        then_block : block;
        else_block : block;
      }
    | PartialCond of block

  type t = {
    point : ident;
    (* ret : ident *)
    next : ident list;
    blocks : blocks
  }

  let iter_block tracelet f =
    match tracelet with
    | WholeFun b -> f b
    | PartialFun b -> f b
    | WholeCond cb -> (f cb.then_block); (f cb.else_block)
    | PartialCond b -> f b

  let either_block tracelet f =
    match tracelet with
    | WholeFun b -> f b
    | PartialFun b -> f b
    | WholeCond cb -> (f cb.then_block) || (f cb.else_block)
    | PartialCond b -> f b

  let list_in_block blocks f =
    match blocks with
    | WholeFun b -> f b
    | PartialFun b -> f b
    | WholeCond cb -> (f cb.then_block) @ (f cb.else_block)
    | PartialCond b -> f b

  let cid_of_tracelet tl =
    let cids_of_block block = 
      List.map (fun (Ident id, _) -> id) block.clauses in
    list_in_block tl.blocks cids_of_block

  let direct_cid_of_tracelet tl =
    let cids_of_block block = 
      List.filter_map  (function
          | (Ident id, Direct _) -> Some id
          | _ -> None) 
        block.clauses in
    list_in_block tl.blocks cids_of_block

  let split_clauses (Expr clauses) : (ident * trace_clause) list * ident list * ident list =
    List.fold_left (fun (cs, app_ids, cond_ids) (Clause(Var (cid, _), b) as c) ->
        match b with
        | Appl_body (_, _)
          -> (cs @ [cid, App], app_ids @ [cid], cond_ids)
        | Conditional_body (Var (x, _), _, _)
          -> (cs @ [cid, Cond], app_ids, cond_ids @ [cid])
        | Value_body (Value_function _)
          -> (cs @ [cid, Fun], app_ids, cond_ids)
        | _
          -> (cs @ [cid, Direct c], app_ids, cond_ids)

      ) ([], [], []) clauses

  let block_of_expr e =
    let clauses, app_ids, cond_ids = split_clauses e in
    { clauses; app_ids; cond_ids }

  let tracelet_map_of_expr e : t Ident_map.t =
    let map = ref Ident_map.empty in
    (* add main block *)
    let block = block_of_expr e in
    let blocks = WholeFun block in
    let tracelet = 
      { point = id_main ; next = []; blocks } in
    map := Ident_map.add id_main tracelet !map
    ;
    (* add nested blocks *)
    e
    |> Ast_tools.flatten
    |> List.enum
    |> Enum.iter (function 
        | Clause (Var (cid, _), Value_body (Value_function (Function_value (_arg, fbody)))) ->
          let block = block_of_expr fbody in
          let blocks = WholeFun block in
          let tracelet = 
            { point = cid ; next = []; blocks } in
          map := Ident_map.add cid tracelet !map
        | Clause (Var (cid, _), Conditional_body (Var(cond, _), e1, e2)) ->
          let then_block = block_of_expr e1
          and else_block = block_of_expr e2 in
          let blocks = 
            WholeCond {cond; then_block; else_block} in
          let tracelet = 
            { point = cid ; next = []; blocks } in
          map := Ident_map.add cid tracelet !map      
        | _ -> ())
    ;
    !map

  let pred_ids_of_block (x : ident) block : (ident * trace_clause) list * ident list * ident list =
    if List.exists (fun (cid, _c) -> cid = x) block.clauses then
      List.fold_while 
        (fun _acc (cid, _c) -> cid <> x)
        (fun (cs, app_ids, cond_ids) (cid, c) ->
           match c with
           | App ->
             (cs, app_ids @ [cid], cond_ids)
           | Fun ->
             (cs, app_ids, cond_ids)
           | Cond ->
             (cs, app_ids, cond_ids @ [cid])
           | Direct c ->
             (cs @ [cid, Direct c], app_ids, cond_ids)
        )
        ([], [], [])
        block.clauses
      |> fst
    else
      ([], [], [])

  let debug_pred_ids_of_block x block =
    let direct_ids, app_ids, cond_ids = pred_ids_of_block x block in
    let direct_names = List.map (fun (Ident name, _) -> name) direct_ids
    and app_names = List.map (fun (Ident name) -> name) app_ids
    and cond_names = List.map (fun (Ident name) -> name) cond_ids in
    (direct_names, app_names, cond_names)

  let debug_pred_ids_of_tracelet x tracelet =
    match tracelet.blocks with
    | WholeFun b -> debug_pred_ids_of_block x b
    | PartialFun b -> debug_pred_ids_of_block x b
    | WholeCond cb -> (
        let d1, a1, c1 = debug_pred_ids_of_block x cb.then_block
        and d2, a2, c2 = debug_pred_ids_of_block x cb.else_block
        in
        ( d1 @ d2, a1 @ a2, c1 @ c2 )
      )
    | PartialCond b -> debug_pred_ids_of_block x b

end

(* open Tracelet *)



(* 
annotated_clause

initial acl

Unannotated_clause(
  lift_clause @@ Ident_map.find program_point env.le_clause_mapping)


usage of acl

let%bind acl1 = pick @@ preds acl0 env.le_cfg in

acl |> x |> pattern match
 *)

(* ddpa workflow

   Make(Context_stack) -> Analysis_sig

   Context_stack =
   | n_elem | non_repeating | single_elem | two_elem | unit_stack


   create_initial_analysis : expr -> ddpa_alys
    => Abs_expr(cls) = lift_expr e

   perform_closure_steps : ddpa_alys -> ddpa_alys
    => ...

   perform_full_closure : ddpa_alys -> ddpa_alys
    recursively call perform_closure_steps until `is_fully_closed`



   cfg_of_analysis : ddpa_alys -> ddpa_graph

   type ddpa_analysis = {
    { ddpa_graph : ddpa_graph
    ; ddpa_graph_fully_closed : bool
    ; pds_reachability : Ddpa_pds_reachability.analysis
    ; ddpa_active_nodes : Annotated_clause_set.t
    ; ddpa_active_non_immediate_nodes : Annotated_clause_set.t
    ; ddpa_logging_data : ddpa_analysis_logging_data option
    }
   }

*)

(* outside
   sym_interpreter.ml:

   start : ddpa_graph -> expr -> ident --> evaluation
      => prepare_environment : expr -> ddpa_graph --> lookup_environment

    lookup: lookup_environment -> lookup_stack -> annotated_clause -> relstack -> symbol M.monad

   lookup :
    let%bind acl1 = pick @@ preds acl0 env.le_cfg in

   generator.ml:

   create : policy -> conf -> expr -> Id --> test_generator
      => cfg = e 
              |> create_initial_analysis
              |> perform_full_closure
              |> cfg_of_analysis
      => start
*)

