open Batteries
(* open Jhupllib *)
open Odefa_ast
open Odefa_ddpa

open Ast;;
(* open Ast_pp *)
open Ddpa_abstract_ast
open Ddpa_graph
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

open Ast_helper

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

  (* TODO:
     we might get rid of map at all, by using tree (zipper)
  *)
  let name_main = "0_main"

  let id_main = Ident name_main

  (* duplicate for simplicity *)
  type trace_clause = 
    | App | Fun | Cond | Direct of clause

  type id_with_dst = ident * ident list

  type block = {
    clauses : (ident * trace_clause) list;
    app_ids : id_with_dst list;
    cond_ids : id_with_dst list;
  }

  let empty_block = {
    clauses = []; app_ids = []; cond_ids = []
  }

  type cond_source_block = { 
    cond : ident;
    then_block : block;
    else_block : block;
  }

  type cond_running_block = {
    cond : ident;
    choice : bool;
    block : block;
    other_block : block;
  }

  type block_node = 
    | Main of block
    | Fun of block
    | CondSource of cond_source_block
    | CondRunning of cond_running_block

  type source_tracelet = {
    point : ident;
    outer_point : ident;
    source_block : block_node;
  }

  type t = source_tracelet

  type block_cat = 
    | Whole
    | Partial of ident

  (* type dynamic_frame = {
     point : ident;
     (* ret : ident *)
     next : ident list;
     block : dynamic_block;
     } *)

  let get_block tl =
    match tl.source_block with
    | Main b -> b
    | Fun b -> b
    | CondSource c -> 
      {
        clauses = c.then_block.clauses @ c.else_block.clauses;
        app_ids = c.then_block.app_ids @ c.else_block.app_ids;
        cond_ids = c.then_block.cond_ids @ c.else_block.cond_ids
      }
    | CondRunning c -> c.block

  let running_cond choice (cb : cond_source_block) : cond_running_block =
    { cond = cb.cond;
      choice;
      block = if choice then cb.then_block else cb.else_block;
      other_block = if choice then cb.else_block else cb.then_block;
    }

  let cids_of tl =
    tl
    |> get_block
    |> fun block -> List.map (fun (Ident id, _) -> id) block.clauses    

  let direct_cids_of tl =
    tl
    |> get_block
    |> fun block -> List.filter_map (function
        | (Ident id, Direct _) -> Some id
        | _ -> None) block.clauses

  let app_ids_of tl =
    tl
    |> get_block
    |> fun block -> List.map (fun (Ident id, _) -> id) block.app_ids   

  let cond_ids_of tl =
    tl
    |> get_block
    |> fun block -> List.map (fun (Ident id, _) -> id) block.cond_ids   

  let split_clauses (Expr clauses) : (ident * trace_clause) list * id_with_dst list * id_with_dst list =
    List.fold_left (fun (cs, app_ids, cond_ids) (Clause(Var (cid, _), b) as c) ->
        match b with
        | Appl_body (_, _)
          -> (cs @ [cid, App], app_ids @ [cid, []], cond_ids)
        | Conditional_body (Var (x, _), _, _)
          -> (cs @ [cid, Cond], app_ids, cond_ids @ [cid, []])
        | Value_body (Value_function _)
          -> (cs @ [cid, Fun], app_ids, cond_ids)
        | _
          -> (cs @ [cid, Direct c], app_ids, cond_ids)

      ) ([], [], []) clauses

  let block_of_expr e =
    let clauses, app_ids, cond_ids = split_clauses e in
    { clauses; app_ids; cond_ids }

  (* partial map
     id |-> block defined by it *)
  let tracelet_map_of_expr e : t Ident_map.t =
    let map = ref Ident_map.empty in

    let main_tracelet = 
      let block = block_of_expr e in
      let source_block = Main block in
      { point = id_main; outer_point = id_main; source_block } in
    map := Ident_map.add id_main main_tracelet !map
    ;

    let rec loop outer_point e =
      let Expr clauses = e in
      let handle_clause = function
        | Clause (Var (cid, _), Value_body (Value_function (Function_value (_arg, fbody)))) ->
          let block = block_of_expr fbody in
          let source_block = Fun block in
          let tracelet = 
            { point = cid ; outer_point; source_block } in
          map := Ident_map.add cid tracelet !map;
          loop cid fbody
        | Clause (Var (cid, _), Conditional_body (Var(cond, _), e1, e2)) ->
          let then_block = block_of_expr e1
          and else_block = block_of_expr e2 in
          let source_block = 
            CondSource {cond; then_block; else_block} in
          let tracelet = 
            { point = cid ; outer_point; source_block } in
          map := Ident_map.add cid tracelet !map;
          loop cid e1;
          loop cid e2     
        | _ -> ()
      in
      List.iter handle_clause clauses
    in

    loop id_main e;
    !map

  let cut_before x tl =
    let cut_block block : block = 
      let clauses, app_ids, cond_ids =
        List.fold_while 
          (fun _acc (cid, _c) -> cid <> x)
          (fun (cs, app_ids, cond_ids) (cid, tc as cc) ->
             (* TODO: bad design *)
             match tc with
             | App ->
               let dst = List.assoc cid block.app_ids in
               (cs @ [cc], app_ids @ [cid, dst], cond_ids)
             | Cond ->
               let dst = List.assoc cid block.cond_ids in
               (cs @ [cc], app_ids, cond_ids @ [cid, dst])
             | Fun ->
               (cs @ [cc], app_ids, cond_ids)
             | Direct cd ->
               (cs @ [cc], app_ids, cond_ids)
          )
          ([], [], [])
          block.clauses
        |> fst
      in
      { clauses; app_ids; cond_ids }
    in
    let source_block = 
      match tl.source_block with
      | Main b -> Main (cut_block b)
      | Fun b -> Fun (cut_block b)
      | CondSource cond ->
        (* TODO : 
           Noting: cutting can only occur in one block of a cond, therefore
           it's a hidden change from CondSource to CondRunning
        *)
        let choice = List.mem_assoc x cond.then_block.clauses in
        let cond' = running_cond choice cond in
        CondRunning { cond' with block = cut_block cond'.block }
      | CondRunning cond -> 
        CondRunning { cond with block = cut_block cond.block }
    in
    { tl with source_block }

  let search_id x tl_map =
    tl_map
    |> Ident_map.values
    |> Enum.find (fun tl -> List.mem_assoc x @@ (get_block tl).clauses )
    |> cut_before x

end

(* open Tracelet *)

module Tunnel = struct
  (* let clauses = Ast_tools.flatten e in
     let (Clause (Var (_, id0), _)) = List.hd clauses in
     List.fold_while
     (fun (flag, prev) _ -> flag)
     (fun (flag, prev) c -> match c with
       | Clause (Var (_, id), Value_body(Value_function _))
       | Clause (Var (_, id), Conditional_body(_, _, _)) ->
         true, prev
       | _ -> 
         true, prev
     )
     (false, id0)
     clauses *)
  type callsite_tunnel = ident Ident_map.t

  exception Invalid_query of string

  let pred_map e cfg pt =
    let map = ref BatMultiPMap.empty
    (* and id_first = first_var e *)
    and acl =
      try
        Unannotated_clause(
          lift_clause @@ Ident_map.find pt (clause_mapping e))
      with
      | Not_found ->
        raise @@ Invalid_query(
          Printf.sprintf "Variable %s is not defined" (show_ident pt))
    in

    let rec loop acl id_start : unit = 
      (* let (Unannotated_clause(Abs_clause (Abs_var id, _))) = acl in
         if id = id_first
         then
         map := Ident_map.add id_start id !map
         else  *)
      (* 

          *)
      match acl with
      | Unannotated_clause _
      | Start_clause _ | End_clause _ ->
        Enum.iter (fun acl -> loop acl id_start) (preds acl cfg)
      | Binding_exit_clause (Abs_var __para, Abs_var _ret_var, Abs_clause(_, _site_clause)) -> 
        (* can be ignored since 
           exit occurs when it meets a *-site and *-site will be handled later 

           a = f 1
           <-
        *)
        map := BatMultiPMap.add id_start id_start !map;
        let def_acls = List.of_enum @@ preds acl cfg in
        assert (List.length def_acls = 1);
        ()
      | Nonbinding_enter_clause (_, _)
      | Binding_enter_clause (_, _, _)
        ->
        ()
    in

    (* Enum.iter (fun _ac ->
         let map' = Ident_map.add pt pt !map in
         map := map' in
         )  *)

    loop acl pt;
    !map


end


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

