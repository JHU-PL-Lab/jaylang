open Core
open Graph
open Dj_common
open Log.Export
open Jayil
open Ast

open Batteries
open Jhupllib
include Types_and_printing
open OptionSyntax

let rec constant_folding (Expr clauses) =
  let new_clauses = List.map constant_folding_clause clauses in
  Expr new_clauses

and constant_folding_clause clause =
  let (Clause (Var (x, s), cbody)) = clause in
  match cbody with
  | Binary_operation_body (x1, Binary_operator_minus, x2)
    when Ast.Var.equal x1 x2 ->
      let new_body = Value_body (Value_int 0) in
      Clause (Var (x, s), new_body)
  | _ -> clause

let eval expr =

  Fmt.pr "%a\n" Jayil.Pp.expr expr ;
  let new_expr = constant_folding expr in
  Fmt.pr "%a\n" Jayil.Pp.expr new_expr ;

  new_expr
;;

let eval_file raw_source = raw_source |> Dj_common.File_utils.read_source |> eval;;


(* Helper functions *)

let bail_with (default : clause_body) ((attempt, deps) : presidual option * LexAdr_set.t) = 
  match attempt with
  | Some ((PValue _) as x) -> LexAdr_set.empty,  x
  | Some x -> deps, x
  | None -> deps, PClause(default) 

let bail_compose (default : clause_body) ((attempt, deps) : presidual option * LexAdr_set.t) =
  match attempt with
  | Some ((PValue _) as x) -> Some x, LexAdr_set.empty
  | Some x -> Some x, deps
  | None -> Some (PClause(default)), deps




(* Eval helpers *)
(* let rec find_function_env_deps (Function_value (Var (Ident id , _), func) : function_value) (env : penv) =
  let rec in_find_fun (Expr func_clauses : expr) (env : penv) =
    List.fold_left *)

let find_var_env_deps (var_enum : var Enum.t) (env : penv) : penv * LexAdr_set.t =
  let lines, deps = get_many_lines_from_ident_opt var_enum env
  (* in let () = Format.printf "Finding var env deps:\n %a\n" LexAdr_set.pp deps; Format.printf "%a\n" (pp_enum pp_var) (var_enum) *)
  in List.fold_right begin fun cur_lexadr new_env ->
    (* Format.printf "%s\n" (show_lexadr cur_lexadr);
    Format.printf "%a\n" pp_penv new_env; *)
    let wrap_lexadr = LexAdr cur_lexadr
    in let (ident, _, _) as cur_val = IdentLine_map.find wrap_lexadr env (* get_many_deps only returns real deps *)
    in add_ident_line ident cur_lexadr cur_val new_env
  end lines IdentLine_map.empty, deps

let prepend_vars_to_expr (var_enum : var Enum.t) (Expr clauses : expr) (env : penv) : expr =
  Expr (Enum.fold (fun cur_expr cur_var ->
    let pvalue = get_pvalue_from_ident cur_var env
    in Clause (cur_var, Value_body (value_of_pvalue pvalue)) :: cur_expr
  ) clauses var_enum)
;;

(* Currently, all captured function & record values are unconditionally moved inside the function body,
   even when those values themselves may have dependencies / reference non-evaled variables. This may
   or may not be desired...But thankfully, this is easy to fix by just modifying the pattern matching
   below. *)
let fold_what (old_env : penv) ((cur_expr, cur_env, cur_deps) : clause list * penv * LexAdr_set.t) (cur_var : var) =
  match fst @@ get_from_ident_opt cur_var old_env with
  (* | Some (_, PValue pvalue, deps) ->  *)
  | Some (_, PValue (Direct _ as pvalue), deps) ->
    Clause (cur_var, Value_body (value_of_pvalue pvalue)) :: cur_expr, cur_env, LexAdr_set.union (LexAdr_set.pop_max deps |> snd) cur_deps
  | Some ((ident, _, deps) as cur_val) -> 
    cur_expr, (add_ident_line ident (LexAdr_set.max_elt deps) cur_val cur_env), LexAdr_set.union deps cur_deps
  | None -> cur_expr, cur_env, cur_deps
;;

let prepend_vars_if_pvalue_else_env_deps (var_enum : var Enum.t) (Expr clauses : expr) (env : penv) : expr * penv * LexAdr_set.t =
  let new_expr, new_env, new_deps = Enum.fold (fold_what env) (clauses, IdentLine_map.empty, LexAdr_set.empty) var_enum
in Expr new_expr, new_env, new_deps
;;

let ident_set_from_var_enum (enum : var Enum.t) =
  Ident_set.of_enum @@ Enum.map (fun (Var (x, _)) -> x) enum
;;

let simple_cval (expr : expr) : Var_set.t = begin

  let rec empty_ident_pair = (Var_set.empty, Var_set.empty)

  and cval_expr (ident_pair : Var_set.t * Var_set.t) (Expr (clauses) : expr) : Var_set.t = 
    
    let foldable_eval_clause (ident_pair : Var_set.t * Var_set.t) (index : int) (clause : clause) : Var_set.t * Var_set.t = (
      let ident_pair' = cval_clause ident_pair clause in ident_pair' 
    )
    in let captured, _ = List.fold_lefti foldable_eval_clause ident_pair clauses
    in captured

  and cval_clause ((cap, def) : Var_set.t * Var_set.t) (Clause (vx, body) : clause) : Var_set.t * Var_set.t =
    let lineuses = match body with

    | Value_body (Value_function Function_value (vx1, func_expr)) -> begin
      let added_ident_pair = (Var_set.empty, Var_set.singleton vx1)
      in cval_expr added_ident_pair func_expr
    end
    
    (* | Value_body (Value_record Record_value record) -> Var_set.of_enum @@ Enum.map (fun (Var (x, _)) -> x) (Ident_map.values record) *)
    | Value_body (Value_record Record_value record) -> Var_set.of_enum @@ Ident_map.values record
    
    | Value_body v -> Var_set.empty

    (* | Var_body vx -> *)

    | Input_body -> Var_set.empty

    (* | Match_body (vx, _) -> *)

    (* Improvement could be made to find deps of only e1 or e2 if vx is indeed known beforehand *)
    | Conditional_body (vx, e1, e2) -> begin
      let bool_ident = Var_set.singleton vx
      in let inner_deps = Var_set.union (cval_expr empty_ident_pair e1) (cval_expr empty_ident_pair e2)
      in Var_set.union bool_ident inner_deps
    end

    (* Deps of function should have already been found, only need to union func_deps with val_deps *)
    | Appl_body (vx1, vx2) -> Var_set.of_list [vx1; vx2]

    (* Improvement could be made to find deps of only var associated with key if v is indeed known beforehand *)
    | Projection_body (v, key) -> Var_set.singleton v
    
    (* | Not_body vx -> *)
    
    | Binary_operation_body (vx1, op, vx2) -> Var_set.of_list [vx1; vx2]

    | Var_body vx | Match_body (vx, _) | Not_body vx -> Var_set.singleton vx
    
    | Abort_body | Assert_body _ | Assume_body _ -> failwith "Evaluation does not yet support abort, assert, and assume!"
  
    in Var_set.union cap (Var_set.diff lineuses def), Var_set.add vx def

  in cval_expr empty_ident_pair expr
end
;;

let check_pattern (v : pvalue) (pattern : pattern) : bool =
  match v, pattern with
  | Direct (Value_int _), Int_pattern -> true
  | Direct (Value_bool _), Bool_pattern -> true
  | Direct (Value_function _), _ -> failwith "fun must be a closure (Impossible!)"
  | Direct (Value_record _), _ -> failwith "record must be a closure (Impossible!)"
  | RecordClosure (Record_value record, _), Rec_pattern key_set ->
      Ident_set.for_all (fun id -> Ident_map.mem id record) key_set
  | RecordClosure (Record_value record, _), Strict_rec_pattern key_set ->
      Ident_set.equal key_set (Ident_set.of_enum @@ Ident_map.keys record)
  | FunClosure (_, _, _, _), Fun_pattern -> true
  | _, Any_pattern -> true
  | _ -> false

and binop = function
  | Binary_operator_plus, Value_int n1, Value_int n2 ->
      Value_int (n1 + n2)
  | Binary_operator_minus, Value_int n1, Value_int n2 ->
      Value_int (n1 - n2)
  | Binary_operator_times, Value_int n1, Value_int n2 ->
      Value_int (n1 * n2)
  | Binary_operator_divide, Value_int n1, Value_int n2 ->
      Value_int (n1 / n2)
  | Binary_operator_modulus, Value_int n1, Value_int n2 ->
      Value_int (n1 mod n2)
  | Binary_operator_less_than, Value_int n1, Value_int n2 ->
      Value_bool (n1 < n2)
  | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 ->
      Value_bool (n1 <= n2)
  | Binary_operator_equal_to, Value_int n1, Value_int n2 ->
      Value_bool (n1 = n2)
  | Binary_operator_equal_to, Value_bool b1, Value_bool b2 ->
      Value_bool (Core.Bool.( = ) b1 b2)
  | Binary_operator_and, Value_bool b1, Value_bool b2 ->
      Value_bool (b1 && b2)
  | Binary_operator_or, Value_bool b1, Value_bool b2 ->
      Value_bool (b1 || b2)
  | _, _, _ -> failwith "incorrect binop"


let reconstruct_expr_from_lexadr (deps : LexAdr_set.t) (env : penv) : expr =
  let inner_reconstruct (deps : lexadr) next =
    let ident, presidual, _ = IdentLine_map.find (LexAdr deps) env
    in let clauses_val = match presidual with
    | PExpr (Expr expr_val, end_ident) -> expr_val @ [Clause (Var (ident, None), Var_body (Var (end_ident, None)))]
    | _ ->
      let clause_body = match [@warning "-8"] presidual with
      | PValue pval -> Value_body (value_of_pvalue pval)
      | PClause pcls -> pcls
      in [Clause (Var (ident, None), clause_body)]
    in (fun res -> next (clauses_val @ res))
  in Expr ((LexAdr_set.fold inner_reconstruct deps (fun res -> res)) [])


let bad_prefix_expr (prefix : string) (Expr (clauses) : expr) =
  Expr (List.map (fun 
  (Clause (Var (Ident old_ident, stack), body)) -> 
  (Clause (Var (Ident (prefix ^ old_ident), stack), body))
  ) clauses)


(* evals *)
let simple_eval (expr : expr) : value * penv = begin

  let rec eval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : (ident * pvalue * LexAdr_set.t) * penv = 
    (* Format.printf "Eval expression with envnum %d, with following environment:\n%a\n" envnum pp_penv env; *)
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = eval_clause (envnum, index+1) env clause in env' 
    )
    in let endenv = List.fold_lefti foldable_eval_clause env clauses
    in let [@warning "-8"] end_ident, PValue end_pvalue, inner_deps = (IdentLine_map.find (LexAdr (envnum, -1)) endenv)
    in (end_ident, end_pvalue, (LexAdr_set.filter (fun (end_envnum, _) -> end_envnum < envnum) inner_deps)), endenv

  and eval_clause ((envnum, line) as lexadr : int * int) (env : penv) (Clause (Var (x, _), body) : clause) : penv = 
    let linedeps, res_value = match body with

    | Value_body (Value_function (Function_value ((Var(x1, _)) as vx1, func_expr) as _vf)) -> begin
      let captured = Var_set.remove vx1 (simple_cval func_expr)
      (* in let () = Format.printf "%a\n" Var_set.pp captured *)
      (* in let new_env, deps = find_var_env_deps (Var_set.enum captured) env *)
      (* in let () = Format.printf "%a\n" pp_penv new_env *)
      in let new_func_expr, new_env, new_deps = prepend_vars_if_pvalue_else_env_deps (Var_set.enum captured) func_expr env
      in new_deps, FunClosure (x, (Function_value (vx1, new_func_expr)), new_env, true)
    end
    
    | Value_body (Value_record ((Record_value r) as vr)) -> let new_env, deps = find_var_env_deps (Ident_map.values r) env
      in deps, RecordClosure (vr, new_env)
    
    | Value_body v -> LexAdr_set.empty, Direct v

    | Var_body vx -> get_pvalue_deps_from_ident vx env

    | Input_body -> LexAdr_set.empty, Direct (Value_int (read_int ())) (* This will be interesting to peval...We'll need arguments for whether input is known at peval-time or not *)

    | Match_body (vx, p) -> let deps, v = get_pvalue_deps_from_ident vx env in
      deps, Direct (Value_bool (check_pattern v p))

    | Conditional_body (vx, e1, e2) -> begin
      let bool_deps, bool_val = match get_pvalue_deps_from_ident vx env with
      | deps, Direct (Value_bool b) -> deps, b
      | _ -> failwith "Type error! Conditional attempted with a non-bool!"
      in let inner_envnum = envnum + 1
      in let (_, end_pvalue, enddeps) = fst @@ eval_expr inner_envnum env (if bool_val then e1 else e2)
      in LexAdr_set.union bool_deps enddeps, end_pvalue
    end

    | Appl_body (vx1, vx2) -> begin
      let func_x, func_expr, func_env = match get_pvalue_from_ident vx1 env with
      | FunClosure (_, Function_value (Var (x, _), expr), env, _) -> x, expr, env
      | _ -> failwith "Type error! Function application attempted with a non-function!"
      in let inner_envnum = envnum + 1
      in let beginenv = get_entry_from_ident vx2 env |> (add_param_el_penv func_x inner_envnum ~map:func_env)
      (* in let () = Format.printf "Begin function with:\n %a\n" pp_penv beginenv *)
      in let (_, end_pvalue, enddeps) = fst @@ eval_expr inner_envnum beginenv func_expr
      in enddeps, end_pvalue
    end

    (* Deps list here actually only needs to be the original captured variable, not the whole list for the record! *)
    | Projection_body (vx, key) -> begin match get_pvalue_from_ident vx env with
      | RecordClosure (Record_value r, renv) ->
        let proj_x = Ident_map.find key r in
        get_pvalue_deps_from_ident proj_x renv
      | _ -> failwith "Type error! Projection attempted on a non-record!"
    end
    
    | Not_body vx -> begin match get_pvalue_deps_from_ident vx env with
      | deps, Direct (Value_bool b) -> deps, Direct (Value_bool (not b))
      | _ -> failwith "Type error! Not attempted on a non-bool!"
    end
    
    | Binary_operation_body (vx1, op, vx2) ->
      let lexadrs1, v1 = get_pvalue_deps_from_ident vx1 env
      and lexadrs2, v2 = get_pvalue_deps_from_ident vx2 env in
      let v1, v2 = match v1, v2 with
        | Direct v1, Direct v2 -> v1, v2
        | _ -> failwith "Type error! Binary ops attempted on incompatible types!"
      in let v = binop (op, v1, v2)
      in LexAdr_set.union lexadrs1 lexadrs2, Direct v
    
    | Abort_body | Assert_body _ | Assume_body _ -> failwith "Evaluation does not yet support abort, assert, and assume!"
  
    in (add_ident_line_penv x lexadr linedeps (PValue res_value) env)
  
  in let (_, end_pvalue, _), endenv = eval_expr 0 IdentLine_map.empty expr
  in value_of_pvalue end_pvalue, endenv
end
;;

let simple_peval (peval_input : bool) (expr : expr) : expr * penv = begin

  let rec peval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : (ident * expr * LexAdr_set.t) * penv = 
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = peval_clause (envnum, index+1) env clause in env' 
    )
    in let endenv = List.fold_lefti foldable_eval_clause env clauses
    in let end_ident, _, enddeps = (IdentLine_map.find (LexAdr (envnum, -1)) endenv)
    in (
      end_ident,
      reconstruct_expr_from_lexadr
        (LexAdr_set.filter (fun (end_envnum, _) -> end_envnum = envnum) enddeps)
        endenv,
      (LexAdr_set.filter (fun (end_envnum, _) -> end_envnum < envnum) enddeps)
      ), endenv

  and peval_clause ((envnum, line) as lexadr : int * int) (env : penv) (Clause (Var (Ident assign_ident as x, _), body) : clause) : penv = 
    let bail = bail_with body in
    let linedeps, res_value = begin match body with

    (* Deps list is inaccurate, need to actually go through function and see what variables are captured *)
    | Value_body (Value_function (Function_value ((Var(x1, _)) as vx1, func_expr) as _vf)) -> begin 
      let init_captured = Var_set.remove vx1 (simple_cval func_expr)
      in let optim_func_expr, (captured, uses_param) = if Var_set.is_empty init_captured then func_expr, (init_captured, true) else
        let init_env, _ = find_var_env_deps (Var_set.enum init_captured) env
        in let inner_envnum = envnum + 1
        in let _, new_func_expr, _ = fst @@ peval_expr inner_envnum init_env func_expr
        in let new_captured = simple_cval new_func_expr
        in new_func_expr, match Var_set.find_opt vx1 new_captured with
            | Some _ -> Var_set.remove vx1 new_captured, true
            | None -> new_captured, false
      in let new_func_expr, new_env, new_deps = prepend_vars_if_pvalue_else_env_deps (Var_set.enum captured) func_expr env
      in new_deps, PValue (FunClosure (x, Function_value(vx1, new_func_expr), new_env, uses_param))
    end
    
    | Value_body (Value_record ((Record_value r) as vr)) -> let new_env, deps = find_var_env_deps (Ident_map.values r) env
      in deps, PValue (RecordClosure (vr, new_env))

    | Value_body v -> LexAdr_set.empty, PValue (Direct v)

    | Var_body vx -> bail @@ get_presidual_from_ident_semi_ref_opt vx env
 
    | Input_body -> LexAdr_set.empty, if peval_input (* This will be interesting to peval...We'll need arguments for whether input is known at peval-time or not *)
      then PValue (Direct (Value_int (read_int ()))) (* Interestingly enough, this condition should itself be partially evaluated... *)
      else PClause(body)

    | Match_body (vx, p) -> bail begin
      let+ pval = get_pvalue_from_ident_opt vx env in
      PValue (Direct (Value_bool (check_pattern pval p)))
    end

    | Conditional_body (Var (Ident cond_ident, _) as vx, e1, e2) -> bail begin
      let inner_envnum = envnum + 1
      in match get_pvalue_from_ident_opt vx env with
      | None, cond_deps ->
        let    (_, end_pexpr1, enddeps1) = fst @@ peval_expr inner_envnum env e1
        in let (_, end_pexpr2, enddeps2) = fst @@ peval_expr inner_envnum env e2
        in Some (PClause (Conditional_body (vx, end_pexpr1, end_pexpr2))), LexAdr_set.union cond_deps (LexAdr_set.union enddeps1 enddeps2)
      | Some (Direct (Value_bool bool_val)), _ ->
        let (end_ident, end_pexpr, enddeps) = fst @@ peval_expr inner_envnum env (if bool_val then e1 else e2)
        in let prefix = Printf.sprintf "%s__%s__" assign_ident cond_ident
        in let modified_pexpr = bad_prefix_expr prefix end_pexpr
        in Some (PExpr (modified_pexpr, end_ident)), enddeps
      | _ -> failwith "Type error! Conditional attempted with a non-bool!"
    end

    (* let bool_deps, bool_val = match get_pvalue_deps_from_ident vx env with
      | deps, Direct (Value_bool b) -> deps, b
      | _ -> failwith "Type error! Conditional attempted with a non-bool!" *)

    (* Partial eval on function applications is unnecessarily strict at the moment. If the function
       is known but the argument is not, we refuse to partially evaluate. *)
    | Appl_body (Var (Ident func_ident, _) as vx1, (Var (Ident arg_ident, _) as vx2)) -> bail begin
      match
        let+ v1 = get_pvalue_from_ident_opt vx1 env
        and+ _ = get_pvalue_from_ident_opt vx2 env
        in v1
      with
      | ((None, _) as none) -> none
      | Some (v1), _ -> let func_x, func_expr, func_env = match v1 with
          | FunClosure (_, Function_value (Var (x, _), expr), env, _) -> x, expr, env
          | _ -> failwith "Type error! Function application attempted with a non-function!"
        in let inner_envnum = envnum + 1
        in let beginenv = get_entry_from_ident vx2 env |> (add_param_el_penv func_x inner_envnum ~map:func_env)
        in let (end_ident, end_pexpr, enddeps) = fst @@ peval_expr inner_envnum beginenv func_expr
        in let prefix = Printf.sprintf "%s__%s__%s__" assign_ident func_ident arg_ident
        in let modified_pexpr = bad_prefix_expr prefix end_pexpr
        in Some (PExpr (modified_pexpr, end_ident)), enddeps
    end

    
    (* Partial eval on function applications can be made more strict. If the function
    is known but the argument is not, we can refuse to partially evaluate. *)
    (* | Appl_body (Var (x1, _) as vx1, (Var (x2, _) as vx2)) -> bail begin
      match
        let+ v1 = get_pvalue_from_ident_opt vx1 env
        and+ v2 = get_entry_from_ident_opt vx2 env
        in v1, v2
      with
      | ((None, _) as none) -> none
      | Some (v1, v2), _ -> let func_x, func_expr, func_env = match v1 with
          | FunClosure (_, Function_value (Var (x, _), expr), env, _) -> x, expr, env
          | _ -> failwith "Type error! Function application attempted with a non-function!"
        in let inner_envnum = envnum + 1
        in let beginenv = v2 |> (add_param_el_penv func_x inner_envnum ~map:func_env)
        in let (end_ident, end_pexpr, enddeps) = fst @@ peval_expr inner_envnum beginenv func_expr
        in let prefix = let Ident func_ident, Ident arg_ident = x1, x2 in Printf.sprintf "%s__%s__" func_ident arg_ident
        in let modified_pexpr = prefix_expr prefix end_pexpr
        in Some (PExpr (modified_pexpr, end_ident)), enddeps
    end *)

    (* Deps list here actually only needs to be the original captured variable, not the whole list for the record! *)
    | Projection_body (vx, key) -> bail begin
      match get_pvalue_from_ident_opt vx env with
      | (None, _ as none) -> none
      | Some (RecordClosure (Record_value r, renv)), _ ->
          let proj_x = Ident_map.find key r in bail_compose (Var_body proj_x) (
            get_presidual_from_ident_semi_ref_opt proj_x renv
        )
      | _ -> failwith "Type error! Projection attempted on a non-record!"
    end
    
    | Not_body vx -> bail begin
      let+ pvalue = get_pvalue_from_ident_opt vx env in
      match pvalue with
      | Direct (Value_bool b) -> PValue (Direct (Value_bool (not b)))
      | _ -> failwith "Type error! Not attempted on a non-bool!"
    end
    
    | Binary_operation_body (vx1, op, vx2) -> bail begin
      let+ v1 = get_pvalue_from_ident_opt vx1 env
      and+ v2 = get_pvalue_from_ident_opt vx2 env in
      let v1, v2 = match v1, v2 with
        | Direct v1, Direct v2 -> v1, v2
        | _ -> failwith "Type error! Binary ops attempted on incompatible types!"
      in let v = binop (op, v1, v2)
      in PValue (Direct v)
    end
    
    | Abort_body -> bail (None, LexAdr_set.empty) (* LexAdr_set.empty, PClause clause *)
    
    | Assert_body vx | Assume_body vx -> bail (None, snd @@ get_deps_from_ident_opt vx env)

    end in (add_ident_line_penv x lexadr linedeps res_value env)
  
  in let (_, end_expr, _), endenv = peval_expr 0 IdentLine_map.empty expr
  in end_expr, endenv
end
;;




(* Defining printing modules *)
module StringParser = struct
  let parse = pstring
end

module FileParser = struct
  let parse = pfile
end

module SimpleEval = PEToploop (FileParser) (struct type t = value;; let eval = simple_eval;; let unparse = unparse_value end)

module PartialEval = PEToploop (FileParser) (struct type t = expr;; let eval = simple_peval true;; let unparse = unparse_expr end)



(* Below copied from dbmc *)
exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : pvalue }
exception Found_abort of pvalue
exception Terminate of pvalue
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

type clause_cb = Id.t -> Concrete_stack.t -> value -> unit
type debug_mode = No_debug | Debug_clause of clause_cb

module G = Imperative.Digraph.ConcreteBidirectional (Id_with_stack)

type session = {
  (* tuning *)
  step : int ref;
  max_step : int option;
  (* book-keeping *)
  alias_graph : G.t;
  (* debug *)
  is_debug : bool; (* TODO: get rid of this *)
  debug_mode : debug_mode;
  val_def_map : (Id_with_stack.t, clause_body * pvalue) Core.Hashtbl.t;
  (* term_detail_map : (Lookup_key.t, Term_detail.t) Hashtbl.t; *)
  (* block_map : Cfg.block Jayil.Ast.Ident_map.t; *)
  (* rstk_picked : (Rstack.t, bool) Hashtbl.t; *)
  (* lookup_alert : Lookup_key.t Hash_set.t; *)
}

let make_default_session () =
  {
    max_step = None;
    is_debug = false;
    debug_mode = No_debug;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Core.Hashtbl.create (module Id_with_stack);
    (* term_detail_map = Hashtbl.create (module Lookup_key); *)
    (* block_map = Jayil.Ast.Ident_map.empty; *)
    (* rstk_picked = Hashtbl.create (module Rstack); *)
    (* lookup_alert = Hash_set.create (module Lookup_key); *)
  }

(* let create_session ?max_step ?(debug_mode = No_debug) (* state : Global_state.t *)
    (config : Global_config.t) mode input_feeder : session =
  (* = With_full_target (config.target, target_stk) *)
  {
    max_step;
    is_debug = config.debug_interpreter;
    debug_mode;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Core.Hashtbl.create (module Id_with_stack);
    (* term_detail_map = Hashtbl.create (module Lookup_key); (* state.term_detail_map; *) *)
    (* block_map = Jayil.Ast.Ident_map.empty; (* state.block_map; *) *)
    (* rstk_picked = Hashtbl.create (module Rstack); (* state.rstk_picked; *) *)
    (* lookup_alert = Hash_set.create (module Lookup_key); (* state.lookup_alert; *) *)
  } *)

let cond_fid b = if b then Ast.Ident "$tt" else Ast.Ident "$ff"

(* This function will add a directed edge x1 -> x2 in the alias graph. Thus
   x1 here needs to be the *later* defined variable. *)
let add_alias x1 x2 session : unit =
  let alias_graph = session.alias_graph in
  G.add_edge alias_graph x1 x2

let add_val_def_mapping x vdef session : unit =
  let val_def_mapping = session.val_def_map in
  Core.Hashtbl.add_exn ~key:x ~data:vdef val_def_mapping

(* let debug_update_read_node (session : session) (x : ident) (stk : Concrete_stack.t) =  ()

let debug_update_write_node (session : session) (x : ident) (stk : Concrete_stack.t) = ()

let debug_stack (session : session) (x : ident) (stk : Concrete_stack.t) ((v : pvalue), _) = ()

let raise_if_with_stack (session : session) (x : ident) (stk : Concrete_stack.t) (v : pvalue) = ()

let alert_lookup (session : session) (x : ident) (stk : Concrete_stack.t) = () *)

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

(* let debug_clause ~session x v stk =
  ILog.app (fun m -> m "@[%a = %a@]" Id.pp x pp_pvalue v) ;

  raise_if_with_stack session x stk v ;
  debug_stack session x stk (v, stk) ;
  ()

(* OB: we cannot enter the same stack twice. *)
let rec fetch_val_with_stk ~session ~stk env (Var (x, _)) :
    pvalue * Concrete_stack.t =
  let res = Ident_map.find x env in
  debug_update_read_node session x stk ;
  res

and fetch_val ~session ~stk env x : pvalue =
  fst (fetch_val_with_stk ~session ~stk env x)

and fetch_val_to_direct ~session ~stk env vx : value =
  match fetch_val ~session ~stk env vx with
  | Direct v -> v
  | _ -> failwith "eval to non direct value"

and fetch_val_to_bool ~session ~stk env vx : bool =
  match fetch_val ~session ~stk env vx with
  | Direct (Value_bool b) -> b
  | _ -> failwith "eval to non bool"
 *)