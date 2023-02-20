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

let find_record_env_deps (Record_value record : record_value) (env : penv) : penv * LexAdr_set.t =
  let lines, deps = get_many_lines_from_ident_opt (Ident_map.values record) env
  (* in let () = Format.printf "%a\n" LexAdr_set.pp deps; Format.printf "%a\n" (pp_enum pp_var) (Ident_map.values record) *)
  in List.fold_right begin fun cur_lexadr new_env ->
    (* Format.printf "%s\n" (show_lexadr cur_lexadr);
    Format.printf "%a\n" pp_penv new_env; *)
    let wrap_lexadr = LexAdr cur_lexadr
    in let (ident, _, _) as cur_val = IdentLine_map.find wrap_lexadr env (* get_many_deps only returns real deps *)
    in add_ident_line ident cur_lexadr cur_val new_env
  end lines IdentLine_map.empty, deps

(* Note: Only reason right now to also return penv is for debugging purposes only *)
(* There should be no diverging concerns here, as this devaluator (dependency evaluator)
   only enters scope; it does not evaluate any functions *)
let rec simple_deval (start_envnum : int) (expr : expr) : LexAdr_set.t * penv = (

  let rec dummy_map_val = PValue (AbortClosure IdentLine_map.empty)

  and deval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : LexAdr_set.t * penv = 
    (* Format.printf "Eval expression with envnum %d, with following environment:\n%a\n" envnum pp_penv env; *)
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = deval_clause (envnum, index+1) env clause in env' 
    )
    in let endenv = List.fold_lefti foldable_eval_clause env clauses
    in let _, _, enddeps = (IdentLine_map.find (LexAdr (0, -1)) endenv)
    in enddeps, endenv

  and deval_clause (lexadr : int * int) (env : penv) (Clause (Var (x, _), body) : clause) : penv = 
    let linedeps = match body with

    | Value_body (Value_function Function_value (_, func_expr)) -> begin
      let inner_envnum = fst lexadr + 1
      in LexAdr_set.filter (fun (envnum, _) -> envnum < inner_envnum) @@ fst @@ deval_expr inner_envnum env func_expr
    end
    
    | Value_body (Value_record Record_value record) -> get_many_deps_from_ident_opt (Ident_map.values record) env
    
    | Value_body v -> LexAdr_set.empty

    (* | Var_body vx -> *)

    | Input_body -> LexAdr_set.empty

    (* | Match_body (vx, _) -> *)

    (* Improvement could be made to find deps of only e1 or e2 if vx is indeed known beforehand *)
    | Conditional_body (vx, e1, e2) -> begin
      let bool_deps = get_deps_from_ident vx env
      in let inner_envnum = fst lexadr + 1
      in let inner_deps = LexAdr_set.union (deval_expr inner_envnum env e1 |> fst) (deval_expr inner_envnum env e2 |> fst)
      in LexAdr_set.union bool_deps (LexAdr_set.filter (fun (envnum, _) -> envnum < inner_envnum) inner_deps)
    end

    (* Deps of function should have already been found, only need to union func_deps with val_deps *)
    | Appl_body (vx1, vx2) -> LexAdr_set.union (get_deps_from_ident vx1 env) (get_deps_from_ident vx2 env)

    (* Improvement could be made to find deps of only var associated with key if v is indeed known beforehand *)
    | Projection_body (v, key) -> get_deps_from_ident v env
    
    (* | Not_body vx -> *)
    
    | Binary_operation_body (vx1, op, vx2) -> LexAdr_set.union (get_deps_from_ident vx1 env) (get_deps_from_ident vx2 env)

    | Var_body vx | Match_body (vx, _) | Not_body vx -> get_deps_from_ident vx env
    
    | Abort_body | Assert_body _ | Assume_body _ -> failwith "Evaluation does not yet support abort, assert, and assume!"
  
    in (add_ident_line_penv x lexadr linedeps dummy_map_val env)
  
  in deval_expr start_envnum IdentLine_map.empty expr
)

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
  | FunClosure (_, _, _), Fun_pattern -> true
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
    | PExpr Expr expr_val -> expr_val
    | _ ->
      let clause_body = match [@warning "-8"] presidual with
      | PValue pval -> Value_body (value_of_pvalue pval)
      | PClause pcls -> pcls
      in [(Clause (Var (ident, None), clause_body))]
    in (fun res -> next (clauses_val @ res))
  in Expr ((LexAdr_set.fold inner_reconstruct deps (fun res -> res)) [])





(* evals *)
let simple_eval (expr : expr) : value * penv = (

  let rec eval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : penv = 
    (* Format.printf "Eval expression with envnum %d, with following environment:\n%a\n" envnum pp_penv env; *)
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = eval_clause (envnum, index+1) env clause in env' 
    )
    in List.fold_lefti foldable_eval_clause env clauses

  and eval_clause (lexadr : int * int) (env : penv) (Clause (Var (x, _), body) : clause) : penv = 
    let linedeps, res_value = match body with

    (* Deps list is inaccurate, need to actually go through function and see what variables are captured *)
    | Value_body (Value_function (Function_value (_, func_expr) as vf)) -> simple_deval (fst lexadr + 1) func_expr |> fst, FunClosure (x, vf, env)
    
    | Value_body (Value_record vr) -> let new_env, deps = find_record_env_deps vr env
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
      in let inner_envnum = fst lexadr + 1
      in let endenv = eval_expr inner_envnum env (if bool_val then e1 else e2)
      in let [@warning "-8"] _, PValue endpvalue, inner_deps = (IdentLine_map.find (LexAdr (inner_envnum, -1)) endenv)
      in LexAdr_set.union bool_deps (LexAdr_set.filter (fun (envnum, _) -> envnum < inner_envnum) inner_deps), endpvalue
    end

    | Appl_body (vx1, vx2) -> begin
      let func_deps, func_x, func_expr, func_env = match get_pvalue_deps_from_ident vx1 env with
      | deps, FunClosure (_, Function_value (Var (x, _), expr), env) -> deps, x, expr, env
      | _ -> failwith "Type error! Function application attempted with a non-function!"
      in let inner_envnum = fst lexadr + 1
      in let endenv = eval_expr inner_envnum (get_from_ident vx2 env |> (add_ident_el_penv func_x ~map:func_env)) func_expr
      in let [@warning "-8"] _, PValue endpvalue, inner_deps = (IdentLine_map.find (LexAdr (inner_envnum, -1)) endenv)
      in LexAdr_set.union func_deps (LexAdr_set.filter (fun (envnum, _) -> envnum < inner_envnum) inner_deps), endpvalue
    end

    (* Deps list here actually only needs to be the original captured variable, not the whole list for the record! *)
    | Projection_body (v, key) -> begin match get_pvalue_from_ident v env with
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
  
  in let endenv = eval_expr 0 IdentLine_map.empty expr
  in let [@warning "-8"] _, PValue endpvalue, _ = (IdentLine_map.find (LexAdr (0, -1)) endenv)
  in value_of_pvalue endpvalue, endenv
)
;;

let simple_peval (peval_input : bool) (expr : expr) : expr * penv = (

  let rec peval_expr (envnum : int) (env : penv) (Expr (clauses) : expr) : expr * penv = 
    let foldable_eval_clause (env : penv) (index : int) (clause : clause) : penv = (
      let env' = peval_clause (envnum, index+1) env clause in env' 
    )
    in let endenv = List.fold_lefti foldable_eval_clause env clauses
    in let _, _, enddeps = (IdentLine_map.find (LexAdr (0, -1)) endenv)
    in reconstruct_expr_from_lexadr enddeps endenv, endenv

  and peval_clause (lexadr : int * int) (env : penv) (Clause (Var (x, _), body) : clause) : penv = 
    let bail = bail_with body in
    let linedeps, res_value = begin match body with

    (* Deps list is inaccurate, need to actually go through function and see what variables are captured *)
    | Value_body (Value_function vf) -> LexAdr_set.empty, PValue (FunClosure (x, vf, env))
    
    | Value_body (Value_record vr) -> let new_env, deps = find_record_env_deps vr env
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

    | Conditional_body (x2, e1, e2) -> failwith "Evaluation does not yet support conditionals!"

    | Appl_body (vx1, (Var (x2, _) as vx2)) -> failwith "Evaluation does not yet support function application!"

    (* Deps list here actually only needs to be the original captured variable, not the whole list for the record! *)
    | Projection_body (v, key) -> bail begin
      match get_pvalue_from_ident_opt v env with
      | (None, _ as none) -> none
      | Some (RecordClosure (Record_value r, renv)), _ ->
          let proj_x = Ident_map.find key r in bail_compose (Var_body proj_x) (
            get_presidual_from_ident_semi_ref_opt proj_x renv
        )
      | _ -> failwith "Type error! Projection attempted on a non-record!"
      end
    
    | Not_body vx -> bail begin
      let+ presidual = get_pvalue_from_ident_opt vx env in
      match presidual with
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
    
    | Abort_body | Assert_body _ | Assume_body _ -> failwith "Evaluation does not yet support abort, assert, and assume!"

    end in (add_ident_line_penv x lexadr linedeps res_value env)
  
  in peval_expr 0 IdentLine_map.empty expr
)
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