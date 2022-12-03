open Core
open Graph
open Dj_common
open Log.Export
open Jayil
open Ast

let rec constant_folding (Expr clauses) =
  let new_clauses = List.map ~f:constant_folding_clause clauses in
  Expr new_clauses

and constant_folding_clause clause =
  let (Clause (Var (x, s), cbody)) = clause in
  match cbody with
  | Binary_operation_body (x1, Binary_operator_minus, x2)
    when Ast.Var.equal x1 x2 ->
      let new_body = Value_body (Value_int 0) in
      Clause (Var (x, s), new_body)
  | _ -> clause

let eval raw_source =
  let expr = Dj_common.File_utils.read_source raw_source in

  Fmt.pr "%a\n" Jayil.Pp.expr expr ;
  let new_expr = constant_folding expr in
  Fmt.pr "%a\n" Jayil.Pp.expr new_expr ;
  expr

type dvalue =
  | Direct of value
  | FunClosure of Ident.t * function_value * denv
  | RecordClosure of record_value * denv
  | AbortClosure of denv

and dvalue_with_stack = dvalue * Concrete_stack.t
and denv = dvalue_with_stack Ident_map.t

let value_of_dvalue = function
  | Direct v -> v
  | FunClosure (_fid, fv, _env) -> Value_function fv
  | RecordClosure (r, _env) -> Value_record r
  | AbortClosure _ -> Value_bool false

let rec pp_dvalue oc = function
  | Direct v -> Jayil.Ast_pp.pp_value oc v
  | FunClosure _ -> Format.fprintf oc "(fc)"
  | RecordClosure (r, env) -> pp_record_c (r, env) oc
  | AbortClosure _ -> Format.fprintf oc "(abort)"

and pp_record_c (Record_value r, env) oc =
  let pp_entry oc (x, v) =
    Fmt.pf oc "%a = %a" Jayil.Ast_pp.pp_ident x Jayil.Ast_pp.pp_var v
  in
  (Fmt.braces (Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry))
    oc r

exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : dvalue }
exception Found_abort of dvalue
exception Terminate of dvalue
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

type mode =
  | Plain
  | With_target_x of Id.t
  | With_full_target of Id.t * Concrete_stack.t

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
  val_def_map : (Id_with_stack.t, clause_body * dvalue) Hashtbl.t;
  (* term_detail_map : (Lookup_key.t, Term_detail.t) Hashtbl.t; *)
  block_map : Cfg.block Jayil.Ast.Ident_map.t;
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
    val_def_map = Hashtbl.create (module Id_with_stack);
    block_map = Jayil.Ast.Ident_map.empty;
    (* term_detail_map = Hashtbl.create (module Lookup_key); *)
    (* rstk_picked = Hashtbl.create (module Rstack); *)
    (* lookup_alert = Hash_set.create (module Lookup_key); *)
  }

let create_session ?max_step ?(debug_mode = No_debug) (* state : Global_state.t *)
    (config : Global_config.t) mode input_feeder : session =
  (* = With_full_target (config.target, target_stk) *)
  {
    max_step;
    is_debug = config.debug_interpreter;
    debug_mode;
    step = ref 0;
    alias_graph = G.create ();
    block_map = Jayil.Ast.Ident_map.empty; (* state.block_map; *)
    val_def_map = Hashtbl.create (module Id_with_stack);
    (* term_detail_map = Hashtbl.create (module Lookup_key); (* state.term_detail_map; *) *)
    (* rstk_picked = Hashtbl.create (module Rstack); (* state.rstk_picked; *) *)
    (* lookup_alert = Hash_set.create (module Lookup_key); (* state.lookup_alert; *) *)
  }

let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(* This function will add a directed edge x1 -> x2 in the alias graph. Thus
   x1 here needs to be the *later* defined variable. *)
let add_alias x1 x2 session : unit =
  let alias_graph = session.alias_graph in
  G.add_edge alias_graph x1 x2

let add_val_def_mapping x vdef session : unit =
  let val_def_mapping = session.val_def_map in
  Hashtbl.add_exn ~key:x ~data:vdef val_def_mapping

let debug_update_read_node (session : session) (x : ident) (stk : Concrete_stack.t) =  ()

let debug_update_write_node (session : session) (x : ident) (stk : Concrete_stack.t) = ()

let debug_stack (session : session) (x : ident) (stk : Concrete_stack.t) ((v : dvalue), _) = ()

let raise_if_with_stack (session : session) (x : ident) (stk : Concrete_stack.t) (v : dvalue) = ()

let alert_lookup (session : session) (x : ident) (stk : Concrete_stack.t) = ()

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

let debug_clause ~session x v stk =
  ILog.app (fun m -> m "@[%a = %a@]" Id.pp x pp_dvalue v) ;

  raise_if_with_stack session x stk v ;
  debug_stack session x stk (v, stk) ;
  ()

(* OB: we cannot enter the same stack twice. *)
let rec eval_exp ~session stk env e : denv * dvalue =
  ILog.app (fun m -> m "@[-> %a@]\n" Concrete_stack.pp stk) ;

  let (Expr clauses) = e in
  let denv, vs' =
    List.fold_map ~f:(eval_clause ~session stk) ~init:env clauses
  in
  (denv, List.last_exn vs')

(* OB: once stack is to change, there must be an `eval_exp` *)
and eval_clause ~session stk env clause : denv * dvalue =
  let (Clause (Var (x, _), cbody)) = clause in

  debug_update_write_node session x stk ;

  let (v : dvalue) =
    match cbody with
    | Value_body (Value_function vf) ->
        let retv = FunClosure (x, vf, env) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Value_body (Value_record r) ->
        let retv = RecordClosure (r, env) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Value_body v ->
        let retv = Direct v in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Var_body vx ->
        let (Var (v, _)) = vx in
        let ret_val, ret_stk = fetch_val_with_stk ~session ~stk env vx in
        add_alias (x, stk) (v, ret_stk) session ;
        ret_val
    | Conditional_body (x2, e1, e2) ->
        let e, stk' =
          if fetch_val_to_bool ~session ~stk env x2
          then (e1, Concrete_stack.push (x, cond_fid true) stk)
          else (e2, Concrete_stack.push (x, cond_fid false) stk)
        in
        let ret_env, ret_val = eval_exp ~session stk' env e in
        let (Var (ret_id, _) as last_v) = Ast_tools.retv e in
        let _, ret_stk = fetch_val_with_stk ~session ~stk:stk' ret_env last_v in
        add_alias (x, stk) (ret_id, ret_stk) session ;
        ret_val
    | Input_body ->
        (* TODO: the interpreter may propagate the dummy value (through the value should never be used in any control flow)  *)
        (* To change: Partial eval should view input as an unknown*)
        let n = 1 in
        let retv = Direct (Value_int n) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Appl_body (vx1, (Var (x2, _) as vx2)) -> (
        match fetch_val ~session ~stk env vx1 with
        | FunClosure (fid, Function_value (Var (arg, _), body), fenv) ->
            let v2, v2_stk = fetch_val_with_stk ~session ~stk env vx2 in
            let stk2 = Concrete_stack.push (x, fid) stk in
            let env2 = Ident_map.add arg (v2, stk) fenv in
            add_alias (arg, stk) (x2, v2_stk) session ;
            let ret_env, ret_val = eval_exp ~session stk2 env2 body in
            let (Var (ret_id, _) as last_v) = Ast_tools.retv body in
            let _, ret_stk =
              fetch_val_with_stk ~session ~stk:stk2 ret_env last_v
            in
            add_alias (x, stk) (ret_id, ret_stk) session ;
            ret_val
        | _ -> failwith "app to a non fun")
    | Match_body (vx, p) ->
        let retv = Direct (Value_bool (check_pattern ~session ~stk env vx p)) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Projection_body (v, key) -> (
        match fetch_val ~session ~stk env v with
        | RecordClosure (Record_value r, denv) ->
            let (Var (proj_x, _) as vv) = Ident_map.find key r in
            let dvv, vv_stk = fetch_val_with_stk ~session ~stk denv vv in
            add_alias (x, stk) (proj_x, vv_stk) session ;
            dvv
        | Direct (Value_record (Record_value _record)) ->
            (* let vv = Ident_map.find key record in
               fetch_val env vv *)
            failwith "project should also have a closure"
        | _ -> failwith "project on a non record")
    | Not_body vx ->
        let v = fetch_val_to_direct ~session ~stk env vx in
        let bv =
          match v with
          | Value_bool b -> Value_bool (not b)
          | _ -> failwith "incorrect not"
        in
        let retv = Direct bv in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Binary_operation_body (vx1, op, vx2) ->
        let v1 = fetch_val_to_direct ~session ~stk env vx1
        and v2 = fetch_val_to_direct ~session ~stk env vx2 in
        let v =
          match (op, v1, v2) with
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
              Value_bool (Bool.( = ) b1 b2)
          | Binary_operator_and, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 && b2)
          | Binary_operator_or, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 || b2)
          | _, _, _ -> failwith "incorrect binop"
        in
        let retv = Direct v in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
        (* | Abort_body ->
             raise @@ Found_abort x
           | Assert_body vx ->
             let v = fetch_val_to_direct ~session ~stk env vx in
             let bv =
               match v with
               | Value_bool b -> Value_bool b
               | _ -> failwith "failed assert"
             in
             Direct bv *)
        (* TODO: What should the interpreter do with an assume statement? *)
    | Abort_body -> (
        let ab_v = AbortClosure env in
        let () = add_val_def_mapping (x, stk) (cbody, ab_v) session in
         raise @@ Found_abort ab_v)
    | Assert_body _ | Assume_body _ ->
        let retv = Direct (Value_bool true) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    (* failwith "not supported yet" *)
  in
  debug_clause ~session x v stk ;
  (Ident_map.add x (v, stk) env, v)

and fetch_val_with_stk ~session ~stk env (Var (x, _)) :
    dvalue * Concrete_stack.t =
  let res = Ident_map.find x env in
  debug_update_read_node session x stk ;
  res

and fetch_val ~session ~stk env x : dvalue =
  fst (fetch_val_with_stk ~session ~stk env x)

and fetch_val_to_direct ~session ~stk env vx : value =
  match fetch_val ~session ~stk env vx with
  | Direct v -> v
  | _ -> failwith "eval to non direct value"

and fetch_val_to_bool ~session ~stk env vx : bool =
  match fetch_val ~session ~stk env vx with
  | Direct (Value_bool b) -> b
  | _ -> failwith "eval to non bool"

and check_pattern ~session ~stk env vx pattern : bool =
  let is_pass =
    match (fetch_val ~session ~stk env vx, pattern) with
    | Direct (Value_int _), Int_pattern -> true
    | Direct (Value_bool _), Bool_pattern -> true
    | Direct (Value_function _), _ -> failwith "fun must be a closure"
    | Direct (Value_record _), _ -> failwith "record must be a closure"
    | RecordClosure (Record_value record, _), Rec_pattern key_set ->
        Ident_set.for_all (fun id -> Ident_map.mem id record) key_set
    | RecordClosure (Record_value record, _), Strict_rec_pattern key_set ->
        Ident_set.equal key_set (Ident_set.of_enum @@ Ident_map.keys record)
    | FunClosure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass

let eval session e =
  let empty_env = Ident_map.empty in
  try
    let v = snd (eval_exp ~session Concrete_stack.empty empty_env e) in
    raise (Terminate v)
  with
  | Reach_max_step (x, stk) ->
      Fmt.epr "Reach max steps\n" ;
      (* alert_lookup target_stk x stk session.lookup_alert; *)
      raise (Reach_max_step (x, stk))
  | Run_the_same_stack_twice (x, stk) ->
      Fmt.epr "Run into the same stack twice\n" ;
      alert_lookup session x stk ;
      raise (Run_the_same_stack_twice (x, stk))
  | Run_into_wrong_stack (x, stk) ->
      Fmt.epr "Run into wrong stack\n" ;
      alert_lookup session x stk ;
      raise (Run_into_wrong_stack (x, stk))

(* toplevel eval assuming Plain mode, returning dvalue directly instead of exception *)
(* Assumes that default session has no max_step *)
let [@warning "-8"] teval e = 
  let empty_env = Ident_map.empty in
    match snd (eval_exp ~session:(make_default_session ()) Concrete_stack.empty empty_env e) with
    | Direct v -> v
;;

(* let unparse_val = function
  | Value_record Record_value r ->
  | Value_function Function_value x e ->
  | Value_int v -> string_of_int v
  | Value_bool True -> "True"
  | Value_bool False -> "False"

and unparse (ast : expr) = *)

let parse = Jayil_parser.Parse.parse_program_str;;

let unparse = Jayil.Ast_pp.show_value;;
let unparse_expr = Jayil.Ast_pp.show_expr;;

let parse_eval (a : string) = a |> parse |> teval;;
let parse_eval_x (a : string) =
  try
    a |> parse |> (fun ast -> eval (make_default_session ()) ast) 
  with 
    | Terminate Direct v -> v
;;


let parse_eval_unparse (a : string) = a |> parse |> teval |> unparse;;
let peu = parse_eval_unparse;;
let parse_eval_print (a : string) = a |> peu |> print_endline;; (* print_endline "";; *)
let rep = parse_eval_print;;