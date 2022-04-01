open Core
open Odefa_ast
open Ast

exception Found_target of value
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

type session = {
  input_feeder : Id.t * Concrete_stack.t -> int option;
  target : Id.t * Concrete_stack.t;
  step : int ref;
  max_step : int option;
  debug_graph : bool;
  node_set : (Lookup_key.t, bool) Hashtbl.t;
  node_get : (Lookup_key.t, int) Hashtbl.t;
  rstk_picked : (Rstack.t, bool) Hashtbl.t;
}

type dvalue =
  | Direct of value
  | FunClosure of Ident.t * function_value * denv
  | RecordClosure of record_value * denv

and dvalue_with_stack = dvalue * Concrete_stack.t
and denv = dvalue_with_stack Ident_map.t

let pp_dvalue oc = function
  | Direct v, _ -> Odefa_ast.Ast_pp.pp_value oc v
  | FunClosure _, _ -> Format.fprintf oc "(fc)"
  | RecordClosure _, _ -> Format.fprintf oc "(rc)"

let cond_fid b = if b then Ident "$tt" else Ident "$ff"

let update_read_node target_stk x stk (node_get : (Lookup_key.t, int) Hashtbl.t)
    =
  let r_stk = Rstack.relativize target_stk stk in
  let key = Lookup_key.of_parts2 [ x ] r_stk in
  (* Fmt.pr "@[Update Get to %a@]\n" Lookup_key.pp key; *)
  Hashtbl.update node_get key ~f:(function None -> 1 | Some n -> n + 1)

let update_write_node target_stk x stk
    (node_set : (Lookup_key.t, bool) Hashtbl.t) =
  let r_stk = Rstack.relativize target_stk stk in
  let key = Lookup_key.of_parts2 [ x ] r_stk in
  (* Fmt.pr "@[Update Set to %a@]\n" Lookup_key.pp key; *)
  Hashtbl.add_exn node_set ~key ~data:true

let alert_lookup target_stk x stk lookup_alert =
  let r_stk = Rstack.relativize target_stk stk in
  let key = Lookup_key.of_parts2 [ x ] r_stk in
  Fmt.epr "@[Update Alert to %a\t%a@]\n" Lookup_key.pp key Concrete_stack.pp stk ;
  Hash_set.add lookup_alert key

let value_of_dvalue = function
  | Direct v, _ -> v
  | FunClosure (_fid, fv, _env), _ -> Value_function fv
  | RecordClosure (r, _env), _ -> Value_record r

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

(* OB: we cannot enter the same stack twice. *)
let rec eval_exp ~session stk env e : dvalue =
  (* Fmt.(pr "\n@[-> %a@]\n" Concrete_stack.pp stk); *)
  let _target_x, target_stk = session.target in
  let r_stk = Rstack.relativize target_stk stk in
  Hashtbl.change session.rstk_picked r_stk ~f:(function
    | Some true -> Some false
    | Some false -> raise (Run_into_wrong_stack (Ast_tools.first_id e, stk))
    | None -> None) ;

  (* raise (Run_into_wrong_stack (Ast_tools.first_id e, stk))); *)
  let (Expr clauses) = e in
  let _, vs' =
    (* List.fold_left_map (eval_clause ~input_feeder ~target stk) env clauses *)
    List.fold_map ~f:(eval_clause ~session stk) ~init:env clauses
  in
  List.last_exn vs'

(* OB: once stack is to change, there must be an `eval_exp` *)
and eval_clause ~session stk env clause : denv * dvalue =
  (* Fmt.(pr "---%d---\n" !(session.step)); *)
  let (Clause (Var (x, _), cbody)) = clause in
  (match session.max_step with
  | None -> ()
  | Some t ->
      Int.incr session.step ;
      if !(session.step) > t then raise (Reach_max_step (x, stk)) else ()) ;
  let target_x, target_stk = session.target in

  if session.debug_graph
  then
    update_write_node target_stk x stk session.node_set
    (* ;
       Fmt.pr "@[%a = _@]\n" Id.pp x *)
  else () ;

  let (v_pre : dvalue) =
    match cbody with
    | Value_body (Value_function vf) -> FunClosure (x, vf, env)
    | Value_body (Value_record r) -> RecordClosure (r, env)
    | Value_body v -> Direct v
    | Var_body vx -> fetch_val ~session ~stk env vx
    | Conditional_body (x2, e1, e2) ->
        let e, stk' =
          if fetch_val_to_bool ~session ~stk env x2
          then (e1, Concrete_stack.push (x, cond_fid true) stk)
          else (e2, Concrete_stack.push (x, cond_fid false) stk)
        in
        eval_exp ~session stk' env e
    | Input_body ->
        (* TODO: the interpreter may propagate the dummy value (through the value should never be used in any control flow)  *)
        let n = Option.value ~default:42 (session.input_feeder (x, stk)) in
        Direct (Value_int n)
    | Appl_body (vx1, vx2) -> (
        match fetch_val ~session ~stk env vx1 with
        | FunClosure (fid, Function_value (Var (arg, _), body), fenv) ->
            let v2 = fetch_val ~session ~stk env vx2 in
            let stk2 = Concrete_stack.push (x, fid) stk in
            let env2 = Ident_map.add arg (v2, stk) fenv in
            eval_exp ~session stk2 env2 body
        | _ -> failwith "app to a non fun")
    | Match_body (vx, p) ->
        Direct (Value_bool (check_pattern ~session ~stk env vx p))
    | Projection_body (v, key) -> (
        match fetch_val ~session ~stk env v with
        | RecordClosure (Record_value r, denv) ->
            let vv = Ident_map.find key r in
            fetch_val ~session ~stk denv vv
        | Direct (Value_record (Record_value _record)) ->
            (* let vv = Ident_map.find key record in
               fetch_val env vv *)
            failwith "project should also have a closure"
        | _ -> failwith "project on a non record")
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
          | Binary_operator_xor, Value_bool b1, Value_bool b2 ->
              Value_bool (Bool.( <> ) b1 b2)
          | _, _, _ -> failwith "incorrect binop"
        in
        Direct v
    | Abort_body | Assume_body _ | Assert_body _ -> failwith "not supported yet"
  in
  let v = (v_pre, stk) in

  if Ident.equal target_x x
  then
    if Concrete_stack.equal_flip target_stk stk
    then raise (Found_target (value_of_dvalue v))
    else
      Fmt.(
        pr "found %a at stack %a, expect %a" pp_ident x Concrete_stack.pp
          target_stk Concrete_stack.pp stk)
  else () ;

  if session.debug_graph
  then
    let rstk = Rstack.relativize target_stk stk in
    Fmt.pr "@[%a = %a\t\t R = %a@]\n" Id.pp x pp_dvalue v Rstack.pp rstk
  else () ;

  (Ident_map.add x v env, v_pre)

and fetch_val ~session ~stk env (Var (x, _)) : dvalue =
  let v, _ = Ident_map.find x env in
  let _target_x, target_stk = session.target in
  update_read_node target_stk x stk session.node_get ;
  v

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
    | Direct (Value_function _), _ -> failwith "must be a closure"
    | Direct (Value_record (Record_value record)), Rec_pattern key_set ->
        Ident_set.for_all
          (fun k ->
            match Ident_map.Exceptionless.find k record with
            | Some _ -> true
            | None -> false)
          key_set
    | FunClosure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass

let eval ~(state : Global_state.t) ~(config : Global_config.t)
    ?(input_feeder = Fn.const None) ~target ~max_step e =
  let session =
    {
      input_feeder;
      target;
      max_step;
      step = ref 0;
      debug_graph = config.debug_graph;
      node_set = state.node_set;
      node_get = state.node_get;
      rstk_picked = state.rstk_picked;
    }
  in

  let empty_env = Ident_map.empty in
  let _target_x, target_stk = target in
  try
    let _ = eval_exp ~session Concrete_stack.empty empty_env e in
    Fmt.epr "No target val" ;
    None
  with
  | Found_target v -> Some v
  | Reach_max_step (_x, _stk) ->
      Fmt.epr "Reach max steps\n" ;
      (* alert_lookup target_stk x stk state.lookup_alert; *)
      None
  | Run_the_same_stack_twice (x, stk) ->
      Fmt.epr "Run into the same stack twice\n" ;
      alert_lookup target_stk x stk state.lookup_alert ;
      None
  | Run_into_wrong_stack (x, stk) ->
      Fmt.epr "Run into wrong stack\n" ;
      alert_lookup target_stk x stk state.lookup_alert ;
      None
