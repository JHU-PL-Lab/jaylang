open Core
open Graph
open Dj_common
open Log.Export
open Jayil
open Ast

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

let pp_dvalue oc = function
  | Direct v -> Jayil.Ast_pp.pp_value oc v
  | FunClosure _ -> Format.fprintf oc "(fc)"
  | RecordClosure _ -> Format.fprintf oc "(rc)"
  | AbortClosure _ -> Format.fprintf oc "(abort)"

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

module G = Imperative.Digraph.ConcreteBidirectional (Id_with_stack)

type session = {
  (* mode *)
  input_feeder : Input_feeder.t;
  mode : mode;
  (* tuning *)
  step : int ref;
  max_step : int option;
  (* book-keeping *)
  alias_graph : G.t;
  val_def_map : (Id_with_stack.t, clause_body * dvalue) Hashtbl.t;
  (* debug *)
  is_debug : bool;
  node_set : (Lookup_key.t, bool) Hashtbl.t;
  node_get : (Lookup_key.t, int) Hashtbl.t;
  rstk_picked : (Rstack.t, bool) Hashtbl.t;
  lookup_alert : Lookup_key.t Hash_set.t;
  (* debug heavily *)
  is_check_per_step : bool;
}

let make_default_session () =
  {
    input_feeder = Fn.const 42;
    mode = Plain;
    max_step = None;
    is_debug = false;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Hashtbl.create (module Id_with_stack);
    node_set = Hashtbl.create (module Lookup_key);
    node_get = Hashtbl.create (module Lookup_key);
    rstk_picked = Hashtbl.create (module Rstack);
    lookup_alert = Hash_set.create (module Lookup_key);
    is_check_per_step = false;
  }

let create_session ?max_step target_stk (state : Global_state.t)
    (config : Global_config.t) input_feeder : session =
  {
    input_feeder;
    mode = With_full_target (config.target, target_stk);
    max_step;
    is_debug = config.debug_graph;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Hashtbl.create (module Id_with_stack);
    node_set = state.node_set;
    node_get = state.node_get;
    rstk_picked = state.rstk_picked;
    lookup_alert = state.lookup_alert;
    is_check_per_step = false;
  }

let expected_input_session ?(is_check_per_step = false) input_feeder target_x =
  {
    (make_default_session ()) with
    input_feeder;
    mode = With_target_x target_x;
    is_check_per_step;
  }

let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(* This function will add a directed edge x1 -> x2 in the alias graph. Thus
   x1 here needs to be the *later* defined variable. *)
let add_alias x1 x2 session : unit =
  let alias_graph = session.alias_graph in
  G.add_edge alias_graph x1 x2

let add_val_def_mapping x vdef session : unit =
  let val_def_mapping = session.val_def_map in
  (* let () = print_endline @@ "This is adding a mapping, here's the key: " in *)
  (* let () = print_endline @@ show_ident_with_stack x in *)
  Hashtbl.add_exn ~key:x ~data:vdef val_def_mapping
(* match added with
   | `Duplicate ->
     let v = Hashtbl.find_exn val_def_mapping x in
     if Ast.equal_clause_body v vdef  then () else failwith "Should be the same value"
   | `Ok -> () *)

let debug_update_read_node session x stk =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let key = Lookup_key.of2 x r_stk in
      (* Fmt.pr "@[Update Get to %a@]\n" Lookup_key.pp key; *)
      Hashtbl.update session.node_get key ~f:(function
        | None -> 1
        | Some n -> n + 1)
  | _, _ -> ()

let debug_update_write_node session x stk =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let key = Lookup_key.of2 x r_stk in
      (* Fmt.pr "@[Update Set to %a@]\n" Lookup_key.pp key; *)
      Hashtbl.add_exn session.node_set ~key ~data:true
  | _, _ -> ()

let debug_stack session x stk (v, _) =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let rstk = Rstack.relativize target_stk stk in
      Fmt.pr "@[%a = %a\t\t R = %a@]\n" Id.pp x pp_dvalue v Rstack.pp rstk
  | _, _ -> ()

let raise_if_with_stack session x stk v =
  match session.mode with
  | With_full_target (target_x, target_stk) when Ident.equal target_x x ->
      if Concrete_stack.equal_flip target_stk stk
      then raise (Found_target { x; stk; v })
      else
        Fmt.(
          pr "found %a at stack %a, expect %a\n" pp_ident x Concrete_stack.pp
            target_stk Concrete_stack.pp stk)
  | With_target_x target_x when Ident.equal target_x x ->
      raise (Found_target { x; stk; v })
  | _ -> ()

let alert_lookup session x stk =
  match session.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let key = Lookup_key.of2 x r_stk in
      Fmt.epr "@[Update Alert to %a\t%a@]\n" Lookup_key.pp key Concrete_stack.pp
        stk ;
      Hash_set.add session.lookup_alert key
  | _ -> ()

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

(* OB: we cannot enter the same stack twice. *)
let rec eval_exp_verbose ~session stk env e : denv * dvalue =
  ILog.app (fun m -> m "@[-> %a@]\n" Concrete_stack.pp stk) ;
  (match session.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      Hashtbl.change session.rstk_picked r_stk ~f:(function
        | Some true -> Some false
        | Some false -> raise (Run_into_wrong_stack (Ast_tools.first_id e, stk))
        | None -> None)
  | _ -> ()) ;

  (* raise (Run_into_wrong_stack (Ast_tools.first_id e, stk))); *)
  let (Expr clauses) = e in
  let denv, vs' =
    (* List.fold_left_map (eval_clause ~input_feeder ~target stk) env clauses *)
    List.fold_map ~f:(eval_clause ~session stk) ~init:env clauses
  in
  (denv, List.last_exn vs')

and eval_exp ~session stk env e : dvalue =
  snd (eval_exp_verbose ~session stk env e)

(* OB: once stack is to change, there must be an `eval_exp` *)
and eval_clause ~session stk env clause : denv * dvalue =
  let (Clause (Var (x, _), cbody)) = clause in
  (match session.max_step with
  | None -> ()
  | Some t ->
      Int.incr session.step ;
      if !(session.step) > t then raise (Reach_max_step (x, stk)) else ()) ;

  debug_update_write_node session x stk ;

  let (v_pre : dvalue) =
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
        (* let () = print_endline @@ "This is adding alias mapping in var body" in
           let () = print_endline @@ show_ident_with_stack (x, stk) in
           let () = print_endline @@ show_ident_with_stack (v, ret_stk) in *)
        add_alias (x, stk) (v, ret_stk) session ;
        ret_val
    | Conditional_body (x2, e1, e2) ->
        let e, stk' =
          if fetch_val_to_bool ~session ~stk env x2
          then (e1, Concrete_stack.push (x, cond_fid true) stk)
          else (e2, Concrete_stack.push (x, cond_fid false) stk)
        in
        let ret_env, ret_val = eval_exp_verbose ~session stk' env e in
        let (Var (ret_id, _) as last_v) = Ast_tools.retv e in
        let _, ret_stk = fetch_val_with_stk ~session ~stk:stk' ret_env last_v in
        (* let () = print_endline @@ "This is adding alias mapping in conditional body" in
           let () = print_endline @@ show_ident_with_stack (x, stk) in
           let () = print_endline @@ show_ident_with_stack (ret_id, ret_stk) in *)
        add_alias (x, stk) (ret_id, ret_stk) session ;
        ret_val
    | Input_body ->
        (* TODO: the interpreter may propagate the dummy value (through the value should never be used in any control flow)  *)
        let n = session.input_feeder (x, stk) in
        let retv = Direct (Value_int n) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    | Appl_body (vx1, (Var (x2, _) as vx2)) -> (
        match fetch_val ~session ~stk env vx1 with
        | FunClosure (fid, Function_value (Var (arg, _), body), fenv) ->
            let v2, v2_stk = fetch_val_with_stk ~session ~stk env vx2 in
            let stk2 = Concrete_stack.push (x, fid) stk in
            let env2 = Ident_map.add arg (v2, stk) fenv in
            (* let () = print_endline @@ "This is adding alias mapping in fun arg" in
               let () = print_endline @@ show_ident_with_stack (arg, stk) in
               let () = print_endline @@ show_ident_with_stack (x2, v2_stk) in *)
            add_alias (arg, stk) (x2, v2_stk) session ;
            let ret_env, ret_val = eval_exp_verbose ~session stk2 env2 body in
            let (Var (ret_id, _) as last_v) = Ast_tools.retv body in
            let _, ret_stk =
              fetch_val_with_stk ~session ~stk:stk2 ret_env last_v
            in
            (* let () = print_endline @@ "This is adding alias mapping in fun ret" in
               let () = print_endline @@ show_ident_with_stack (x, stk) in
               let () = print_endline @@ show_ident_with_stack (ret_id, ret_stk) in
               let () = print_endline @@ "pair added" in *)
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
            (* let () = add_val_def_mapping (x, stk) cbody session in
               let vv = Ident_map.find key r in
               fetch_val ~session ~stk denv vv *)
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
        match session.mode with
        | Plain -> raise @@ Found_abort ab_v
        | With_target_x target ->
            if Id.equal target x
            then raise @@ Found_target { x; stk; v = ab_v }
            else raise @@ Found_abort ab_v
        | With_full_target (target, tar_stk) ->
            (* let () = print_endline @@ "target equal: " ^ string_of_bool (Id.equal target x) in
               let () = print_endline @@ "stack equal: " ^ string_of_bool (Concrete_stack.equal tar_stk stk) in
               let () = print_endline @@ "-------------" in
               let () = print_endline @@ "expected stack  : " ^ Concrete_stack.show tar_stk in
               let () = print_endline @@ "actual stack : " ^ Concrete_stack.show stk in
               let () = print_endline @@ "-------------" in *)
            if Id.equal target x && Concrete_stack.equal_flip tar_stk stk
            then raise @@ Found_target { x; stk; v = ab_v }
            else raise @@ Found_abort ab_v)
    | Assert_body _ | Assume_body _ ->
        let retv = Direct (Value_bool true) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    (* failwith "not supported yet" *)
  in
  let v = (v_pre, stk) in

  ILog.app (fun m -> m "@[%a = %a@]" Id.pp x pp_dvalue v_pre) ;

  raise_if_with_stack session x stk v_pre ;

  debug_stack session x stk v ;

  (Ident_map.add x v env, v_pre)

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
    | FunClosure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass

let eval session e =
  let empty_env = Ident_map.empty in
  try
    let v = eval_exp ~session Concrete_stack.empty empty_env e in
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