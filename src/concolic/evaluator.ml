open Core
open Jayil.Ast
open Dvalue (* just to expose constructors *)

open Concolic_exceptions.Make (Session.Symbolic)

module ILog = Dj_common.Log.Export.ILog
module CLog = Dj_common.Log.Export.CLog

(*
  ------------------------------
  BEGIN HELPERS TO READ FROM ENV   
  ------------------------------
*)

module Fetch =
  struct

    let fetch_val_with_depth env (Var (x, _)) : Dvalue.t * Fun_depth.t =
      Ident_map.find x env (* find the variable and stack in the environment *)

    let fetch_val env x : Dvalue.t =
      fst (fetch_val_with_depth env x) (* find variable and stack, then discard stack *)

    let fetch_depth env x : Fun_depth.t =
      snd (fetch_val_with_depth env x) (* find variable and stack, then discard variable *)

    let fetch_val_to_direct env vx : value =
      match fetch_val env vx with
      | Direct v -> v
      | _ -> failwith "eval to non direct value" (* TODO: add type mismatch here *)

    let check_pattern env vx pattern : bool =
      match (fetch_val env vx, pattern) with
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

  end

(*
  ----------------------------
  END HELPERS TO READ FROM ENV   
  ----------------------------
*)


(*
  ----------
  BEGIN EVAL
  ----------

  This section is basically an interpreter injected with concolic logic.
  It is an evaluation within a single concolic session.
*)

let make_key = Concolic_key.generate

let rec eval_exp
  ~(conc_session : Session.Concrete.t) (* Note: is mutable *)
  ~(symb_session : Session.Symbolic.t)
  (fun_depth : Fun_depth.t)
  (env : Dvalue.denv)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  ILog.app (fun m -> m "@[-> %d]\n" (Fun_depth.to_int fun_depth));
  let Expr clauses = e in
  let (denv, conc_session), vs =
    List.fold_map
      clauses
      ~init:(env, symb_session)
      ~f:(fun (env, pt) clause ->
        let denv, v, pt = eval_clause ~conc_session ~symb_session:pt fun_depth env clause
        in (denv, pt), v) 
  in
  (denv, List.last_exn vs, conc_session)

and eval_clause
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (fun_depth : Fun_depth.t)
  (env : Dvalue.denv)
  (clause : clause)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  let Clause (Var (x, _), cbody) = clause in
  begin
  match conc_session.max_step with 
  | None -> ()
  | Some max_step ->
      Int.incr conc_session.step;
      if !(conc_session.step) > max_step
      then raise (Reach_max_step (x, Session.Symbolic.reach_max_step symb_session))
      else ()
  end;
  
  let x_key = make_key x fun_depth in
  let (v, symb_session) : Dvalue.t * Session.Symbolic.t =
    match cbody with
    | Value_body ((Value_function vf) as v) ->
      (* x = fun ... ; *)
      FunClosure (x, vf, env), Session.Symbolic.add_key_eq_val symb_session x_key v
    | Value_body ((Value_record r) as v) ->
      (* x = { ... } ; *)
      RecordClosure (r, env), Session.Symbolic.add_key_eq_val symb_session x_key v
    | Value_body v -> 
      (* x = <bool or int> ; *)
      Direct v, Session.Symbolic.add_key_eq_val symb_session x_key v
    | Var_body vx ->
      (* x = y ; *)
      let Var (y, _) = vx in
      let ret_val, ret_depth = Fetch.fetch_val_with_depth env vx in
      let y_key = make_key y ret_depth in 
      ret_val, Session.Symbolic.add_alias symb_session x_key y_key
    | Conditional_body (cx, e1, e2) -> 
      (* x = if y then e1 else e2 ; *)
      let Var (y, _) = cx in
      let cond_val, condition_depth = Fetch.fetch_val_with_depth env cx in
      let cond_bool =
        match cond_val with
        | Direct (Value_bool b) -> b 
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let condition_key = make_key y condition_depth in
      let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key = condition_key ; direction = Branch.Direction.of_bool cond_bool } in

      (* enter/hit branch *)
      let symb_session = Session.Symbolic.hit_branch symb_session this_branch in

      let e = if cond_bool then e1 else e2 in

      (* note that [conc_session] gets mutated when evaluating the branch *)
      let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session fun_depth env e in
      let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
      let _, ret_stk = Fetch.fetch_val_with_depth ret_env last_v in

      (* say the ret_key is equal to x now, then clear out branch *)
      let ret_key = make_key ret_id ret_stk in
      ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
    | Input_body ->
      (* x = input ; *)
      let n = conc_session.input_feeder (x, fun_depth) in
      let retv = Direct (Value_int n) in
      retv, Session.Symbolic.add_input symb_session x_key retv
    | Appl_body (vf, (Var (x_arg, _) as varg)) -> begin 
      (* x = f y ; *)
      match Fetch.fetch_val env vf with
      | FunClosure (_, Function_value (Var (param, _), body), fenv) ->
        (* Enter the function (internally increases the function depth) *)
        let symb_session = Session.Symbolic.enter_fun symb_session in
        let fun_depth' = Session.Symbolic.get_fun_depth symb_session in

        (* varg is the argument that fills in param *)
        let arg, arg_depth = Fetch.fetch_val_with_depth env varg in
        let env' = Ident_map.add param (arg, fun_depth') fenv in

        (* enter function: say arg is same as param *)
        let key_param = make_key param fun_depth' in
        let key_arg = make_key x_arg arg_depth in
        let symb_session = Session.Symbolic.add_alias symb_session key_param key_arg in

        (* returned value of function *)
        let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session fun_depth' env' body in
        let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv body in
        let ret_stk = Fetch.fetch_depth ret_env last_v in

        (* exit function: *)
        let ret_key = make_key ret_id ret_stk in
        ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
      | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      end
    | Match_body (vy, p) ->
      (* x = y ~ <pattern> ; *)
      let match_res = Value_bool (Fetch.check_pattern env vy p) in
      let Var (y, _) = vy in
      let match_key = make_key y fun_depth in
      Direct match_res, Session.Symbolic.add_match symb_session x_key match_key p
    | Projection_body (v, label) -> begin
      match Fetch.fetch_val env v with
      | RecordClosure (Record_value r, denv) ->
        let Var (proj_x, _) as proj_v = Ident_map.find label r in
        let retv, fun_depth' = Fetch.fetch_val_with_depth denv proj_v in
        let proj_key = make_key proj_x fun_depth' in
        retv, Session.Symbolic.add_alias symb_session x_key proj_key
      | Direct (Value_record (Record_value _record)) ->
        failwith "project should also have a closure"
      | _ -> failwith "project on a non record" (* TODO: type mismatch here *)
      end
    | Not_body vy ->
      (* x = not y ; *)
      let v = Fetch.fetch_val_to_direct env vy in 
      let y_depth = Fetch.fetch_depth env vy in
      let bv =
        match v with
        | Value_bool b -> Value_bool (not b)
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let (Var (y, _)) = vy in
      let y_key = make_key y y_depth in
      Direct bv, Session.Symbolic.add_not symb_session x_key y_key
    | Binary_operation_body (vy, op, vz) ->
      (* x = y op z *)
      let v1 = Fetch.fetch_val_to_direct env vy
      and v2 = Fetch.fetch_val_to_direct env vz in
      let v =
        match op, v1, v2 with
        | Binary_operator_plus, Value_int n1, Value_int n2                  -> Value_int  (n1 + n2)
        | Binary_operator_minus, Value_int n1, Value_int n2                 -> Value_int  (n1 - n2)
        | Binary_operator_times, Value_int n1, Value_int n2                 -> Value_int  (n1 * n2)
        | Binary_operator_divide, Value_int n1, Value_int n2                -> Value_int  (n1 / n2)
        | Binary_operator_modulus, Value_int n1, Value_int n2               -> Value_int  (n1 mod n2)
        | Binary_operator_less_than, Value_int n1, Value_int n2             -> Value_bool (n1 < n2)
        | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> Value_bool (n1 <= n2)
        | Binary_operator_equal_to, Value_int n1, Value_int n2              -> Value_bool (n1 = n2)
        | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> Value_bool (Bool.(b1 = b2))
        | Binary_operator_and, Value_bool b1, Value_bool b2                 -> Value_bool (b1 && b2)
        | Binary_operator_or, Value_bool b1, Value_bool b2                  -> Value_bool (b1 || b2)
        | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> Value_bool (n1 <> n2)
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let Var (y, _) = vy in
      let Var (z, _) = vz in
      let y_depth = Fetch.fetch_depth env vy in
      let z_depth = Fetch.fetch_depth env vz in
      let y_key = make_key y y_depth in
      let z_key = make_key z z_depth in
      Direct v, Session.Symbolic.add_binop symb_session x_key op y_key z_key
    | Abort_body ->
      let ab_v = AbortClosure env in
      raise @@ Found_abort (ab_v, Session.Symbolic.found_abort symb_session) (* no need to "exit" or anything. Just say interpretation stops. *)
    | Assert_body cx | Assume_body cx ->
      let v = Fetch.fetch_val_to_direct env cx in 
      let b =
        match v with
        | Value_bool b -> b
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let Var (y, _) = cx in 
      let key = make_key y (Fetch.fetch_depth env cx) in
      let symb_session = Session.Symbolic.found_assume symb_session key in
      if not b
      then
        raise @@ Found_failed_assume (Session.Symbolic.fail_assume symb_session) (* fail the assume that was just found *)
      else
        Direct (Value_bool b), symb_session
  in
  (Ident_map.add x (v, fun_depth) env, v, symb_session)

let eval_exp_default
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  eval_exp
    ~conc_session 
    ~symb_session
    Fun_depth.zero
    Ident_map.empty (* empty environment *)
    e

(* Evaluate the expression and return resulting concolic session. Print and discard output. May bubble exception *)
let try_eval_exp_default
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Session.Symbolic.t
  =
  try
    (* might throw exception which is to be caught below *)
    let _, v, symb_session = eval_exp_default ~conc_session ~symb_session e in
    CLog.app (fun m -> m "Evaluated to: %a\n" Dvalue.pp v);
    symb_session
  with
  | Found_abort (_, symb_session) ->
      CLog.app (fun m -> m "Found abort in interpretation\n");
      symb_session
  | Type_mismatch symb_session ->
      CLog.app (fun m -> m "Type mismatch in interpretation\n");
      symb_session
  | Reach_max_step (_, symb_session) ->
      CLog.app (fun m -> m "Reach max steps\n");
      symb_session
  | Found_failed_assume symb_session
  | Found_failed_assert symb_session ->
      CLog.app (fun m -> m "Found failed assume or assert\n");
      symb_session
  | Run_the_same_stack_twice (x, stk) -> (* bubbles exception *)
      Fmt.epr "Run into the same stack twice\n" ;
      raise (Run_the_same_stack_twice (x, stk))
  | Run_into_wrong_stack (x, stk) -> (* bubble exception *)
      Fmt.epr "Run into wrong stack\n" ;
      raise (Run_into_wrong_stack (x, stk))


(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple symbolic sessions, trying to hit the branches.
*)
let rec loop (e : expr) (prev_session : Session.t) : Session.Status.t Lwt.t =
  let%lwt () = Lwt.pause () in
  Session.next prev_session
  |> begin function
    | `Done status ->
      CLog.app (fun m -> m "\n------------------------------\nFinishing concolic evaluation...\n\n");
      CLog.app (fun m -> m "Ran %d interpretations.\n" (Session.run_num prev_session));
      CLog.app (fun m -> m "Session status: %s.\n" (Session.Status.to_string status));
      Lwt.return status
    | `Next (session, symb_session, conc_session) ->
      CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session));
      let t0 = Caml_unix.gettimeofday () in
      let resulting_symbolic = try_eval_exp_default ~conc_session ~symb_session e in
      let t1 = Caml_unix.gettimeofday () in
      CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0));
      loop e
      @@ Session.accum_symbolic session resulting_symbolic
    end

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let lwt_eval : (Jayil.Ast.expr -> Session.Status.t Lwt.t) Options.Fun.t =
  let f =
    fun (r : Options.t) ->
      fun (e : Jayil.Ast.expr) ->
        if not r.random then Random.init seed;
        CLog.app (fun m -> m "\nStarting concolic execution...\n");
        (* Repeatedly evaluate program *)
        Concolic_riddler.reset ();
        Lwt_unix.with_timeout r.global_timeout_sec
        @@ fun () ->
          loop e
          @@ Options.Fun.appl Session.with_options r Session.empty
  in
  Options.Fun.make f