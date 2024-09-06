open Core
open Jayil.Ast
open Dvalue (* just to expose constructors *)
open Cstate

module ILog = Dj_common.Log.Export.ILog
module CLog = Dj_common.Log.Export.CLog

(* Ident for conditional bool. *)
let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(*
  ------------------------------
  BEGIN HELPERS TO READ FROM ENV   
  ------------------------------
*)

module Fetch =
  struct

    let fetch_val_with_key env (Var (x, _)) : Dvalue.t * Concolic_key.t =
      Ident_map.find x env (* find the variable and key in the environment *)

    let fetch_val env x : Dvalue.t =
      fst (fetch_val_with_key env x) (* find variable and key, then discard stack *)

    let fetch_key env x : Concolic_key.t =
      snd (fetch_val_with_key env x) (* find variable and key, then discard variable *)

    let check_pattern env vx pattern : bool =
      match (fetch_val env vx, pattern) with
      | Direct (Value_int _), Int_pattern -> true
      | Direct (Value_bool _), Bool_pattern -> true
      | Direct (Value_function _), _ -> failwith "fun must be a closure"
      | Direct (Value_record _), _ -> failwith "record must be a closure"
      | RecordClosure (Record_value record, _), Rec_pattern label_set ->
          Ident_set.for_all (fun id -> Ident_map.mem id record) label_set
      | RecordClosure (Record_value record, _), Strict_rec_pattern label_set ->
          Ident_set.equal label_set (Ident_set.of_enum @@ Ident_map.keys record)
      | FunClosure _, Fun_pattern -> true
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

let rec eval_exp
  ~(conc_session : Session.Concrete.t) (* Note: is mutable *)
  (env : Dvalue.denv)
  (Expr clauses : expr)
  : (Dvalue.denv * Dvalue.t) m
  =
  List.fold
    clauses
    ~init:(return (env, Direct (Value_int 00))) (* safe to use default value because empty clause list is parse error *)
    ~f:(fun acc_m clause ->
      let%bind (env, _) = acc_m in
      eval_clause ~conc_session env clause)

and eval_clause
  ~(conc_session : Session.Concrete.t)
  (env : Dvalue.denv)
  (Clause (Var (x, _), cbody) : clause)
  : (Dvalue.denv * Dvalue.t) m
  =
  Session.Concrete.incr_step conc_session;
  let%bind () =
    if Session.Concrete.is_max_step conc_session
    then let%bind () = modify Session.Symbolic.reach_max_step in reach_max_step
    else return ()
  in
  
  let x_key = Concolic_key.create x conc_session.step in
  let v_m : Dvalue.t m =
    match cbody with
    | Value_body ((Value_function vf) as v) ->
      (* x = fun ... ; *)
      let%bind _ = modify @@ Session.Symbolic.add_key_eq_val x_key v in
      return @@ FunClosure (x, vf, env)
    | Value_body ((Value_record r) as v) ->
      (* x = { ... } ; *)
      let%bind () = modify @@ Session.Symbolic.add_key_eq_val x_key v in
      return @@ RecordClosure (r, env)
    | Value_body v -> 
      (* x = <bool or int> ; *)
      let%bind () = modify @@ Session.Symbolic.add_key_eq_val x_key v in
      return @@ Direct v
    | Var_body vx ->
      (* x = y ; *)
      let ret_val, ret_key = Fetch.fetch_val_with_key env vx in
      let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
      return ret_val
    | Conditional_body (cx, e1, e2) -> 
      (* x = if y then e1 else e2 ; *)
      let cond_val, condition_key = Fetch.fetch_val_with_key env cx in
      let%bind cond_bool =
        match cond_val with
        | Direct (Value_bool b) -> return b 
        | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
      in
      let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool cond_bool } in

      (* enter/hit branch *)
      let%bind () = modify @@ Session.Symbolic.hit_branch this_branch in

      let e = if cond_bool then e1 else e2 in

      (* note that [conc_session] gets mutated when evaluating the branch *)
      let%bind ret_env, ret_val = eval_exp ~conc_session env e in
      let last_v = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
      let ret_key = Fetch.fetch_key ret_env last_v in

      let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
      return ret_val
    | Input_body ->
      (* x = input ; *)
      let ret_val = Direct (Value_int (conc_session.input_feeder x_key)) in
      let%bind () = modify @@ Session.Symbolic.add_input x_key ret_val in
      return ret_val
    | Appl_body (vf, varg) -> begin 
      (* x = f y ; *)
      match Fetch.fetch_val env vf with
      | FunClosure (_, Function_value (Var (param, _), body), fenv) ->
        (* increment step count so that the key for the parameter gets an identifier different than the clause *)
        Session.Concrete.incr_step conc_session;

        (* varg is the argument that fills in param *)
        let arg, arg_key = Fetch.fetch_val_with_key env varg in
        let param_key = Concolic_key.create param conc_session.step in
        let env' = Ident_map.add param (arg, param_key) fenv in

        (* enter function: say arg is same as param *)
        let%bind () = modify @@ Session.Symbolic.add_alias param_key arg_key in

        (* returned value of function *)
        let%bind (ret_env, ret_val) = eval_exp ~conc_session env' body in
        let last_v = Jayil.Ast_tools.retv body in
        let ret_key = Fetch.fetch_key ret_env last_v in

        (* exit function: *)
        let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
        return ret_val
      | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
      end
    | Match_body (vy, p) ->
      (* x = y ~ <pattern> ; *)
      let match_res = Value_bool (Fetch.check_pattern env vy p) in
      let y_key = Fetch.fetch_key env vy in
      let%bind () = modify @@ Session.Symbolic.add_match x_key y_key p in
      return @@ Direct match_res
    | Projection_body (v, label) -> begin
      match Fetch.fetch_val env v with
      | RecordClosure (Record_value r, denv) ->
        let proj_var = Ident_map.find label r in
        let ret_val, ret_key = Fetch.fetch_val_with_key denv proj_var in
        let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
        return ret_val
      | Direct (Value_record (Record_value _record)) ->
        failwith "project should also have a closure"
      | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
      end
    | Not_body vy ->
      (* x = not y ; *)
      let y_val, y_key = Fetch.fetch_val_with_key env vy in
      let%bind ret_val =
        match y_val with
        | Direct (Value_bool b) -> return @@ Direct (Value_bool (not b))
        | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
      in
      let%bind () = modify @@ Session.Symbolic.add_not x_key y_key in
      return ret_val
    | Binary_operation_body (vy, op, vz) ->
      (* x = y op z *)
      let y, y_key = Fetch.fetch_val_with_key env vy in
      let z, z_key = Fetch.fetch_val_with_key env vz in
      let%bind v =
        let%bind v1, v2 = 
          match y, z with
          | Direct v1, Direct v2 -> return (v1, v2)
          | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
        in
        match op, v1, v2 with
        | Binary_operator_plus, Value_int n1, Value_int n2                  -> return @@ Value_int  (n1 + n2)
        | Binary_operator_minus, Value_int n1, Value_int n2                 -> return @@ Value_int  (n1 - n2)
        | Binary_operator_times, Value_int n1, Value_int n2                 -> return @@ Value_int  (n1 * n2)
        | Binary_operator_divide, Value_int n1, Value_int n2                -> return @@ Value_int  (n1 / n2)
        | Binary_operator_modulus, Value_int n1, Value_int n2               -> return @@ Value_int  (n1 mod n2)
        | Binary_operator_less_than, Value_int n1, Value_int n2             -> return @@ Value_bool (n1 < n2)
        | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> return @@ Value_bool (n1 <= n2)
        | Binary_operator_equal_to, Value_int n1, Value_int n2              -> return @@ Value_bool (n1 = n2)
        | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> return @@ Value_bool (Bool.(b1 = b2))
        | Binary_operator_and, Value_bool b1, Value_bool b2                 -> return @@ Value_bool (b1 && b2)
        | Binary_operator_or, Value_bool b1, Value_bool b2                  -> return @@ Value_bool (b1 || b2)
        | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> return @@ Value_bool (n1 <> n2)
        | _ -> let%bind () = modify Session.Symbolic.reach_max_step in reach_max_step
      in
      let%bind () = modify @@ Session.Symbolic.add_binop x_key op y_key z_key in
      return @@ Direct v
    | Abort_body ->
      let%bind () = modify Session.Symbolic.found_abort in
      found_abort
    | Assert_body cx | Assume_body cx ->
      let cond_val, cond_key = Fetch.fetch_val_with_key env cx in 
      let%bind b =
        match cond_val with
        | Direct (Value_bool b) -> return b
        | _ -> let%bind () = modify Session.Symbolic.found_type_mismatch in type_mismatch
      in
      let%bind () = modify @@ Session.Symbolic.found_assume cond_key in
      if not b
      then let%bind () = modify Session.Symbolic.fail_assume in failed_assume (* fail the assume that was just found *)
      else return cond_val
  in
  let%bind v = v_m in
  return (Ident_map.add x (v, x_key) env, v)

let eval_exp_default
  ~(conc_session : Session.Concrete.t)
  (e : expr)
  : (Dvalue.denv * Dvalue.t) m
  =
  eval_exp
    ~conc_session 
    Ident_map.empty (* empty environment *)
    e

(* Evaluate the expression and return resulting concolic session. Print and discard output. May bubble exception *)
let try_eval_exp_default
  ~(conc_session : Session.Concrete.t)
  (e : expr)
  : unit m
  =
  let%bind (_, v) = eval_exp_default ~conc_session e in
  let%bind s = show (return v) Dvalue.pp in
  CLog.app (fun m -> m "Evaluated to: %s\n" s);
  return ()

(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple symbolic sessions, trying to hit the branches.
*)
let rec loop (e : expr) (prev_session : Session.t) : Session.Status.t Lwt.t =
  let open Lwt.Infix in
  let%lwt () = Lwt.pause () in
  Session.next prev_session
  >>= begin function
      | `Done status ->
        CLog.app (fun m -> m "\n------------------------------\nFinishing concolic evaluation...\n\n");
        CLog.app (fun m -> m "Ran %d interpretations.\n" (Session.run_num prev_session));
        CLog.app (fun m -> m "Session status: %s.\n" (Session.Status.to_string status));
        Lwt.return status
      | `Next (session, symb_session, conc_session) ->
        CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session));
        let t0 = Caml_unix.gettimeofday () in
        let resulting_symbolic, _ = run (try_eval_exp_default ~conc_session e) symb_session in
        let t1 = Caml_unix.gettimeofday () in
        CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0));
        loop e
        @@ Session.accum_symbolic session resulting_symbolic
      end

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let lwt_eval : (Jayil.Ast.expr, Session.Status.t Lwt.t) Options.Fun.t =
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
          @@ Options.Fun.run Session.with_options r Session.empty
  in
  Options.Fun.make f