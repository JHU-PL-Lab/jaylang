open Core
open Jayil.Ast
open Dvalue (* just to expose constructors *)

open Concolic_exceptions.Make (Session.Symbolic)

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
  ~(symb_session : Session.Symbolic.t)
  (env : Dvalue.denv)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  let Expr clauses = e in
  let (denv, symb_session), vs =
    List.fold_map
      clauses
      ~init:(env, symb_session)
      ~f:(fun (env, symb_session) clause ->
        let denv, v, symb_session = eval_clause ~conc_session ~symb_session env clause
        in (denv, symb_session), v) 
  in
  (denv, List.last_exn vs, symb_session)

and eval_clause
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (env : Dvalue.denv)
  (clause : clause)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  let Clause (Var (x, _), cbody) = clause in
  Session.Concrete.incr_step conc_session;
  if conc_session.step > conc_session.max_step
  then raise (Reach_max_step (x, Session.Symbolic.reach_max_step symb_session));
  
  let x_key = Concolic_key.create x conc_session.step in
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
      let ret_val, ret_key = Fetch.fetch_val_with_key env vx in
      ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
    | Conditional_body (cx, e1, e2) -> 
      (* x = if y then e1 else e2 ; *)
      let cond_val, condition_key = Fetch.fetch_val_with_key env cx in
      let cond_bool =
        match cond_val with
        | Direct (Value_bool b) -> b 
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool cond_bool } in

      (* enter/hit branch *)
      let symb_session = Session.Symbolic.hit_branch symb_session this_branch in

      let e = if cond_bool then e1 else e2 in

      (* note that [conc_session] gets mutated when evaluating the branch *)
      let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session env e in
      let last_v = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
      let _, ret_key = Fetch.fetch_val_with_key ret_env last_v in

      ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
    | Input_body ->
      (* x = input ; *)
      let ret_val = Direct (Value_int (conc_session.input_feeder x_key)) in
      ret_val, Session.Symbolic.add_input symb_session x_key ret_val
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
        let symb_session = Session.Symbolic.add_alias symb_session param_key arg_key in

        (* returned value of function *)
        let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session env' body in
        let last_v = Jayil.Ast_tools.retv body in
        let ret_key = Fetch.fetch_key ret_env last_v in

        (* exit function: *)
        ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
      | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      end
    | Match_body (vy, p) ->
      (* x = y ~ <pattern> ; *)
      let match_res = Value_bool (Fetch.check_pattern env vy p) in
      let y_key = Fetch.fetch_key env vy in
      Direct match_res, Session.Symbolic.add_match symb_session x_key y_key p
    | Projection_body (v, label) -> begin
      match Fetch.fetch_val env v with
      | RecordClosure (Record_value r, denv) ->
        let proj_var = Ident_map.find label r in
        let ret_val, ret_key = Fetch.fetch_val_with_key denv proj_var in
        ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
      | Direct (Value_record (Record_value _record)) ->
        failwith "project should also have a closure"
      | _ -> failwith "project on a non record" (* TODO: type mismatch here *)
      end
    | Not_body vy ->
      (* x = not y ; *)
      let y_val, y_key = Fetch.fetch_val_with_key env vy in
      let ret_val =
        match y_val with
        | Direct (Value_bool b) -> Direct (Value_bool (not b))
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      ret_val, Session.Symbolic.add_not symb_session x_key y_key
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
      let y_key = Fetch.fetch_key env vy in
      let z_key = Fetch.fetch_key env vz in
      Direct v, Session.Symbolic.add_binop symb_session x_key op y_key z_key
    | Abort_body ->
      let ab_v = AbortClosure env in
      raise @@ Found_abort (ab_v, Session.Symbolic.found_abort symb_session) (* no need to "exit" or anything. Just say interpretation stops. *)
    | Assert_body cx | Assume_body cx ->
      let cond_val, cond_key = Fetch.fetch_val_with_key env cx in 
      let b =
        match cond_val with
        | Direct (Value_bool b) -> b
        | _ -> raise @@ Type_mismatch (Session.Symbolic.found_type_mismatch symb_session)
      in
      let symb_session = Session.Symbolic.found_assume symb_session cond_key in
      if not b
      then raise @@ Found_failed_assume (Session.Symbolic.fail_assume symb_session) (* fail the assume that was just found *)
      else cond_val, symb_session
  in
  (Ident_map.add x (v, x_key) env, v, symb_session)

let eval_exp_default
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  eval_exp
    ~conc_session 
    ~symb_session
    Ident_map.empty (* empty environment *)
    e

let continue_from_concolic_exception (e : exn) : Session.Symbolic.t =
  try raise e with
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
  | e -> continue_from_concolic_exception e

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
        let resulting_symbolic = try_eval_exp_default ~conc_session ~symb_session e in
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