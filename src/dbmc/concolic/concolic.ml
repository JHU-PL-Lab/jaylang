open Core
open Jayil.Ast
open Dj_common (* exposes Concrete_stack *)
open Dvalue (* just to expose constructors *)

open Concolic_exceptions.Make (Session.Symbolic)

module ILog = Log.Export.ILog

(* Ident for conditional bool. *)
let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(*
  --------------------------------------
  BEGIN DEBUG FUNCTIONS FROM INTERPRETER   
  --------------------------------------

  Unless labeled, I just keep these called "session", when really they're
  a concrete session (see session.mli).
*)

module Debug =
  struct
    let debug_update_read_node session x stk =
      let open Session.Concrete in
      match (session.is_debug, session.mode) with
      | true, Session.Concrete.Mode.With_full_target (_, target_stk) ->
          let r_stk = Rstack.relativize target_stk stk in
          let block = Cfg.(find_reachable_block x session.block_map) in
          let key = Lookup_key.of3 x r_stk block in
          (* This is commented out in the interpreter, where I got the code *)
          (* Fmt.pr "@[Update Get to %a@]\n" Lookup_key.pp key; *)
          Hashtbl.change session.term_detail_map key ~f:(function
            | Some td -> Some { td with get_count = td.get_count + 1 }
            | None -> failwith "not term_detail")
      | _, _ -> ()

    let debug_update_write_node session x stk =
      let open Session.Concrete in
      match (session.is_debug, session.mode) with
      | true, Session.Concrete.Mode.With_full_target (_, target_stk) ->
          let r_stk = Rstack.relativize target_stk stk in
          let block = Cfg.(find_reachable_block x session.block_map) in
          let key = Lookup_key.of3 x r_stk block in
          (* This is commented out in the interpreter, where I got the code *)
          (* Fmt.pr "@[Update Set to %a@]\n" Lookup_key.pp key; *)
          Hashtbl.change session.term_detail_map key ~f:(function
            | Some td -> Some { td with is_set = true }
            | None -> failwith "not term_detail")
      | _, _ -> ()

    let debug_stack session x stk (v, _) =
      let open Session.Concrete in
      match (session.is_debug, session.mode) with
      | true, Session.Concrete.Mode.With_full_target (_, target_stk) ->
          let rstk = Rstack.relativize target_stk stk in
          Fmt.pr "@[%a = %a\t\t R = %a@]\n" Id.pp x Dvalue.pp v Rstack.pp rstk
      | _, _ -> ()

    let raise_if_with_stack session x stk v =
      let open Session.Concrete in
      match session.mode with
      | Session.Concrete.Mode.With_full_target (target_x, target_stk) when Ident.equal target_x x ->
          if Concrete_stack.equal_flip target_stk stk
          then raise (Found_target { x; stk; v })
          else
            Fmt.(
              pr "found %a at stack %a, expect %a\n" pp_ident x Concrete_stack.pp
                target_stk Concrete_stack.pp stk)
      | Session.Concrete.Mode.With_target_x target_x when Ident.equal target_x x ->
          raise (Found_target { x; stk; v })
      | _ -> ()

    let alert_lookup session x stk =
      let open Session.Concrete in
      match session.mode with
      | Session.Concrete.Mode.With_full_target (_, target_stk) ->
          let r_stk = Rstack.relativize target_stk stk in
          let block = Cfg.(find_reachable_block x session.block_map) in
          let key = Lookup_key.of3 x r_stk block in
          Fmt.epr "@[Update Alert to %a\t%a@]\n" Lookup_key.pp key Concrete_stack.pp
            stk ;
          Hash_set.add session.lookup_alert key
      | _ -> ()

    let rec same_stack s1 s2 =
      let open Session.Concrete in
      match (s1, s2) with
      | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
          Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
      | [], [] -> true
      | _, _ -> false

    let debug_clause ~conc_session x v stk =
      let open Session.Concrete in
      ILog.app (fun m -> m "@[%a = %a@]" Id.pp x Dvalue.pp v) ;

      (match conc_session.debug_mode with
      | Session.Concrete.Mode.Debug.Debug_clause clause_cb -> clause_cb x stk (Dvalue.value_of_t v)
      | Session.Concrete.Mode.Debug.No_debug -> ()) ;

      raise_if_with_stack conc_session x stk v ;
      debug_stack conc_session x stk (v, stk) ;
      ()
  end

(*
  ------------------------------------
  END DEBUG FUNCTIONS FROM INTERPRETER   
  ------------------------------------
*)

(*
  ------------------------------
  BEGIN HELPERS TO READ FROM ENV   
  ------------------------------
*)

module Fetch =
  struct

    let fetch_val_with_stk ~(conc_session : Session.Concrete.t) ~stk env (Var (x, _)) :
        Dvalue.t * Concrete_stack.t =
      let res = Ident_map.find x env in (* find the variable and stack in the environment *)
      Debug.debug_update_read_node conc_session x stk ; 
      res

    let fetch_val ~(conc_session : Session.Concrete.t) ~stk env x : Dvalue.t =
      fst (fetch_val_with_stk ~conc_session ~stk env x) (* find variable and stack, then discard stack *)

    let fetch_stk ~(conc_session : Session.Concrete.t) ~stk env x : Concrete_stack.t =
      snd (fetch_val_with_stk ~conc_session ~stk env x) (* find variable and stack, then discard variable *)

    let fetch_val_to_direct ~(conc_session : Session.Concrete.t) ~stk env vx : value =
      match fetch_val ~conc_session ~stk env vx with
      | Direct v -> v
      | _ -> failwith "eval to non direct value"

    let fetch_val_to_bool ~(conc_session : Session.Concrete.t) ~stk env vx : bool =
      match fetch_val ~conc_session ~stk env vx with
      | Direct (Value_bool b) -> b
      | _ -> failwith "eval to non bool"

    let check_pattern ~(conc_session : Session.Concrete.t) ~stk env vx pattern : bool =
      match (fetch_val ~conc_session ~stk env vx, pattern) with
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

(* let generate_lookup_key (x : Jayil.Ast.ident) (stk : Dj_common.Concrete_stack.t) : Lookup_key.t =
  Lookup_key.without_block x
  @@ Rstack.from_concrete stk *)

let make_key = Session.Symbolic.Lazy_key.make
let get_key = Session.Symbolic.Lazy_key.to_key

let rec eval_exp
  ~(conc_session : Session.Concrete.t) (* Note: is mutable *)
  ~(symb_session : Session.Symbolic.t)
  (stk : Concrete_stack.t)
  (env : Dvalue.denv)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  ILog.app (fun m -> m "@[-> %a@]\n" Concrete_stack.pp stk);
  (match conc_session.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      Hashtbl.change conc_session.rstk_picked r_stk ~f:(function
        | Some true -> Some false
        | Some false -> raise (Run_into_wrong_stack (Jayil.Ast_tools.first_id e, stk))
        | None-> None)
  | _ -> ());
  let Expr clauses = e in
  let (denv, conc_session), vs =
    List.fold_map
      clauses
      ~init:(env, symb_session)
      ~f:(fun (env, pt) clause ->
        let denv, v, pt = eval_clause ~conc_session ~symb_session:pt stk env clause
        in (denv, pt), v) 
  in
  (denv, List.last_exn vs, conc_session)

and eval_clause
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (stk : Concrete_stack.t)
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
      then raise (Reach_max_step (x, stk, Session.Symbolic.reach_max_step symb_session))
      else ()
  end;
  
  Debug.debug_update_write_node conc_session x stk;
  let x_key = make_key x stk in
  let (v, symb_session) : Dvalue.t * Session.Symbolic.t =
    match cbody with
    | Value_body ((Value_function vf) as v) ->
      (* x = fun ... ; *)
      let retv = FunClosure (x, vf, env) in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      retv, Session.Symbolic.add_key_eq_val symb_session x_key v
    | Value_body ((Value_record r) as v) ->
      (* x = { ... } ; *)
      let retv = RecordClosure (r, env) in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      retv, Session.Symbolic.add_key_eq_val symb_session x_key v
    | Value_body v -> 
      (* x = <bool or int> ; *)
      let retv = Direct v in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      retv, Session.Symbolic.add_key_eq_val symb_session x_key v
    | Var_body vx ->
      (* x = y ; *)
      let Var (y, _) = vx in
      let ret_val, ret_stk = Fetch.fetch_val_with_stk ~conc_session ~stk env vx in
      Session.Concrete.add_alias (x, stk) (y, ret_stk) conc_session;
      let y_key = make_key y ret_stk in 
      ret_val, Session.Symbolic.add_alias symb_session x_key y_key
    | Conditional_body (cx, e1, e2) ->
      (* x = if y then e1 else e2 ; *)
      let Var (y, _) = cx in
      let cond_val, condition_stk = Fetch.fetch_val_with_stk ~conc_session ~stk env cx in
      let cond_bool = match cond_val with Direct (Value_bool b) -> b | _ -> failwith "non-bool condition" in
      let condition_key = make_key y condition_stk in
      let this_branch = Branch.Runtime.{ branch_key = get_key x_key ; condition_key = get_key condition_key ; direction = Branch.Direction.of_bool cond_bool } in

      (* enter/hit branch *)
      let symb_session = Session.Symbolic.hit_branch symb_session this_branch in

      let e = if cond_bool then e1 else e2 in
      let stk' = Concrete_stack.push (x, cond_fid cond_bool) stk in

      (* note that [conc_session] gets mutated when evaluating the branch *)
      let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session stk' env e in
      let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
      let _, ret_stk = Fetch.fetch_val_with_stk ~conc_session ~stk:stk' ret_env last_v in

      (* say the ret_key is equal to x now, then clear out branch *)
      let ret_key = make_key ret_id ret_stk in
      let symb_session = Session.Symbolic.add_alias symb_session x_key ret_key in
      Session.Concrete.add_alias (x, stk) (ret_id, ret_stk) conc_session;
      ret_val, symb_session
    | Input_body ->
      (* x = input ; *)
      let n = conc_session.input_feeder (x, stk) in
      let retv = Direct (Value_int n) in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      retv, Session.Symbolic.add_input symb_session x_key retv
    | Appl_body (vf, (Var (x_arg, _) as varg)) -> begin
      (* x = f y ; *)
      match Fetch.fetch_val ~conc_session ~stk env vf with
      | FunClosure (fid, Function_value (Var (param, _), body), fenv) ->
        (* varg is the argument that fills in param *)
        let arg, arg_stk = Fetch.fetch_val_with_stk ~conc_session ~stk env varg in
        let stk' = Concrete_stack.push (x, fid) stk in
        let env' = Ident_map.add param (arg, stk') fenv in
        Session.Concrete.add_alias (param, stk) (x_arg, arg_stk) conc_session;

        (* enter function: say arg is same as param *)
        let key_param = make_key param stk' in
        let key_arg = make_key x_arg arg_stk in
        let symb_session = Session.Symbolic.add_alias symb_session key_param key_arg in

        (* returned value of function *)
        let ret_env, ret_val, symb_session = eval_exp ~conc_session ~symb_session stk' env' body in
        let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv body in
        let ret_stk = Fetch.fetch_stk ~conc_session ~stk:stk' ret_env last_v in
        Session.Concrete.add_alias (x, stk) (ret_id, ret_stk) conc_session;

        (* exit function: *)
        let ret_key = make_key ret_id ret_stk in
        ret_val, Session.Symbolic.add_alias symb_session x_key ret_key
      | _ -> failwith "appl to a non fun"
      end
    | Match_body (vy, p) ->
      (* x = y ~ <pattern> ; *)
      let match_res = Value_bool (Fetch.check_pattern ~conc_session ~stk env vy p) in
      let retv = Direct (match_res) in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      let Var (y, _) = vy in
      let match_key = make_key y stk in
      retv, Session.Symbolic.add_match symb_session x_key match_key p
    | Projection_body (v, label) -> begin
      match Fetch.fetch_val ~conc_session ~stk env v with
      | RecordClosure (Record_value r, denv) ->
        let proj_ident = function Ident s -> s in
        let Var (proj_x, _) as proj_v = Ident_map.find label r in
        let retv, stk' = Fetch.fetch_val_with_stk ~conc_session ~stk denv proj_v in
        Session.Concrete.add_alias (x, stk) (proj_x, stk') conc_session;
        let Var (v_ident, _) = v in
        let v_stk = Fetch.fetch_stk ~conc_session ~stk env v in
        let record_key = make_key v_ident v_stk in
        let proj_key = make_key proj_x stk' in
        retv, Session.Symbolic.add_alias symb_session x_key proj_key
      | Direct (Value_record (Record_value _record)) ->
        failwith "project should also have a closure"
      | _ -> failwith "project on a non record"
      end
    | Not_body vy ->
      (* x = not y ; *)
      let v = Fetch.fetch_val_to_direct ~conc_session ~stk env vy in 
      let bv =
        match v with
        | Value_bool b -> Value_bool (not b)
        | _ -> failwith "incorrect not"
      in
      let retv = Direct bv in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      let (Var (y, _)) = vy in
      let y_key = make_key y stk in
      retv, Session.Symbolic.add_not symb_session x_key y_key
    | Binary_operation_body (vy, op, vz) ->
      (* x = y op z *)
      let v1 = Fetch.fetch_val_to_direct ~conc_session ~stk env vy
      and v2 = Fetch.fetch_val_to_direct ~conc_session ~stk env vz in
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
        | _, _, _ -> failwith "incorrect binop"
      in
      let retv = Direct v in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
      let Var (y, _) = vy in
      let Var (z, _) = vz in
      let y_stk = Fetch.fetch_stk ~conc_session ~stk env vy in
      let z_stk = Fetch.fetch_stk ~conc_session ~stk env vz in
      let y_key = make_key y y_stk in
      let z_key = make_key z z_stk in
      retv, Session.Symbolic.add_binop symb_session x_key op y_key z_key (* just adding keys, not any runtime values, so does not need to be implied by results of earlier branches *)
    | Abort_body -> begin
      let ab_v = AbortClosure env in
      Session.Concrete.add_val_def_mapping (x, stk) (cbody, ab_v) conc_session;
      match conc_session.mode with
      | Plain -> raise @@ Found_abort (ab_v, Session.Symbolic.found_abort symb_session) (* no need to "exit" or anything. Just say interpretation stops. *)
      (* next two are for debug mode *)
      | With_target_x target ->
        if Id.equal target x
        then raise @@ Found_target { x ; stk ; v = ab_v }
        else raise @@ Found_abort (ab_v, symb_session)
      | With_full_target (target, tar_stk) ->
        if Id.equal target x && Concrete_stack.equal_flip tar_stk stk
        then raise @@ Found_target { x ; stk ; v = ab_v }
        else raise @@ Found_abort (ab_v, symb_session)
      end
    | Assert_body cx | Assume_body cx ->
      (* TODO: should I ever treat assert and assume differently? *)
      let v = Fetch.fetch_val_to_bool ~conc_session ~stk env cx in
      let Var (y, _) = cx in 
      let key = make_key y (Fetch.fetch_stk ~conc_session ~stk env cx) in
      let symb_session = Session.Symbolic.found_assume symb_session key in
      if not v
      then
        raise @@ Found_failed_assume (Session.Symbolic.fail_assume symb_session) (* fail the assume that was just found *)
      else
        let retv = Direct (Value_bool v) in
        Session.Concrete.add_val_def_mapping (x, stk) (cbody, retv) conc_session;
        retv, symb_session (*Session.Symbolic.add_key_eq_val symb_session x_key (Value_bool v) *)
  in
  Debug.debug_clause ~conc_session x v stk;
  (Ident_map.add x (v, stk) env, v, symb_session)

let eval_exp_default
  ~(conc_session : Session.Concrete.t)
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Dvalue.denv * Dvalue.t * Session.Symbolic.t
  =
  eval_exp
    ~conc_session 
    ~symb_session
    Concrete_stack.empty (* empty stack *)
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
    Log.Export.CLog.app (fun m -> m "Evaluated to: %a\n" Dvalue.pp v);
    symb_session
  with
  | Found_abort (_, symb_session) ->
      Log.Export.CLog.app (fun m -> m "Found abort in interpretation\n");
      symb_session
  | Reach_max_step (_, _, symb_session) ->
      Log.Export.CLog.app (fun m -> m "Reach max steps\n");
      symb_session
  | Found_failed_assume symb_session
  | Found_failed_assert symb_session ->
      Log.Export.CLog.app (fun m -> m "Found failed assume or assert\n");
      symb_session
  | Run_the_same_stack_twice (x, stk) -> (* bubbles exception *)
      Fmt.epr "Run into the same stack twice\n" ;
      Debug.alert_lookup conc_session x stk ;
      raise (Run_the_same_stack_twice (x, stk))
  | Run_into_wrong_stack (x, stk) -> (* bubble exception *)
      Fmt.epr "Run into wrong stack\n" ;
      Debug.alert_lookup conc_session x stk ;
      raise (Run_into_wrong_stack (x, stk))


(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple symbolic sessions, trying to hit the branches.
*)
let rec loop (e : expr) (prev_session : Session.t) : (Branch_info.t * bool) Lwt.t =
  let open Lwt.Infix in
  let%lwt () = Lwt.pause () in
  Session.next prev_session
  |> begin function
    | `Done (branch_info, has_pruned) ->
      Log.Export.CLog.app (fun m -> m "\n------------------------------\nFinishing concolic evaluation...\n\n");
      Log.Export.CLog.app (fun m -> m "Ran %d interpretations.\n" (Session.run_num prev_session));
      Log.Export.CLog.app (fun m -> m "Tree was pruned: %b\n" has_pruned);
      Log.Export.CLog.app (fun m -> m "%s" (Branch_info.to_string branch_info));
      Lwt.return (branch_info, has_pruned)
    | `Next (session, symb_session, conc_session) ->
      (* let status_store = Session.Symbolic.status_store symb_session in *)
      Log.Export.CLog.info (fun m -> m "Pre-run info:\n");
      Log.Export.CLog.info (fun m -> m "%s" (Branch_info.to_string @@ Session.branch_info session));
      (* Branch_tracker.Status_store.Without_payload.print status_store; *)
      Log.Export.CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session));
      let t0 = Caml_unix.gettimeofday () in
      let resulting_symbolic = try_eval_exp_default ~conc_session ~symb_session e in
      let t1 = Caml_unix.gettimeofday () in
      Log.Export.CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0));
      loop e
      @@ Session.accum_symbolic session resulting_symbolic
    end

(* TODO: maybe move some of this to driver *)
let lwt_eval : (Jayil.Ast.expr -> (Branch_info.t * bool) Lwt.t) Concolic_options.Fun.t =
  let f =
    fun (r : Concolic_options.t) ->
      fun (e : Jayil.Ast.expr) ->
        Log.Export.CLog.app (fun m -> m "\nStarting concolic execution...\n");
        (* Repeatedly evaluate program *)
        Riddler.reset ();
        Lwt_unix.with_timeout r.global_timeout_sec
        @@ fun () ->
          e
          |> Session.of_expr
          |> Concolic_options.Fun.appl Session.with_options r
          |> loop e
  in
  Concolic_options.Fun.make f

(* Concolically execute/test program. Not necessarily needed anymore *)
let[@landmark] eval : (Jayil.Ast.expr -> Branch_info.t) Concolic_options.Fun.t =
  let f =
    fun (r : Concolic_options.t) ->
      fun (e : Jayil.Ast.expr) ->
        try
          let t0 = Caml_unix.gettimeofday () in
          let res =
            Tuple2.get1
            @@ Lwt_main.run
            @@ Concolic_options.Fun.appl lwt_eval r e
          in
          Log.Export.CLog.app (fun m -> m "\nFinished concolic evaluation in %fs.\n" (Caml_unix.gettimeofday () -. t0));
          res
        with
        | Lwt_unix.Timeout ->
          Log.Export.CLog.app (fun m -> m "Quit due to total run timeout in %0.3f seconds.\n" r.global_timeout_sec);
          Branch_info.empty
  in
  Concolic_options.Fun.make f

module Test_result =
  struct
    type t =
      | Found_abort of Branch.t * Jil_input.t list (* Found an abort at this branch using these inputs *)
      | Exhausted               (* Ran all possible tree paths, and no paths were too deep *)
      | Exhausted_pruned_tree   (* Ran all possible tree paths up to the given max step *)
      | Timeout                 (* total evaluation timeout *)
  end

let[@landmark] test : (Jayil.Ast.expr -> Test_result.t) Concolic_options.Fun.t =
  let f =
    fun (r : Concolic_options.t) ->
      fun (e : Jayil.Ast.expr) ->
        try
          let t0 = Caml_unix.gettimeofday () in
          let res, has_pruned =
            Lwt_main.run
            @@ Concolic_options.Fun.appl lwt_eval r e
          in
          Log.Export.CLog.app (fun m -> m "\nFinished concolic evaluation in %fs.\n" (Caml_unix.gettimeofday () -. t0));
          Branch_info.find res ~f:(fun _ -> function Branch_info.Status.Found_abort _ -> true | _ -> false)
          |> function
            | Some (branch, Branch_info.Status.Found_abort inputs) ->
              Format.printf "\nFOUND_ABORT\n";
              Test_result.Found_abort (branch, inputs)
            | None when not has_pruned ->
              Format.printf "\nEXHAUSTED\n";
              Exhausted
            | _ ->
              Format.printf "\nEXHAUSTED_PRUNED_TREE\n";
              Exhausted_pruned_tree
        with
        | Lwt_unix.Timeout ->
          Log.Export.CLog.app (fun m -> m "Quit due to total run timeout in %0.3f seconds.\n" r.global_timeout_sec);
          Format.printf "\nTIMEOUT\n";
          Timeout
  in
  Concolic_options.Fun.make f
