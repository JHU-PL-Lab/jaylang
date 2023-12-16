open Core
open Jayil.Ast
open Dj_common (* exposes Concrete_stack *)
open Concolic_exceptions (* these help convey status of evaluation *)
open Dvalue (* just to expose constructors *)

module ILog = Log.Export.ILog

(* Ident for conditional bool. *)
let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(*
  --------------------------------------
  BEGIN DEBUG FUNCTIONS FROM INTERPRETER   
  --------------------------------------

  Unless labeled, I just keep these called "session", when really they're
  an eval session (see Session.mli).
*)

module Debug =
  struct
    let debug_update_read_node session x stk =
      let open Session.Eval in
      match (session.is_debug, session.mode) with
      | true, Session.Mode.With_full_target (_, target_stk) ->
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
      let open Session.Eval in
      match (session.is_debug, session.mode) with
      | true, Session.Mode.With_full_target (_, target_stk) ->
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
      let open Session.Eval in
      match (session.is_debug, session.mode) with
      | true, Session.Mode.With_full_target (_, target_stk) ->
          let rstk = Rstack.relativize target_stk stk in
          Fmt.pr "@[%a = %a\t\t R = %a@]\n" Id.pp x Dvalue.pp v Rstack.pp rstk
      | _, _ -> ()

    let raise_if_with_stack session x stk v =
      let open Session.Eval in
      match session.mode with
      | Session.Mode.With_full_target (target_x, target_stk) when Ident.equal target_x x ->
          if Concrete_stack.equal_flip target_stk stk
          then raise (Found_target { x; stk; v })
          else
            Fmt.(
              pr "found %a at stack %a, expect %a\n" pp_ident x Concrete_stack.pp
                target_stk Concrete_stack.pp stk)
      | Session.Mode.With_target_x target_x when Ident.equal target_x x ->
          raise (Found_target { x; stk; v })
      | _ -> ()

    let alert_lookup session x stk =
      let open Session.Eval in
      match session.mode with
      | Session.Mode.With_full_target (_, target_stk) ->
          let r_stk = Rstack.relativize target_stk stk in
          let block = Cfg.(find_reachable_block x session.block_map) in
          let key = Lookup_key.of3 x r_stk block in
          Fmt.epr "@[Update Alert to %a\t%a@]\n" Lookup_key.pp key Concrete_stack.pp
            stk ;
          Hash_set.add session.lookup_alert key
      | _ -> ()

    let rec same_stack s1 s2 =
      let open Session.Eval in
      match (s1, s2) with
      | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
          Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
      | [], [] -> true
      | _, _ -> false

    let debug_clause ~eval_session x v stk =
      let open Session.Eval in
      ILog.app (fun m -> m "@[%a = %a@]" Id.pp x Dvalue.pp v) ;

      (match eval_session.debug_mode with
      | Session.Mode.Debug.Debug_clause clause_cb -> clause_cb x stk (Dvalue.value_of_t v)
      | Session.Mode.Debug.No_debug -> ()) ;

      raise_if_with_stack eval_session x stk v ;
      debug_stack eval_session x stk (v, stk) ;
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

    let fetch_val_with_stk ~(eval_session : Session.Eval.t) ~stk env (Var (x, _)) :
        Dvalue.t * Concrete_stack.t =
      let res = Ident_map.find x env in (* find the variable and stack in the environment *)
      Debug.debug_update_read_node eval_session x stk ; 
      res

    let fetch_val ~(eval_session : Session.Eval.t) ~stk env x : Dvalue.t =
      fst (fetch_val_with_stk ~eval_session ~stk env x) (* find variable and stack, then discard stack *)

    let fetch_stk ~(eval_session : Session.Eval.t) ~stk env x : Concrete_stack.t =
      snd (fetch_val_with_stk ~eval_session ~stk env x) (* find variable and stack, then discard variable *)

    let fetch_val_to_direct ~(eval_session : Session.Eval.t) ~stk env vx : value =
      match fetch_val ~eval_session ~stk env vx with
      | Direct v -> v
      | _ -> failwith "eval to non direct value"

    let fetch_val_to_bool ~(eval_session : Session.Eval.t) ~stk env vx : bool =
      match fetch_val ~eval_session ~stk env vx with
      | Direct (Value_bool b) -> b
      | _ -> failwith "eval to non bool"

    let check_pattern ~(eval_session : Session.Eval.t) ~stk env vx pattern : bool =
      match (fetch_val ~eval_session ~stk env vx, pattern) with
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

let generate_lookup_key (x : Jayil.Ast.ident) (stk : Dj_common.Concrete_stack.t) : Lookup_key.t =
  { x
  ; r_stk = Rstack.from_concrete stk
  ; block = Dj_common.Cfg.{ id = x ; clauses = [] ; kind = Main } }

(*
  When evaluating any expression, the provided parent is not yet a parent dependency
  inside the session to any variable within. A variable has a parent dependency if it
  was a branch key (i.e. a variable that takes on the value of a branch clause);
  the parent it has is the condition in its clause. It also gains any "parent" from
  internal branches that it depends on by taking on their parents as dependencies.
  The only other variables that have parents are those that depend on a branch key that
  *does* have parents, and these variables thus gain those parents. These variables do
  not gain their parent condition as a parent yet in the case that they aren't used at all
  in the return of the branch. If they are used in the return of the branch, then the branch
  key then depends on them, and when the branch key finally gains the condition as a parent,
  so do the variables implicitly.
*)
let rec eval_exp
  ~(session : Session.Concolic.t ref)
  (stk : Concrete_stack.t)
  (env : Dvalue.denv)
  (e : expr)
  (parent : Branch_solver.Parent.t)
  : Dvalue.denv * Dvalue.t
  =
  ILog.app (fun m -> m "@[-> %a@]\n" Concrete_stack.pp stk);
  (match (!session).eval.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      Hashtbl.change (!session).eval.rstk_picked r_stk ~f:(function
        | Some true -> Some false
        | Some false -> raise (Run_into_wrong_stack (Jayil.Ast_tools.first_id e, stk))
        | None-> None)
  | _ -> ());
  let Expr clauses = e in
  let denv, vs' =
    List.fold_map ~f:(eval_clause ~session stk parent) ~init:env clauses
  in
  (denv, List.last_exn vs')

and eval_clause
  ~(session : Session.Concolic.t ref)
  (stk : Concrete_stack.t)
  (parent : Branch_solver.Parent.t)
  (env : Dvalue.denv)
  (clause : clause)
  : Dvalue.denv * Dvalue.t
  =
  let Clause (Var (x, _), cbody) = clause in
  (* Note that we extract the eval session out, and even if we reassign to the session cell,
     as long as the eval session is copied (e.g. { !session with run_num = 10 }), changes to eval session
     carry over to the record that is in the session cell at the end. 
     e.g.
        f eval_session; (* mutates eval_session *)
        session := { !session with run_num = 10};
        (* here (!session).eval is exactly eval_session still *)
  *)
  let eval_session = (!session).eval in

  (match eval_session.max_step with 
  | None -> ()
  | Some max_step ->
      Int.incr eval_session.step;
      if !(eval_session.step) > max_step then raise (Reach_max_step (x, stk)) else ()
    );
  
  Debug.debug_update_write_node eval_session x stk;
  let x_key = generate_lookup_key x stk in
  let (v : Dvalue.t)(*, (session : Session.Concolic.t) *) =
    match cbody with
    | Value_body ((Value_function vf) as v) ->
      (* x = fun ... ; *)
      let retv = FunClosure (x, vf, env) in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      (* This would be more smooth if it returned session as well, and session weren't mutable *)
      Session.Concolic.Ref_cell.add_key_eq_val session parent x_key v;
      retv
    | Value_body ((Value_record r) as v) ->
      (* x = { ... } ; *)
      let retv = RecordClosure (r, env) in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      Session.Concolic.Ref_cell.add_key_eq_val session parent x_key v;
      retv
    | Value_body v -> 
      (* x = <bool or int> ; *)
      let retv = Direct v in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      Session.Concolic.Ref_cell.add_key_eq_val session parent x_key v;
      retv
    | Var_body vx ->
      (* x = y ; *)
      let Var (y, _) = vx in
      let ret_val, ret_stk = Fetch.fetch_val_with_stk ~eval_session ~stk env vx in
      Session.Eval.add_alias (x, stk) (y, ret_stk) eval_session;
      let y_key = generate_lookup_key y ret_stk in 
      Session.Concolic.Ref_cell.add_formula session [y_key] parent @@ Riddler.eq x_key y_key;
      Session.Concolic.Ref_cell.add_siblings session x_key [y_key];
      ret_val
    | Conditional_body (cx, e1, e2) ->
      (* x = if y then e1 else e2 ; *)
      let Var (y, _) = cx in
      let cond_val, condition_stk = Fetch.fetch_val_with_stk ~eval_session ~stk env cx in
      let cond_bool = match cond_val with Direct (Value_bool b) -> b | _ -> failwith "non-bool condition" in
      let condition_key = generate_lookup_key y condition_stk in
      let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool cond_bool } in
      let this_branch_as_parent = Branch_solver.Parent.of_runtime_branch this_branch in
      (* Hit branch *)
      Session.Concolic.Ref_cell.hit_branch ~new_status:Branch.Status.Hit session
      @@ Branch.Ast_branch.of_ident_and_bool x cond_bool;
      (* Set target branch to the other side if the other side hasn't been hit yet *)
      Session.Concolic.Ref_cell.update_target_branch session this_branch;

      let e = if cond_bool then e1 else e2 in
      let stk' = Concrete_stack.push (x, cond_fid cond_bool) stk in

      (* this session gets mutated when evaluating the branch *)
      let res = Result.try_with (fun () -> eval_exp ~session stk' env e this_branch_as_parent) in
      begin
        match res with
        | Error (Found_abort (AbortClosure ret_env)) ->
          (* TODO: fix up this temporary patch where I say result key is x, which is operationally the same as no result at all *)
          Session.Concolic.Ref_cell.exit_branch session parent this_branch x_key
          (* Don't set branch to found abort because that happens only to parent when actually finding abort. *)
          (* Session.Concolic.Ref_cell.hit_branch ~new_status:Branch.Status.Found_abort session
          @@ Branch.Ast_branch.of_ident_and_bool x cond_bool; *)
        | Error (Reach_max_step _) -> ()
          (* TODO: retry? *)
          (* Session.Concolic.Ref_cell.hit_branch ~new_status:Branch.Status.Reached_max_step session
          @@ Branch.Ast_branch.of_ident_and_bool x cond_bool; *)
        | _ -> () (* continue normally on Ok or any other exception *)
      end;
      let ret_env, ret_val = Result.ok_exn res in (* Bubbles exceptions if necessary *)
      let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
      let _, ret_stk = Fetch.fetch_val_with_stk ~eval_session ~stk:stk' ret_env last_v in

      (* say the ret_key is equal to x now, then clear out branch *)
      let ret_key = generate_lookup_key ret_id ret_stk in
      Session.Concolic.Ref_cell.add_formula session [ret_key] this_branch_as_parent @@ Riddler.eq x_key ret_key;
      Session.Concolic.Ref_cell.exit_branch session parent this_branch ret_key;
      Session.Eval.add_alias (x, stk) (ret_id, ret_stk) eval_session;
      ret_val
    | Input_body ->
      let n = eval_session.input_feeder (x, stk) in
      let retv = Direct (Value_int n) in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      let Ident s = x in
      Format.printf "Feed %d to %s \n" n s;
      (* Session.Concolic.Ref_cell.add_key_eq_value_opt session parent x_key None; TODO: why not say x equals Value_int n? because we need x to be variable? Does it have to do with name of input variable(s)? *)
      retv
    | Appl_body (vf, (Var (x_arg, _) as varg)) -> begin
      (* x = f y ; *)
      match Fetch.fetch_val ~eval_session ~stk env vf with
      | FunClosure (fid, Function_value (Var (param, _), body), fenv) ->
        (* vx2 is the argument that fills in param *)
        let arg, arg_stk = Fetch.fetch_val_with_stk ~eval_session ~stk env varg in
        let stk' = Concrete_stack.push (x, fid) stk in
        let env' = Ident_map.add param (arg, stk') fenv in
        Session.Eval.add_alias (param, stk) (x_arg, arg_stk) eval_session;

        (* enter function: *)
        let Var (xid, _) = vf in
        let f_stk = Fetch.fetch_stk ~eval_session ~stk env vf in
        let key_f = generate_lookup_key xid f_stk in
        let key_param = generate_lookup_key param stk' in
        let key_arg = generate_lookup_key x_arg arg_stk in
        Session.Concolic.Ref_cell.add_formula session [key_f; key_arg] parent @@ Riddler.enter_fun key_param key_arg;

        (* returned value of function *)
        let ret_env, ret_val = eval_exp ~session stk' env' body parent in
        let (Var (ret_id, _) as last_v) = Jayil.Ast_tools.retv body in
        let ret_stk = Fetch.fetch_stk ~eval_session ~stk:stk' ret_env last_v in
        Session.Eval.add_alias (x, stk) (ret_id, ret_stk) eval_session;

        (* exit function: *)
        let ret_key = generate_lookup_key ret_id ret_stk in
        Session.Concolic.Ref_cell.add_formula session [ret_key; key_f; key_arg] parent @@ Riddler.exit_fun x_key ret_key;
        Session.Concolic.Ref_cell.add_siblings session x_key [ret_key; key_f; key_arg];
        ret_val
      | _ -> failwith "appl to a non fun"
      end
    | Match_body (vy, p) ->
      (* x = y ~ <pattern> ; *)
      let match_res = Value_bool (Fetch.check_pattern ~eval_session ~stk env vy p) in
      let retv = Direct (match_res) in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      let Var (y, _) = vy in
      let match_key = generate_lookup_key y stk in
      let x_key_exp = Riddler.key_to_var x_key in
      Session.Concolic.Ref_cell.add_formula session [match_key] parent @@ Solver.SuduZ3.ifBool x_key_exp; (* x has a bool value that is must take on *)
      Session.Concolic.Ref_cell.add_formula session [match_key] parent @@ Solver.SuduZ3.eq (Solver.SuduZ3.project_bool x_key_exp) (Riddler.is_pattern match_key p); (* x is same as result of match *)
      Session.Concolic.Ref_cell.add_siblings session x_key [match_key];
      (* we add as a sibling later so that there are fewer "implies" within the expressions, but I think
          it could be more concise to add siblings before. *)
      retv
    | Projection_body (v, label) -> begin
      match Fetch.fetch_val ~eval_session ~stk env v with
      | RecordClosure (Record_value r, denv) ->
        let Var (proj_x, _) as proj_v = Ident_map.find label r in
        let retv, stk' = Fetch.fetch_val_with_stk ~eval_session ~stk denv proj_v in
        Session.Eval.add_alias (x, stk) (proj_x, stk') eval_session;
        (* TODO: records have limited concolic functionality *)
        let Var (v_ident, _) = v in
        let v_stk = Fetch.fetch_stk ~eval_session ~stk env v in
        let record_key = generate_lookup_key v_ident v_stk in
        let proj_key = generate_lookup_key proj_x stk' in
        Session.Concolic.Ref_cell.add_formula session [proj_key; record_key] parent @@ Riddler.eq x_key proj_key;
        Session.Concolic.Ref_cell.add_siblings session x_key [proj_key; record_key];
        retv
      | Direct (Value_record (Record_value _record)) ->
        failwith "project should also have a closure"
      | _ -> failwith "project on a non record"
      end
    | Not_body vy ->
      (* x = not y ; *)
      let v = Fetch.fetch_val_to_direct ~eval_session ~stk env vy in 
      let bv =
        match v with
        | Value_bool b -> Value_bool (not b)
        | _ -> failwith "incorrect not"
      in
      let retv = Direct bv in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      let (Var (y, _)) = vy in
      let y_key = generate_lookup_key y stk in
      Session.Concolic.Ref_cell.add_formula session [y_key] parent @@ Riddler.not_ x_key y_key;
      Session.Concolic.Ref_cell.add_siblings session x_key [y_key];
      retv
    | Binary_operation_body (vy, op, vz) ->
      (* x = y op z *)
      let v1 = Fetch.fetch_val_to_direct ~eval_session ~stk env vy
      and v2 = Fetch.fetch_val_to_direct ~eval_session ~stk env vz in
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
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      let Var (y, _) = vy in
      let Var (z, _) = vz in
      let y_stk = Fetch.fetch_stk ~eval_session ~stk env vy in
      let z_stk = Fetch.fetch_stk ~eval_session ~stk env vz in
      let y_key = generate_lookup_key y y_stk in
      let z_key = generate_lookup_key z z_stk in
      Session.Concolic.Ref_cell.add_formula session [y_key; z_key] parent @@ Riddler.binop_without_picked x_key op y_key z_key;
      Session.Concolic.Ref_cell.add_siblings session x_key [y_key; z_key];
      retv
    | Abort_body -> begin
      (* TODO: concolic *)
      let ab_v = AbortClosure env in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, ab_v) eval_session;
      (* TODO: let abort branches not be solved for in future runs *)
      Session.Concolic.Ref_cell.hit_branch ~new_status:Branch.Status.Found_abort session
      @@ Branch_solver.Parent.to_ast_branch_exn parent;
      match eval_session.mode with
      | Plain -> raise @@ Found_abort ab_v
      (* next two are for debug mode *)
      | With_target_x target ->
        if Id.equal target x
        then raise @@ Found_target { x ; stk ; v = ab_v }
        else raise @@ Found_abort ab_v
      | With_full_target (target, tar_stk) ->
        if Id.equal target x && Concrete_stack.equal_flip tar_stk stk
        then raise @@ Found_target { x ; stk ; v = ab_v }
        else raise @@ Found_abort ab_v
      end
    | Assert_body _ | Assume_body _ ->
      let v = Value_bool true in
      let retv = Direct v in
      Session.Eval.add_val_def_mapping (x, stk) (cbody, retv) eval_session;
      Session.Concolic.Ref_cell.add_key_eq_val session parent x_key v;
      retv
  in
  Debug.debug_clause ~eval_session x v stk;
  (Ident_map.add x (v, stk) env, v)


let eval_exp_default ~(session: Session.Concolic.t ref) (e : expr) : Dvalue.denv * Dvalue.t =
  eval_exp
    ~session 
    Concrete_stack.empty (* empty stack *)
    Ident_map.empty (* empty environment *) (* TODO: should env be in the session? *)
    e
    Branch_solver.Parent.Global (* global environment i.e not under any conditional branch *)

(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple concolic sessions, trying to hit the branches.
*)
let rec eval (e : expr) (prev_session : Session.Concolic.t) : unit =
  Format.printf "------------------------------\nRunning program...\n";
  Branch.Status_store.print prev_session.branch_store;
  Branch.Runtime.print_target_option @@ List.hd prev_session.target_stack;

  (* Generate the next session, which throws appropriate errors if execution is complete *)
  match Session.Concolic.next prev_session with
  | `Done branch_store -> Session.Concolic.finish_and_print { (Session.Concolic.create_default ()) with branch_store } (* TODO: clean this *)
  | `Next session -> 
    let this_target = List.hd session.target_stack in (* target is top of stack of new session *)
    let session = ref session in
    (* Sean had bug where step and val_def_map needed to be reset *)

    (* Create new environment and evaluate program: *)
    try
      let _, v = eval_exp_default ~session:session e in
      (* Assert that we hit our target branch: *)
      Session.Concolic.assert_target_hit !session this_target;
      (* Print evaluated result and run again. *)
      Format.printf "Evaluated to: %a\n" Dvalue.pp v;
      eval e !session
    with
    (* TODO: Error cases: TODO, if we hit abort, re-run if setting is applied. *)
    | Found_abort _ ->
        (* TODO: abort cuts us too short to actually solve for the next target. The feeder is wrong.*)
        Format.printf "Running next iteration of concolic after abort\n";
        eval e !session
    | Reach_max_step (x, stk) ->
        (* Fmt.epr "Reach max steps\n" ; *)
        (* alert_lookup target_stk x stk session.lookup_alert; *)
        raise (Reach_max_step (x, stk))
    | Run_the_same_stack_twice (x, stk) ->
        Fmt.epr "Run into the same stack twice\n" ;
        Debug.alert_lookup (!session).eval x stk ;
        raise (Run_the_same_stack_twice (x, stk))
    | Run_into_wrong_stack (x, stk) ->
        Fmt.epr "Run into wrong stack\n" ;
        Debug.alert_lookup (!session).eval x stk ;
        raise (Run_into_wrong_stack (x, stk))

(* Concolically execute/test program. *)
let concolic_eval (e : expr) : unit = 
  Format.printf "\nStarting concolic execution...\n";
  Session.Concolic.load_branches (Session.Concolic.create_default ()) e
  |> eval e (* Repeatedly evaluate program *)