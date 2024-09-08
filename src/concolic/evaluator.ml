open Core
open Jayil.Ast
open Dvalue (* just to expose constructors *)

module CLog = Dj_common.Log.Export.CLog

let check_pattern (env : Denv.t) (vx : var) (p : pattern) : bool =
  match (Denv.fetch_val env vx, p) with
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

module Cresult =
  struct
    type t =
      | Ok of { denv : Denv.t ; dval : Dvalue.t ; symb_session : Session.Symbolic.t }
      | Found_abort of Session.Symbolic.t
      | Type_mismatch of Session.Symbolic.t
      | Found_failed_assume of Session.Symbolic.t
      | Reach_max_step of Session.Symbolic.t

    let pp (x : t) : string =
      match x with
      | Ok r -> Dvalue.pp r.dval
      | Found_abort _ -> "Found abort in interpretation"
      | Type_mismatch _ -> "Type mismatch in interpretation"
      | Reach_max_step _ -> "Reach max steps during interpretation"
      | Found_failed_assume _ -> "Found failed assume or assert"

    let return denv dval symb_session = Ok { denv ; dval ; symb_session }

    let get_session = function
    | Ok { symb_session = s ; _ } 
    | Found_abort s
    | Type_mismatch s
    | Found_failed_assume s
    | Reach_max_step s -> s

    let reach_max_step =
      fun s -> Reach_max_step (Session.Symbolic.reach_max_step s)

    let found_abort =
      fun s -> Found_abort (Session.Symbolic.found_abort s)

    let failed_assume =
      fun s -> Found_failed_assume (Session.Symbolic.fail_assume s)

    let type_mismatch =
      fun s -> Type_mismatch (Session.Symbolic.found_type_mismatch s)
  end

open Cresult

(*
  ----------
  BEGIN EVAL
  ----------

  This section is basically an interpreter injected with concolic logic.
  It is a single evaluation of the program, and it accumulates symbolic formulas.

  I used to have a nice state monad here, but because of the ppx expansion, we 
  lost tail recursion, so now I just manually pass it through and case on the
  result a *lot*. It's ugly, but it's faster than with the nice state monad.
*)

type c = Cresult.t -> Cresult.t

let eval_exp
  ~(conc_session : Session.Concrete.t) (* Note: is mutable. Doesn't get passed through *)
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Session.Symbolic.t
  =
  let rec eval_exp ~(symb_session : Session.Symbolic.t) (env : Denv.t) (Expr clauses : expr) (cont : c) : Cresult.t =
    match clauses with
    | [] -> failwith "empty clause list" (* safe because empty clause list is a parse error *)
    | clause :: [] -> eval_clause ~symb_session env clause cont
    | clause :: nonempty_tl ->
      eval_clause ~symb_session env clause (function
        | Ok { denv ; symb_session ; _ } -> eval_exp ~symb_session denv (Expr nonempty_tl) cont
        | res -> res
      )

  and eval_clause ~(symb_session : Session.Symbolic.t) (env : Denv.t) (Clause (Var (x, _), cbody) : clause) (cont : c) : Cresult.t =
    Session.Concrete.incr_step conc_session; (* mutates the session that is in scope *)

    if Session.Concrete.is_max_step conc_session
    then reach_max_step symb_session
    else
      let x_key = Concolic_key.create x conc_session.step in

      let next v s =
        cont @@ return (Denv.add env x v x_key) v s
      in
      
      match cbody with
      | Value_body ((Value_function vf) as v) ->
        (* x = fun ... ; *)
        next (FunClosure (x, vf, env)) @@ Session.Symbolic.add_key_eq_val x_key v symb_session
      | Value_body ((Value_record r) as v) ->
        (* x = { ... } ; *)
        next (RecordClosure (r, env)) @@ Session.Symbolic.add_key_eq_val x_key v symb_session
      | Value_body v -> 
        (* x = <bool or int> ; *)
        next (Direct v) @@ Session.Symbolic.add_key_eq_val x_key v symb_session
      | Var_body vx ->
        (* x = y ; *)
        let ret_val, ret_key = Denv.fetch env vx in
        next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
      | Conditional_body (cx, e1, e2) -> 
        (* x = if y then e1 else e2 ; *)
        let cond_val, condition_key = Denv.fetch env cx in
        begin
          match cond_val with
          | Direct (Value_bool b) ->
            let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool b } in
            let e = if b then e1 else e2 in
            (* note that [conc_session] gets mutated when evaluating the branch *)
            eval_exp ~symb_session:(Session.Symbolic.hit_branch this_branch symb_session) env e (function
            | Ok { denv = ret_env ; dval = ret_val ; symb_session } ->
              let last_v = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
              let ret_key = Denv.fetch_key ret_env last_v in
              next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
            | res -> res
            )
          | _ -> type_mismatch symb_session
        end
      | Input_body ->
        (* x = input ; *)
        let ret_val = Direct (Value_int (conc_session.input_feeder x_key)) in
        next ret_val @@ Session.Symbolic.add_input x_key ret_val symb_session
      | Appl_body (vf, varg) -> begin 
        (* x = f y ; *)
        match Denv.fetch_val env vf with
        | FunClosure (_, Function_value (Var (param, _), body), fenv) ->
          (* increment step count so that the key for the parameter gets an identifier different than the clause *)
          Session.Concrete.incr_step conc_session;

          (* varg is the argument that fills in param *)
          let arg, arg_key = Denv.fetch env varg in
          let param_key = Concolic_key.create param conc_session.step in
          let env' = Denv.add fenv param arg param_key in

          (* returned value of function *)
          eval_exp ~symb_session:(Session.Symbolic.add_alias param_key arg_key symb_session) env' body (function
          | Ok { denv = ret_env ; dval = ret_val ; symb_session } ->
            let last_v = Jayil.Ast_tools.retv body in
            let ret_key = Denv.fetch_key ret_env last_v in

            (* exit function: *)
            next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
          | res -> res
          )
        | _ -> type_mismatch symb_session
        end
      | Match_body (vy, p) ->
        (* x = y ~ <pattern> ; *)
        let match_res = Value_bool (check_pattern env vy p) in
        let y_key = Denv.fetch_key env vy in
        next (Direct match_res) @@ Session.Symbolic.add_match x_key y_key p symb_session
      | Projection_body (v, label) -> begin
        match Denv.fetch_val env v with
        | RecordClosure (Record_value r, denv) ->
          let proj_var = Ident_map.find label r in
          let ret_val, ret_key = Denv.fetch denv proj_var in
          next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
        | Direct (Value_record (Record_value _record)) ->
          failwith "project should also have a closure"
        | _ -> type_mismatch symb_session
        end
      | Not_body vy ->
        (* x = not y ; *)
        let y_val, y_key = Denv.fetch env vy in
        begin
          match y_val with
          | Direct (Value_bool b) ->
            next (Direct (Value_bool (not b))) @@ Session.Symbolic.add_not x_key y_key symb_session
          | _ -> type_mismatch symb_session
        end
      | Binary_operation_body (vy, op, vz) ->
        (* x = y op z *)
        let y, y_key = Denv.fetch env vy in
        let z, z_key = Denv.fetch env vz in
        begin
          match y, z with
          | Direct v1, Direct v2 ->
            let symb_session = Session.Symbolic.add_binop x_key op y_key z_key symb_session in
            begin
            match op, v1, v2 with
            | Binary_operator_plus, Value_int n1, Value_int n2                  -> next (Direct (Value_int  (n1 + n2)))
            | Binary_operator_minus, Value_int n1, Value_int n2                 -> next (Direct (Value_int  (n1 - n2)))
            | Binary_operator_times, Value_int n1, Value_int n2                 -> next (Direct (Value_int  (n1 * n2)))
            | Binary_operator_divide, Value_int n1, Value_int n2                -> next (Direct (Value_int  (n1 / n2)))
            | Binary_operator_modulus, Value_int n1, Value_int n2               -> next (Direct (Value_int  (n1 mod n2)))
            | Binary_operator_less_than, Value_int n1, Value_int n2             -> next (Direct (Value_bool (n1 < n2)))
            | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> next (Direct (Value_bool (n1 <= n2)))
            | Binary_operator_equal_to, Value_int n1, Value_int n2              -> next (Direct (Value_bool (n1 = n2)))
            | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> next (Direct (Value_bool (Bool.(b1 = b2))))
            | Binary_operator_and, Value_bool b1, Value_bool b2                 -> next (Direct (Value_bool (b1 && b2)))
            | Binary_operator_or, Value_bool b1, Value_bool b2                  -> next (Direct (Value_bool (b1 || b2)))
            | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> next (Direct (Value_bool (n1 <> n2)))
            | _ -> type_mismatch
            end
            symb_session
        | _ -> type_mismatch symb_session
        end
      | Abort_body -> found_abort symb_session
      | Assert_body cx | Assume_body cx ->
        let cond_val, cond_key = Denv.fetch env cx in 
        match cond_val with
        | Direct (Value_bool b) ->
          let symb_session = Session.Symbolic.found_assume cond_key symb_session in
          if not b
          then failed_assume symb_session
          else next cond_val symb_session
        | _ -> type_mismatch symb_session
  in

  get_session
  @@ eval_exp ~symb_session Denv.empty e (fun res ->
      let s = Cresult.pp res in
      CLog.app (fun m -> m "Evaluated to: %s\n" s);
      res
    )

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
        let resulting_symbolic = eval_exp ~conc_session ~symb_session e in
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
        (* Repeatedly evaluate program: *)
        Concolic_riddler.reset ();
        Lwt_unix.with_timeout r.global_timeout_sec
        @@ fun () ->
          loop e
          @@ Options.Fun.run Session.with_options r Session.empty
  in
  Options.Fun.make f