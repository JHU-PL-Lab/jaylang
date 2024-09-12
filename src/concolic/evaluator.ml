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

module Crecord =
  struct
    type t = { denv : Denv.t ; dval : Dvalue.t }
  end

open Crecord

module Cresult =
  struct
    type 'a t =
      | Ok of 'a
      | Found_abort
      | Type_mismatch
      | Found_failed_assume
      | Reach_max_step

    let return a = Ok a
  end

module Cstate =
  struct
    type 'a m = Session.Symbolic.t * int -> (Session.Symbolic.t * int) * 'a Cresult.t

    let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
      fun s ->
        let ss, r = x s in
        match r with
        | Ok a -> f a ss
        | Found_abort -> ss, Found_abort
        | Type_mismatch -> ss, Type_mismatch
        | Found_failed_assume -> ss, Found_failed_assume
        | Reach_max_step -> ss, Reach_max_step

    let return (a : 'a) : 'a m =
      fun s -> s, Ok a

    let read : Session.Symbolic.t m =
      function (ss, _) as s -> s, Ok ss

    let modify (f : Session.Symbolic.t -> Session.Symbolic.t) : unit m =
      fun (s, i) -> (f s, i), Ok ()

    let incr_step : int m =
      fun (s, i) -> (s, i + 1), Ok (i + 1)

    let run (x : 'a m) (s : Session.Symbolic.t) : Session.Symbolic.t * 'a Cresult.t =
      let (ss, _), a = x (s, 0) in ss, a
    
    let log (x : 'a m) (f : 'a -> string) : unit m =
      fun s ->
        let ss, r = x s in
        let str = 
          match r with
          | Ok a -> f a
          | Found_abort -> "Found abort in interpretation"
          | Type_mismatch -> "Type mismatch in interpretation"
          | Reach_max_step -> "Reach max steps during interpretation"
          | Found_failed_assume -> "Found failed assume or assert"
        in
        CLog.app (fun m -> m "%s\n" str);
        ss, Ok ()

    let reach_max_step : 'a. 'a m =
      let%bind () = modify Session.Symbolic.reach_max_step in
      fun s -> s, Cresult.Reach_max_step

    let found_abort : 'a. 'a m =
      let%bind () = modify Session.Symbolic.found_abort in
      fun s -> s, Cresult.Found_abort

    let failed_assume : 'a. 'a m =
      let%bind () = modify Session.Symbolic.fail_assume in
      fun s -> s, Cresult.Found_failed_assume

    let type_mismatch : 'a. 'a m =
      let%bind () = modify Session.Symbolic.found_type_mismatch in
      fun s -> s, Cresult.Type_mismatch
  end

open Cstate
(* open Cresult *)

(*
  ----------
  BEGIN EVAL
  ----------

  This section is basically an interpreter injected with concolic logic.
  It is a single evaluation of the program, and it accumulates symbolic formulas.

  This version has a state monad, which I think makes it easy to read and track
  where changes to the step count and session are happening. And while it is stack
  safe with the tail calls, it is slow when run for many steps. It is much more
  efficient to handle the steps and session manually, so that is what is on the
  main branch.
*)

type s = Crecord.t m
type c = s -> s

let eval_exp
  (symb_session : Session.Symbolic.t)
  (e : expr)
  : Session.Symbolic.t
  =
  let max_step = Session.Symbolic.get_max_step symb_session in

  let rec eval_exp
    (env : Denv.t)
    (Expr clauses : expr)
    (cont : c)
    : s
    =
    match clauses with
    | [] -> failwith "empty clause list" (* safe because empty clause list is a parse error *)
    | clause :: [] -> eval_clause env clause cont
    | clause :: nonempty_tl ->
      eval_clause env clause (fun res ->
        let%bind r = res in
        eval_exp r.denv (Expr nonempty_tl) cont
      )

  and eval_clause
    (env : Denv.t)
    (Clause (Var (x, _), cbody) : clause)
    (cont : c)
    : s
    =
    let%bind step = incr_step in

    if step >= max_step
    then reach_max_step
    else
      let x_key = Concolic_key.create x step in

      let next v =
        cont @@ return { denv = Denv.add env x v x_key ; dval = v }
      in
      
      match cbody with
      | Value_body ((Value_function vf) as v) ->
        (* x = fun ... ; *)
        let%bind () = modify @@ Session.Symbolic.add_key_eq_val x_key v in
        next (FunClosure (x, vf, env))
      | Value_body ((Value_record r) as v) ->
        (* x = { ... } ; *)
        let%bind () = modify @@ Session.Symbolic.add_key_eq_val x_key v in
        next (RecordClosure (r, env))
      | Value_body v -> 
        (* x = <bool or int> ; *)
        let%bind () = modify @@ Session.Symbolic.add_key_eq_val x_key v in
        next (Direct v)
      | Var_body vx ->
        (* x = y ; *)
        let ret_val, ret_key = Denv.fetch env vx in
        let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
        next ret_val
      | Conditional_body (cx, e1, e2) -> 
        (* x = if y then e1 else e2 ; *)
        let cond_val, condition_key = Denv.fetch env cx in
        let%bind cond_bool =
          match cond_val with
          | Direct (Value_bool b) -> return b
          | _ -> type_mismatch
        in
        let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool cond_bool } in
        let e = if cond_bool then e1 else e2 in
        let%bind () = modify @@ Session.Symbolic.hit_branch this_branch in
        eval_exp env e (fun res ->
          let%bind r = res in
          let last_v = Jayil.Ast_tools.retv e in
          let ret_key = Denv.fetch_key r.denv last_v in (* last defined value in the branch *)
          let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
          next r.dval
        )
      | Input_body ->
        (* x = input ; *)
        let ret_val = Direct (Value_int (Session.Symbolic.get_feeder symb_session x_key)) in
        let%bind () = modify @@ Session.Symbolic.add_input x_key ret_val in
        next ret_val
      | Appl_body (vf, varg) -> begin 
        (* x = f y ; *)
        match Denv.fetch_val env vf with
        | FunClosure (_, Function_value (Var (param, _), body), fenv) ->
          (* increment step count so that the key for the parameter gets an identifier different than the clause *)
          let%bind step = incr_step in

          (* varg is the argument that fills in param *)
          let arg, arg_key = Denv.fetch env varg in
          let param_key = Concolic_key.create param step in
          let env' = Denv.add fenv param arg param_key in

          let%bind () = modify @@ Session.Symbolic.add_alias param_key arg_key in

          (* returned value of function *)
          eval_exp env' body (fun res ->
            let%bind r = res in
            let last_v = Jayil.Ast_tools.retv body in
            let ret_key = Denv.fetch_key r.denv last_v in

            (* exit function: *)
            let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
            next r.dval 
          )
        | _ -> type_mismatch
        end
      | Match_body (vy, p) ->
        (* x = y ~ <pattern> ; *)
        let match_res = Value_bool (check_pattern env vy p) in
        let y_key = Denv.fetch_key env vy in
        let%bind () = modify @@ Session.Symbolic.add_match x_key y_key p in
        next (Direct match_res)
      | Projection_body (v, label) -> begin
        match Denv.fetch_val env v with
        | RecordClosure (Record_value r, denv) ->
          let proj_var = Ident_map.find label r in
          let ret_val, ret_key = Denv.fetch denv proj_var in
          let%bind () = modify @@ Session.Symbolic.add_alias x_key ret_key in
          next ret_val
        | Direct (Value_record (Record_value _record)) ->
          failwith "project should also have a closure"
        | _ -> type_mismatch
        end
      | Not_body vy ->
        (* x = not y ; *)
        let y_val, y_key = Denv.fetch env vy in
        let%bind b =
          match y_val with
          | Direct (Value_bool b) -> return b
          | _ -> type_mismatch
        in
        let%bind () = modify @@ Session.Symbolic.add_not x_key y_key in
        next (Direct (Value_bool (not b)))
      | Binary_operation_body (vy, op, vz) -> begin
        (* x = y op z *)
        let y, y_key = Denv.fetch env vy in
        let z, z_key = Denv.fetch env vz in
        let%bind () = modify @@ Session.Symbolic.add_binop x_key op y_key z_key in
        let%bind dval =
        match y, z with
        | Direct v1, Direct v2 ->
          begin
            match op, v1, v2 with
            | Binary_operator_plus, Value_int n1, Value_int n2                  -> return @@ Direct (Value_int  (n1 + n2))
            | Binary_operator_minus, Value_int n1, Value_int n2                 -> return @@ Direct (Value_int  (n1 - n2))
            | Binary_operator_times, Value_int n1, Value_int n2                 -> return @@ Direct (Value_int  (n1 * n2))
            | Binary_operator_divide, Value_int n1, Value_int n2                -> return @@ Direct (Value_int  (n1 / n2))
            | Binary_operator_modulus, Value_int n1, Value_int n2               -> return @@ Direct (Value_int  (n1 mod n2))
            | Binary_operator_less_than, Value_int n1, Value_int n2             -> return @@ Direct (Value_bool (n1 < n2))
            | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> return @@ Direct (Value_bool (n1 <= n2))
            | Binary_operator_equal_to, Value_int n1, Value_int n2              -> return @@ Direct (Value_bool (n1 = n2))
            | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> return @@ Direct (Value_bool (Bool.(b1 = b2)))
            | Binary_operator_and, Value_bool b1, Value_bool b2                 -> return @@ Direct (Value_bool (b1 && b2))
            | Binary_operator_or, Value_bool b1, Value_bool b2                  -> return @@ Direct (Value_bool (b1 || b2))
            | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> return @@ Direct (Value_bool (n1 <> n2))
            | _ -> type_mismatch
          end
        | _ -> type_mismatch
        in
        next dval
      end
      | Abort_body -> found_abort
      | Assert_body cx | Assume_body cx ->
        let cond_val, cond_key = Denv.fetch env cx in 
        match cond_val with
        | Direct (Value_bool b) ->
          let%bind () = modify @@ Session.Symbolic.found_assume cond_key in
          if not b
          then failed_assume
          else next cond_val
        | _ -> type_mismatch
  in

  let res_session, _ = run
    (eval_exp Denv.empty e (fun res ->
        let%bind () = log res (fun r -> Dvalue.pp r.dval) in
        res
    ))
    symb_session
  in
  res_session

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
      | `Next (session, symb_session) ->
        CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session));
        let t0 = Caml_unix.gettimeofday () in
        let resulting_symbolic = eval_exp symb_session e in
        let t1 = Caml_unix.gettimeofday () in
        CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0));
        loop e
        @@ Session.accum_symbolic session resulting_symbolic
      end

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let lwt_eval : (Jayil.Ast.expr, Session.Status.t Lwt.t) Options.Fun.t =
  (* Dj_common.Log.init { Dj_common.Global_config.default_config with log_level_concolic = Some Debug }; *)
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