open Core
open Jayil.Ast
open Dvalue (* just to expose constructors *)
open Options.Fun.Infix

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

    module Crecord =
      struct
        type t = { denv : Denv.t ; dval : Dvalue.t ; step : int ; symb_session : Session.Symbolic.t }

        let return denv dval symb_session step = { denv ; dval ; step ; symb_session }
      end

    module Cerror =
      struct
        type t = Found_abort | Found_diverge | Type_mismatch | Reach_max_step

        let pp = function
          | Found_abort -> "Found abort in interpretation"
          | Type_mismatch -> "Type mismatch in interpretation"
          | Found_diverge -> "Found diverge"
          | Reach_max_step -> "Reach max steps during interpretation"
      end

    open Crecord
    open Cerror

    type t = (Crecord.t, Cerror.t * Session.Symbolic.t) result

    let show (x : t) : string =
      match x with
      | Ok r -> Dvalue.pp r.dval
      | Error (e, _) -> Cerror.pp e

    let get_session = function
    | Ok { symb_session = s ; _ } 
    | Error (_, s) -> s

    let return denv dval symb_session step =
      Result.return
      @@ Crecord.return denv dval symb_session step

    let err e f = fun s -> Result.fail (e, f s)

    let reach_max_step = err Reach_max_step Session.Symbolic.reach_max_step
    let found_abort = err Found_abort Session.Symbolic.found_abort
    let found_diverge = err Found_diverge Session.Symbolic.found_diverge
    let type_mismatch = err Type_mismatch Session.Symbolic.found_type_mismatch
  end

open Cresult

(*
  ----------
  BEGIN EVAL
  ----------

  This section is basically an interpreter injected with concolic logic.
  It is a single evaluation of the program, and it accumulates symbolic formulas.
*)

type k = Cresult.t -> Cresult.t

let eval_exp
  ~(symb_session : Session.Symbolic.t)
  (e : expr)
  : Session.Symbolic.t
  =
  let max_step = Session.Symbolic.get_max_step symb_session in

  let rec eval_exp
    ~(symb_session : Session.Symbolic.t)
    ~(step : int)
    (env : Denv.t)
    (Expr clauses : expr)
    (cont : k)
    : Cresult.t
    =
    match clauses with
    | [] -> failwith "empty clause list" (* safe because empty clause list is a parse error *)
    | clause :: [] -> eval_clause ~symb_session ~step env clause cont
    | clause :: nonempty_tl ->
      eval_clause ~symb_session ~step env clause (function
        | Ok { denv ; symb_session ; step ; _ } -> eval_exp ~symb_session ~step denv (Expr nonempty_tl) cont
        | res -> res
      )

  and eval_clause
    ~(symb_session : Session.Symbolic.t)
    ~(step : int)
    (env : Denv.t)
    (Clause (Var (x, _), cbody) : clause)
    (cont : k)
    : Cresult.t
    =
    let step = step + 1 in

    if step >= max_step
    then reach_max_step symb_session
    else
      let x_key = Concolic_key.create x step in

      let next ?(step : int = step) v s =
        cont @@ return (Denv.add env x v x_key) v s step
      in
      
      match cbody with
      | Value_body (Value_function vf) -> (** x = fun ... ; *)
        next (FunClosure (x, vf, env)) symb_session
      | Value_body (Value_record r) -> (** x = { ... } ; *)
        next (RecordClosure (r, env)) symb_session
      | Value_body (Value_int i as v) -> (** x = <int> ; *)
        next (Direct v) @@ Session.Symbolic.add_key_eq_int x_key i symb_session
      | Value_body (Value_bool b as v) -> (** x = <bool> ; *)
        next (Direct v) @@ Session.Symbolic.add_key_eq_bool x_key b symb_session
      | Var_body vx -> (** x = y ; *)
        let ret_val, ret_key = Denv.fetch env vx in
        next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
      | Conditional_body (cx, e1, e2) -> (** x = if y then e1 else e2 ; *)
        let cond_val, condition_key = Denv.fetch env cx in
        begin
          match cond_val with
          | Direct (Value_bool b) ->
            let this_branch = Branch.Runtime.{ branch_key = x_key ; condition_key ; direction = Branch.Direction.of_bool b } in
            let e = if b then e1 else e2 in
            eval_exp ~symb_session:(Session.Symbolic.hit_branch this_branch symb_session) ~step env e (function
            | Ok { denv = ret_env ; dval = ret_val ; symb_session ; step } ->
              let last_v = Jayil.Ast_tools.retv e in (* last defined value in the branch *)
              let ret_key = Denv.fetch_key ret_env last_v in
              next ~step ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
            | res -> res
            )
          | _ -> type_mismatch symb_session
        end
      | Input_body -> (** x = input ; *)
        let ret_val = Direct (Value_int (Session.Symbolic.get_feeder symb_session x_key)) in
        next ret_val @@ Session.Symbolic.add_input x_key ret_val symb_session
      | Appl_body (vf, varg) -> (** x = f y ; *) begin
        match Denv.fetch_val env vf with
        | FunClosure (_, Function_value (Var (param, _), body), fenv) ->
          (* increment step count so that the key for the parameter gets an identifier different than the clause *)
          let step = step + 1 in

          (* varg is the argument that fills in param *)
          let arg_val, arg_key = Denv.fetch env varg in
          let param_key = Concolic_key.create param step in
          let env' = Denv.add fenv param arg_val param_key in

          (* returned value of function *)
          eval_exp ~symb_session:(Session.Symbolic.add_alias param_key arg_key symb_session) ~step env' body (function
          | Ok { denv = ret_env ; dval = ret_val ; step ; symb_session } ->
            let last_v = Jayil.Ast_tools.retv body in
            let ret_key = Denv.fetch_key ret_env last_v in

            (* exit function: *)
            next ~step ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
          | res -> res
          )
        | _ -> type_mismatch symb_session
        end
      | Match_body (vy, p) -> (** x = y ~ <pattern> ; *)
        let b = check_pattern env vy p in
        next (Direct (Value_bool b)) @@ Session.Symbolic.add_key_eq_bool x_key b symb_session
      | Projection_body (v, label) -> begin
        match Denv.fetch_val env v with
        | RecordClosure (Record_value r, denv) -> begin
          match Ident_map.find_opt label r with
          | Some proj_var ->
              let ret_val, ret_key = Denv.fetch denv proj_var in
              next ret_val @@ Session.Symbolic.add_alias x_key ret_key symb_session
          | None -> type_mismatch symb_session
        end
        | Direct (Value_record (Record_value _record)) ->
          failwith "project should also have a closure"
        | _ -> type_mismatch symb_session
        end
      | Not_body vy -> (** x = not y ; *)
        let y_val, y_key = Denv.fetch env vy in
        begin
          match y_val with
          | Direct (Value_bool b) ->
            next (Direct (Value_bool (not b))) @@ Session.Symbolic.add_not x_key y_key symb_session
          | _ -> type_mismatch symb_session
        end
      | Binary_operation_body (vy, op, vz) -> (** x = y op z *) begin
        let y, y_key = Denv.fetch env vy in
        let z, z_key = Denv.fetch env vz in
        match y, z with
        | Direct v1, Direct v2 -> begin
          let k v binop = (* continuation alias for shorter code in following match *)
            next (Direct v)
            @@ Session.Symbolic.add_binop x_key binop y_key z_key symb_session
          in
          let open Expression.Untyped_binop in
          match op, v1, v2 with
          | Binary_operator_plus, Value_int n1, Value_int n2                  -> k (Value_int  (n1 + n2)) Plus
          | Binary_operator_minus, Value_int n1, Value_int n2                 -> k (Value_int  (n1 - n2)) Minus
          | Binary_operator_times, Value_int n1, Value_int n2                 -> k (Value_int  (n1 * n2)) Times
          | Binary_operator_divide, Value_int n1, Value_int n2 when n2 <> 0   -> k (Value_int  (n1 / n2)) Divide
          | Binary_operator_modulus, Value_int n1, Value_int n2 when n2 <> 0  -> k (Value_int  (n1 mod n2)) Modulus
          | Binary_operator_less_than, Value_int n1, Value_int n2             -> k (Value_bool (n1 < n2)) Less_than
          | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 -> k (Value_bool (n1 <= n2)) Less_than_eq
          | Binary_operator_equal_to, Value_int n1, Value_int n2              -> k (Value_bool (n1 = n2)) Equal_int
          | Binary_operator_equal_to, Value_bool b1, Value_bool b2            -> k (Value_bool (Bool.(b1 = b2))) Equal_bool
          | Binary_operator_and, Value_bool b1, Value_bool b2                 -> k (Value_bool (b1 && b2)) And
          | Binary_operator_or, Value_bool b1, Value_bool b2                  -> k (Value_bool (b1 || b2)) Or
          | Binary_operator_not_equal_to, Value_int n1, Value_int n2          -> k (Value_bool (n1 <> n2)) Not_equal
          | _ -> type_mismatch symb_session (* includes mod or divide by 0 *)
        end 
        | _ -> type_mismatch symb_session
      end
      | Abort_body -> found_abort symb_session
      | Diverge_body -> found_diverge symb_session
  in

  get_session
  @@ eval_exp ~symb_session ~step:0 Denv.empty e (fun res ->
      let s = Cresult.show res in
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
let rec loop (e : expr) (session : Session.t) (symb_session : Session.Symbolic.t) : Session.Status.t Lwt.t =
  let open Lwt.Infix in
  let%lwt () = Lwt.pause () in
  CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session));
  let t0 = Caml_unix.gettimeofday () in
  let resulting_symbolic = eval_exp ~symb_session e in
  let t1 = Caml_unix.gettimeofday () in
  CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0));
  Session.next
  @@ Session.accum_symbolic session resulting_symbolic
  >>= begin function
    | `Done status ->
        CLog.app (fun m -> m "\n------------------------------\nFinishing concolic evaluation...\n\n");
        CLog.app (fun m -> m "Ran %d interpretations.\n" (Session.run_num session));
        CLog.app (fun m -> m "Session status: %s.\n" (Session.Status.to_string status));
        Lwt.return status
    | `Next (session, symb_session) -> loop e session symb_session
    end

let lwt_eval : (Jayil.Ast.expr, Session.Status.t Lwt.t) Options.Fun.a =
  (* Dj_common.Log.init { Dj_common.Global_config.default_config with log_level_concolic = Some Debug }; *)
  Session.of_options
  >>> (Options.Fun.make
  @@ fun r (session, symb_session) (e : expr) ->
      C_sudu.set_timeout (Core.Time_float.Span.of_sec r.solver_timeout_sec);
      if not r.random then C_random.reset ();
      CLog.app (fun m -> m "\nStarting concolic execution...\n");
      Lwt_unix.with_timeout r.global_timeout_sec
      @@ fun () -> loop e session symb_session (* repeatedly evaluate program *)
  )
  |> Options.Fun.thaw