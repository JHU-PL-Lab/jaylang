
open Core
open Lang.Ast
open Expr

module Error_msg = Lang.Value.Error_msg (Value)

(*
  It's likely a good idea to use real OCaml state and effects to get
  performance, but for this pass, we use a monad to encode the concolic
  semantics. It just helps with correctness that way.
*)
let eval_exp 
  (consts : Effects.Consts.t)
  (expr : Embedded.t) 
  : Status.Eval.t * Target.t list
  =
  let open Effects.Initialize (struct let c = consts end) in
  let open Effects.M in

  let rec eval (expr : Embedded.t) : Value.t m =
    let open Value.M in (* puts the value constructors in scope *)
    let%bind () = incr_step in
    match expr with
    (* Determinism *)
    | EDet e -> with_incr_depth @@ eval e
    | EEscapeDet e -> with_escaped_det @@ eval e
    (* Ints and bools -- constant expressions *)
    | EInt i -> return @@ VInt (i, Expression.const_int i)
    | EBool b -> return @@ VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
    | EDefer e -> eval e (* this concolic evaluator is eager *)
    | EUnit -> return VUnit
    | EVar id -> fetch id
    | EFunction { param ; body } ->
      let%bind env = read_env in
      return @@ VFunClosure { param ; closure = { body ; env } }
    | EId -> return VId
    | EFreeze e_freeze_body -> 
      let%bind env = read_env in
      return @@ VFrozen { body = e_freeze_body ; env }
    | EVariant { label ; payload = e_payload } -> 
      let%bind payload = eval e_payload in
      return @@ VVariant { label ; payload }
    | EUntouchable e ->
      let%bind v = eval e in
      return @@ VUntouchable v
    | EProject { record = e_record ; label } -> begin
      match%bind eval e_record with
      | (VRecord body | VModule body) as v -> begin
        match Map.find body label with
        | Some v -> return v
        | None -> type_mismatch @@ Error_msg.project_missing_label label v
      end
      | v -> type_mismatch @@ Error_msg.project_non_record label v
    end
    | EThaw e_frozen -> begin
      match%bind eval e_frozen with
      | VFrozen { body ; env } -> local (fun _ -> env) (eval body)
      | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
    end
    | ERecord record_body ->
      let%bind value_record_body =
        Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
          let%bind acc = acc_m in
          let%bind v = eval e in
          return @@ Map.set acc ~key ~data:v
        )
      in
      return @@ VRecord value_record_body
    | EModule stmt_ls ->
      let%bind module_body =
        let rec fold_stmts acc_m = function
          | [] -> acc_m
          | SUntyped { var ; defn } :: tl ->
            let%bind acc = acc_m in
            let%bind v = eval defn in
            local (Env.add var v) (
              fold_stmts (return @@ Map.set acc ~key:(RecordLabel.RecordLabel var) ~data:v) tl
            )
        in
        fold_stmts (return RecordLabel.Map.empty) stmt_ls
      in
      return @@ VModule module_body 
    | EIgnore { ignored ; body } ->
      let%bind _ : Value.t = eval ignored in
      eval body
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
      let%bind v = eval subject in
      match
        (* find the matching pattern and add to env any values capture by the pattern *)
        List.find_map patterns ~f:(fun (pat, body) ->
          match Value.matches v pat with
          | Some bindings -> Some (body, fun env ->
            List.fold bindings ~init:env ~f:(fun acc (v_bind, id_bind) -> Env.add id_bind v_bind acc)
          )
          | None -> None
        )
      with
      | Some (e, f) -> local f (eval e)
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | ELet { var ; defn ; body } ->
      let%bind v = eval defn in
      local (Env.add var v) (eval body)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func in
      match vfunc with
      | VId -> eval arg
      | VFunClosure { param ; closure } ->
        let%bind varg = eval arg in
        local (fun _ -> Env.add param varg closure.env) (eval closure.body)
      | _ -> type_mismatch @@ Error_msg.bad_appl vfunc
    end
    (* Operations -- build new expressions *)
    | EBinop { left ; binop ; right } -> begin
      let%bind vleft = eval left in
      let%bind vright = eval right in
      let k f e1 e2 op =
        return @@ f (Expression.op e1 e2 op)
      in
      let open Expression.Typed_binop in
      let v_int n = fun e -> VInt (n, e) in
      let v_bool b = fun e -> VBool (b, e) in
      match binop, vleft, vright with
      | BPlus        , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 + n2)) e1 e2 Plus
      | BMinus       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 - n2)) e1 e2 Minus
      | BTimes       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 * n2)) e1 e2 Times
      | BDivide      , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 / n2)) e1 e2 Divide
      | BModulus     , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 mod n2)) e1 e2 Modulus
      | BEqual       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 = n2)) e1 e2 Equal_int
      | BEqual       , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool Bool.(b1 = b2)) e1 e2 Equal_bool
      | BNeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
      | BLessThan    , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 < n2)) e1 e2 Less_than
      | BLeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
      | BGreaterThan , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 > n2)) e1 e2 Greater_than
      | BGeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
      | BAnd         , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 && b2)) e1 e2 And
      | BOr          , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 || b2)) e1 e2 And
      | _ -> type_mismatch @@ Error_msg.bad_binop vleft binop vright
    end
    | ENot e_not_body -> begin
      match%bind eval e_not_body with
      | VBool (b, e_b) -> return @@ VBool (not b, Expression.not_ e_b) 
      | v -> type_mismatch @@ Error_msg.bad_not v
    end
    | EIntensionalEqual { left ; right } ->
      let%bind vleft = eval left in
      let%bind vright = eval right in
      return @@ VBool (Value.equal vleft vright)
    (* Branching *)
    | EIf { cond ; true_body ; false_body } -> begin
      match%bind eval cond with
      | VBool (b, e) ->
        let body = if b then true_body else false_body in
        let%bind () = hit_branch (Direction.of_bool b) e in
        eval body
      | v -> type_mismatch @@ Error_msg.cond_non_bool v
    end
    | ECase { subject ; cases ; default } -> begin
      let int_cases = List.map cases ~f:Tuple2.get1 in
      match%bind eval subject with
      | VInt (i, e) -> begin
        let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
        match body_opt with
        | Some body -> (* found a matching case *)
          let other_cases = List.filter int_cases ~f:((<>) i) in
          let%bind () = hit_case (Direction.of_int i) e ~other_cases in
          eval body
        | None -> (* no matching case, so take default case *)
          let%bind () = hit_case (Direction.Case_default { not_in = int_cases }) e ~other_cases:int_cases in
          eval default
      end
      | v -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* Inputs *)
    | EPick_i () -> get_input (fun step -> Utils.Separate.I step)
    | EPick_b () -> get_input (fun step -> Utils.Separate.B step)
    (* Tables -- includes some branching *)
    | ETable -> return (VTable { alist = [] })
    | ETblAppl { tbl ; gen ; arg } -> begin
      match%bind eval tbl with
      | VTable mut_r -> begin
        let%bind v = eval arg in
        let%bind output_opt =
          List.fold mut_r.alist ~init:(return None) ~f:(fun acc_m (input, output) ->
            match%bind acc_m with
            | None -> 
              let (b, e) = Value.equal input v in
              let%bind () = hit_branch (Direction.of_bool b) e in
              if b
              then return (Some output)
              else return None
            | Some _ -> acc_m (* already found an output, so go unchanged *)
          )
        in
        match output_opt with
        | Some output -> return output
        | None ->
            let%bind new_output = with_escaped_det @@ eval gen in
            mut_r.alist <- (v, new_output) :: mut_r.alist;
            return new_output
      end
      | tb -> type_mismatch @@ Error_msg.appl_non_table tb
    end
    (* Failure cases *)
    | EAbort msg -> abort msg
    | EVanish () -> vanish
  in

  run (eval expr)

(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This section starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple interpretations, hitting a provably different
  path on each interpretation.
*)

let global_runtime = Utils.Safe_cell.create 0.0
let global_solvetime = Utils.Safe_cell.create 0.0

module Make (S : Solve.S) (P : Pause.S) (O : Options.V) = struct
  module TQ = Target_queue.All

  let empty_tq = Options.Arrow.appl TQ.of_options O.r ()

  let rec step : Embedded.t -> has_pruned:bool -> has_unknown:bool -> TQ.t -> Status.Terminal.t P.t =
    fun e ~has_pruned ~has_unknown tq ->
      let open P in
      let* () = pause () in
      let t0 = Caml_unix.gettimeofday () in
      match TQ.peek tq with
      | Some target -> begin
        let tq = TQ.remove tq target in
        let solve_result = S.solve (Target.to_expressions target) in
        let t1 = Caml_unix.gettimeofday () in
        let _ : float = Utils.Safe_cell.map (fun t -> t +. (t1 -. t0)) global_solvetime in
        match solve_result with
        | `Sat feeder -> begin
            let* () = pause () in
            let status, targets = eval_exp { target ; options = O.r ; input_feeder = feeder } e in
            let t2 = Caml_unix.gettimeofday () in
            let _ : float = Utils.Safe_cell.map (fun t -> t +. (t2 -. t1)) global_runtime in
            match status with
            | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as s -> P.return s
            | Finished { pruned } ->
              let has_pruned = has_pruned || pruned in
              step e ~has_pruned ~has_unknown (TQ.push_list tq targets)
          end
        | `Unknown -> let* () = pause () in step e ~has_pruned ~has_unknown:true tq
        | `Unsat -> let* () = pause () in step e ~has_pruned ~has_unknown tq
      end
      | None ->
        let _ : float = Utils.Safe_cell.map (fun t -> t +. (Caml_unix.gettimeofday () -. t0)) global_solvetime in
        if has_unknown then
          return Status.Unknown
        else if has_pruned then
          return Status.Exhausted_pruned_tree
        else
          return Status.Exhausted_full_tree

  (*
    We don't bootstrap by running `step` with one empty target because we want
    to run with all zeroes first. Just fencepost like this: run once, then start
    stepping.
  *)
  let eval : Embedded.t -> Status.Terminal.t P.t =
    fun e ->
      if not O.r.random then Interp_common.Rand.reset ();
      P.with_timeout O.r.global_timeout_sec @@ fun () ->
        let c =
          Effects.Consts.{ target = Target.make Path.empty
          ; options = O.r
          ; input_feeder = Input_feeder.zero }  
        in
        let t0 = Caml_unix.gettimeofday () in
        let status, targets = eval_exp c e in
        let _ : float = Utils.Safe_cell.map (fun t -> t +. (Caml_unix.gettimeofday () -. t0)) global_runtime in
        match status with
        | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as s -> P.return s
        | Finished { pruned } ->
          step e ~has_pruned:pruned ~has_unknown:false (TQ.push_list empty_tq targets)
end

module F = Make (Solve.Default) (Pause.Lwt)

let lwt_eval : (Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r e ->
    let module E = F (struct let r = r end) in
    E.eval e
