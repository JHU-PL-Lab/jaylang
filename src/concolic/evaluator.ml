
open Core
open Lang.Ast
open Expr
open Value

module State_M = struct
  module State = struct
    type t =
      { step : int 
      ; max_step : int 
      ; session : Eval_session.t }
  end

  include Lang.Interp_monad.Make (State) (struct
    type value = Value.t
    include Env
  end) (struct
    include Status.Eval
    let fail_on_nondeterminism_misuse (state : State.t) : t =
      Eval_session.abort "Nondeterminism used when not allowed." state.session
    let fail_on_fetch (id : Ident.t) (state : State.t) : t =
      Eval_session.unbound_variable state.session id
  end)

  let run (x : 'a m) (session : Eval_session.t) : Status.Eval.t =
    x.run
      ~reject:Fn.id
      ~accept:(fun _ s -> Eval_session.finish s.session)
      State.{ step = 0 ; max_step = Eval_session.get_max_step session ; session }
      Read.empty

  let[@inline always][@specialise] modify_session (f : Eval_session.t -> Eval_session.t) : unit m =
    modify (fun s -> { s with session = f s.session })

  (*
    As a general observation, it is more efficient to be hands-on and write this
    using the structure of State_read_result instead of using `read`, `return`, etc.
    I suppose this is due to better inlining. I consider the tradeoff in favor
    of more efficiency to be worth the less general code.
  *)
  let incr_step : int m =
    { run =
      fun ~reject ~accept s _ ->
        let step = s.step + 1 in
        if step > s.max_step
        then reject @@ Eval_session.reach_max_step s.session
        else accept step { s with step }
    }

  let[@inline always][@specialise] modify_and_return (f : Eval_session.t -> Eval_session.t * 'a) : 'a m =
    { run =
      fun ~reject:_ ~accept s _ ->
        let session, a = f s.session in
        accept a { s with session }
    }

  let[@inline always] fail_map (f : Eval_session.t -> Status.Eval.t) : 'a m =
    let%bind ({ session ; _ }, _) = read in
    fail @@ f session

  let diverge : 'a m = fail_map Eval_session.diverge
  let abort msg: 'a m = fail_map @@ Eval_session.abort msg
  let type_mismatch msg : 'a m = fail_map @@ Eval_session.type_mismatch msg
end

module Error_msg = Lang.Value.Error_msg (Value)

(*
  The only time we really update the session is on max step or hitting a branch.
  Can I then optimize it to not have to pass it around most of the time?
*)
let eval_exp 
  (session : Eval_session.t)
  (expr : Embedded.t) 
  : Status.Eval.t
  =
  let open State_M in

  let rec eval (expr : Embedded.t) : Value.t m =
    let open Value.M in (* puts the value constructors in scope *)
    let%bind step = incr_step in
    match expr with
    (* Determinism *)
    | EDet e -> with_incr_depth @@ eval e
    | EEscapeDet e -> with_escaped_det @@ eval e
    (* Ints and bools -- constant expressions *)
    | EInt i -> return @@ VInt (i, Expression.const_int i)
    | EBool b -> return @@ VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
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
    | EProject { record = e_record ; label } -> begin
      match%bind eval e_record with
      | VRecord record_body as v -> begin
        match Map.find record_body label with
        | Some v -> return v
        | None -> type_mismatch @@ Error_msg.project_missing_label label v
      end
      | v -> type_mismatch @@ Error_msg.project_non_record label v
    end
    | EThaw e_frozen -> begin
      match%bind eval e_frozen with
      | VFrozen { body ; env } -> local_env (fun _ -> env) (eval body)
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
      | Some (e, f) -> local_env f (eval e)
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | ELet { var ; defn ; body } ->
      let%bind v = eval defn in
      local_env (Env.add var v) (eval body)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func in
      match vfunc with
      | VId -> eval arg
      | VFunClosure { param ; closure } ->
        let%bind varg = eval arg in
        local_env (fun _ -> Env.add param varg closure.env) (eval closure.body)
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
        let%bind () = modify_session @@ Eval_session.hit_branch (Direction.of_bool b) e in
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
          let%bind () = modify_session @@ Eval_session.hit_case (Direction.of_int i) e ~other_cases in
          eval body
        | None -> (* no matching case, so take default case *)
          let%bind () = modify_session @@ Eval_session.hit_case (Direction.Case_default { not_in = int_cases }) e ~other_cases:int_cases in
          eval default
      end
      | v -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* Inputs *)
    | EPick_i ->
      let%bind () = assert_nondeterminism in
      modify_and_return @@ Eval_session.get_input (Stepkey.I step)
    | EPick_b ->
      let%bind () = assert_nondeterminism in
      modify_and_return @@ Eval_session.get_input (Stepkey.B step)
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
              let%bind () = modify_session @@ Eval_session.hit_branch (Direction.of_bool b) e in
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
    | EDiverge -> diverge
  in

  run (eval expr) session

(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple interpretations, hitting a provably different
  path on each interpretation.
*)

let global_runtime = Utils.Safe_cell.create 0.0
let global_solvetime = Utils.Safe_cell.create 0.0

module Make (S : Solve.S) (P : Pause.S) (O : Options.V) = struct
  module Intra = Intra_session.Make (S) (P) (O)

  let rec loop (e : Embedded.t) (main_session : Intra.t) (session : Eval_session.t) : Status.Terminal.t P.t =
    let open P in
    let* () = pause () in
    let t0 = Caml_unix.gettimeofday () in
    let res = eval_exp session e in
    let t1 = Caml_unix.gettimeofday () in
    let _ = Utils.Safe_cell.map ((+.) (t1 -. t0)) global_runtime in
    Intra.next
    @@ Intra.accum_eval main_session res
    >>= begin function
      | `Done status ->
        let t2 = Caml_unix.gettimeofday () in
        let _ = Utils.Safe_cell.map ((+.) (t2 -. t1)) global_solvetime in
        return status
      | `Next (session, symb_session) ->
        let t2 = Caml_unix.gettimeofday () in
        let _ = Utils.Safe_cell.map ((+.) (t2 -. t1)) global_solvetime in
        loop e session symb_session
      end

  let eval : Embedded.t -> Status.Terminal.t P.t =
    fun e ->
      if not O.r.random then C_random.reset ();
      let session = Options.Arrow.appl Eval_session.with_options O.r Eval_session.empty in
      P.with_timeout O.r.global_timeout_sec
      @@ fun () -> loop e Intra.empty session
end

module F = Make (Solve.Default) (Pause.Lwt)

let lwt_eval : (Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r e ->
    let module E = F (struct let r = r end) in
    E.eval e
