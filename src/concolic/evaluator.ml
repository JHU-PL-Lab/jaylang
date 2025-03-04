
open Core
open Lang.Ast
open Expr
open Value

(*
  General state monad that supports error but is significantly more efficient
  than using the Result monad inside of a normal state monad.
  Credit for this idea goes to the writer of the following post:
    https://discuss.ocaml.org/t/can-a-state-monad-be-optimized-out-with-flambda/9841/5?u=brandon

  I have further added in the reader monad for a non-propagating element to the state.
  This is nice because we could otherwise be passing around the environment in a "reader" style while
  letting the monad handle other threading. Instead, it feels best to centralize the core
  effects and prevent the proliferation of inconsistent "mini monad" implementations
  scattered throughout the code.

  Brandon's tenth rule: "Any sufficiently complicated functional OCaml program contains several ad hoc,
  informally-specified, boilerplate-ridden, unrolled implementations of common monads". ... that may
  *sometimes* be done better with a monad when many of those instances occur overlapping at once.
*)
module FMonad (State : T) (Read : T) (Err : T) = struct
  type 'a m = {
    run : 'r. reject:(Err.t -> 'r) -> accept:('a -> State.t -> 'r) -> State.t -> Read.t -> 'r
  }

  let[@inline always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    { run =
      fun ~reject ~accept s r ->
        x.run s r ~reject ~accept:(fun x s ->
          (f x).run ~reject ~accept s r
        )
    }

  let[@inline always] return (a : 'a) : 'a m =
    { run = fun ~reject:_ ~accept s _ -> accept a s }

  let read : (State.t * Read.t) m =
    { run = fun ~reject:_ ~accept s r -> accept (s, r) s }

  let[@inline always] modify (f : State.t -> State.t) : unit m =
    { run =
      fun ~reject:_ ~accept s _ ->
        accept () (f s)
    }

  let[@inline always] fail (e : Err.t) : 'a m =
    { run = fun ~reject ~accept:_ _ _ -> reject e }

  let[@inline always] local (f : Read.t -> Read.t) (x : 'a m) : 'a m =
    { run = fun ~reject ~accept s r -> x.run ~reject ~accept s (f r) }
end

module State_M = struct
  module State = struct
    type t = { step : int ; session : Eval_session.t }
  end

  include FMonad (State) (Env) (Status.Eval)

  let run (x : 'a m) (session : Eval_session.t) : Status.Eval.t =
    x.run
      ~reject:Fn.id
      ~accept:(fun _ s -> Eval_session.finish s.session)
      State.{ step = 0 ; session }
      Env.empty

  let[@inline always] modify_session (f : Eval_session.t -> Eval_session.t) : unit m =
    modify (fun s -> { s with session = f s.session })

  (*
    As a general observation, it is more efficient to be hands-on and write this
    using the structure of FMonad instead of using `read`, `return`, etc.
    I suppose this is due to better inlining. I consider the tradeoff in favor
    of more efficiency worth the less general code.
  *)
  let incr_step : int m =
    { run =
      fun ~reject ~accept s _ ->
        let step = s.step + 1 in
        if step > Eval_session.get_max_step s.session
        then reject @@ Eval_session.reach_max_step s.session
        else accept step { s with step }
    }

  let[@inline always] modify_and_return (f : Eval_session.t -> Eval_session.t * 'a) : 'a m =
    { run =
      fun ~reject:_ ~accept s _ ->
        let session, a = f s.session in
        accept a { s with session }
    }

  let read_env : Env.t m =
    let%bind (_, e) = read in
    return e

  let[@inline always] fetch (id : Ident.t) : Value.t m =
    let%bind env = read_env in
    return @@ Env.fetch id env

  let[@inline always] fail_map (f : Eval_session.t -> Status.Eval.t) : 'a m =
    let%bind ({ session ; _ }, _) = read in
    fail @@ f session

  let abort : 'a m = fail_map Eval_session.abort
  let diverge : 'a m = fail_map Eval_session.diverge
  let type_mismatch s : 'a m = fail_map @@ Fn.flip Eval_session.type_mismatch s
end

module Error_msg = struct
  let project_non_record label v =
    Format.sprintf "Label %s not found in non-record `%s`" (RecordLabel.to_string label) (Value.to_string v)

  let project_missing_label label record =
    Format.sprintf "Label %s not found in record %s" (RecordLabel.to_string label) (Value.to_string record)

  let thaw_non_frozen v =
    Format.sprintf "Thaw non-frozen value `%s`" (Value.to_string v)

  let pattern_not_found patterns v =
    Format.sprintf "Value `%s` not in pattern list [ %s ]"
      (Value.to_string v)
      (String.concat ~sep:", " @@ List.map patterns ~f:(fun (p, _) -> Pattern.to_string p))

  let bad_appl vfunc varg =
    Format.sprintf "Apply `%s` to non-function `%s`" (Value.to_string varg) (Value.to_string vfunc)

  let bad_binop vleft binop vright =
    Format.sprintf "Bad binop %s %s %s"
      (Value.to_string vleft)
      (Binop.to_string binop)
      (Value.to_string vright)

  let bad_not v =
    Format.sprintf "Bad unary operation `not %s`" (Value.to_string v)

  let cond_non_bool v = 
    Format.sprintf "Condition on non-bool `%s`" (Value.to_string v)

  let case_non_int v = 
    Format.sprintf "Case on non-int `%s`" (Value.to_string v)
end

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
    (* Ints and bools -- constant expressions *)
    | EInt i -> return @@ VInt (i, Expression.const_int i)
    | EBool b -> return @@ VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
    | EVar id -> fetch id
    | EFunction { param ; body } ->
      let%bind env = read_env in
      return @@ VFunClosure { param ; body = { expr = body ; env } }
    | EId -> return VId
    | EFreeze e_freeze_body -> 
      let%bind env = read_env in
      return @@ VFrozen { expr = e_freeze_body ; env }
    | EVariant { label ; payload = e_payload } -> 
      let%bind payload = eval e_payload in
      return @@ VVariant { label ; payload }
    | EProject { record = e_record ; label } -> begin
      let%bind v = eval e_record in
      match v with
      | VRecord record_body -> begin
        match Map.find record_body label with
        | Some v -> return v
        | None -> type_mismatch @@ Error_msg.project_missing_label label v
      end
      | _ -> type_mismatch @@ Error_msg.project_non_record label v
    end
    | EThaw e_frozen -> begin
      let%bind v = eval e_frozen in
      match v with
      | VFrozen { expr ; env } -> local (fun _ -> env) (eval expr)
      | _ -> type_mismatch @@ Error_msg.thaw_non_frozen v
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
    | EIgnore { ignored ; cont } ->
      let%bind _ : Value.t = eval ignored in
      eval cont
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
      let%bind v = eval subject in
      match
        (* find the matching pattern and add to env any values capture by the pattern *)
        List.find_map patterns ~f:(fun (pat, body) ->
          match pat, v with
          | PAny, _ -> Some (body, Fn.id)
          | PVariable id, _ -> Some (body, Env.add id v)
          | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
              when VariantLabel.equal variant_label label ->
            Some (body, Env.add payload_id payload)
          | _ -> None
          )
      with
      | Some (e, f) -> local f (eval e)
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | ELet { var ; body ; cont } ->
      let%bind v = eval body in
      local (Env.add var v) (eval cont)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func in
      let%bind varg = eval arg in
      match vfunc with
      | VId -> return varg
      | VFunClosure { param ; body } ->
        local (fun _ -> Env.add param varg body.env) (eval body.expr)
      | _ -> type_mismatch @@ Error_msg.bad_appl vfunc varg
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
      let%bind v = eval e_not_body in
      match v with
      | VBool (b, e_b) -> return @@ VBool (not b, Expression.not_ e_b) 
      | _ -> type_mismatch @@ Error_msg.bad_not v
    end
    (* Branching *)
    | EIf { cond ; true_body ; false_body } -> begin
      let%bind v = eval cond in
      match v with
      | VBool (b, e) ->
        let body = if b then true_body else false_body in
        let%bind () = modify_session @@ Eval_session.hit_branch (Direction.of_bool b) e in
        eval body
      | _ -> type_mismatch @@ Error_msg.cond_non_bool v
    end
    | ECase { subject ; cases ; default } -> begin
      let%bind v = eval subject in
      let int_cases = List.map cases ~f:Tuple2.get1 in
      match v with
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
      | _ -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* Inputs *)
    | EPick_i -> modify_and_return @@ Eval_session.get_input (Stepkey.I step)
    | EPick_b -> modify_and_return @@ Eval_session.get_input (Stepkey.B step)
    (* Failure cases *)
    | EAbort -> abort
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

module Make (S : Solve.S) (P : Pause.S) (O : Options.V) = struct
  module Intra = Intra_session.Make (S) (P) (O)

  let rec loop (e : Embedded.t) (main_session : Intra.t) (session : Eval_session.t) : Status.Terminal.t P.t =
    let open P in
    let* () = pause () in
    let res = eval_exp session e in
    Intra.next
    @@ Intra.accum_eval main_session res
    >>= begin function
      | `Done status -> return status
      | `Next (session, symb_session) -> loop e session symb_session
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
