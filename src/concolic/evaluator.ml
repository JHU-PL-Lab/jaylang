
open Core
open Lang.Ast
open Expr
open Value

module State_M = struct
  module State = struct
    type t = { step : int ; session : Eval_session.t }
  end

  type 'a m = State.t -> (State.t * 'a, Status.Eval.t) result

  let[@inline always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    fun s ->
      Result.bind (x s) ~f:(fun (s, a) -> f a s)
    
  let[@inline always] return (a : 'a) : 'a m =
    fun s -> Result.return (s, a)

  let[@inline always] run (x : 'a m) (session : Eval_session.t) : Status.Eval.t =
    match x { step = 0 ; session } with
    | Ok ({ session ; _ }, _) -> Eval_session.finish session
    | Error e -> e

  let[@inline always] modify f : unit m =
    fun s -> Result.return ({ s with session = f s.session }, ())

  let[@inline always] modify_and_return (f : Eval_session.t -> Eval_session.t * 'a) : 'a m =
    fun s -> let sess, a = f s.session in Result.return ({ s with session = sess }, a)

  let[@inline always] incr_step : unit m =
    fun { step ; session } ->
      let s' = step + 1 in
      if s' > Eval_session.get_max_step session
      then Result.fail @@ Eval_session.reach_max_step session
      else Result.return (State.{ step = s' ; session }, ())

  let[@inline always] step : int m =
    fun s -> Result.return (s, s.step)

  let[@inline always] fail (f : Eval_session.t -> Status.Eval.t) : 'a m = 
    fun { session ; _ } -> Result.fail @@ f session

  let abort : 'a m = fail Eval_session.abort
  let diverge : 'a m = fail Eval_session.diverge
  let type_mismatch reason : 'a m = fail @@ Fn.flip Eval_session.type_mismatch reason
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
  ~(session : Eval_session.t)
  (expr : Embedded.t) 
  : Status.Eval.t
  =
  let open State_M in

  let rec eval (expr : Embedded.t) (env : Env.t) : Value.t m =
    let open Value.M in (* puts the value constructors in scope *)
    let%bind () = incr_step in
    match expr with
    (* Ints and bools -- constant expressions *)
    | EInt i -> 
      return @@ VInt (i, Expression.const_int i)
    | EBool b -> 
      return @@ VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
    | EVar id -> 
      return @@ Env.fetch env id
    | EFunction { param ; body } ->
      return @@ VFunClosure { param ; body = { expr = body ; env } }
    | EId -> 
      return VId
    | EFreeze e_freeze_body -> 
      return @@ VFrozen { expr = e_freeze_body ; env }
    | EVariant { label ; payload = e_payload } -> 
      let%bind payload  = eval e_payload env in
      return @@ VVariant { label ; payload }
    | EProject { record = e_record ; label } -> begin
      let%bind v = eval e_record env in
      match v with
      | VRecord record_body -> begin
        match Map.find record_body label with
        | Some v -> return v
        | None -> type_mismatch @@ Error_msg.project_missing_label label v
      end
      | _ -> type_mismatch @@ Error_msg.project_non_record label v
    end
    | EThaw e_frozen -> begin
      let%bind v = eval e_frozen env in
      match v with
      | VFrozen { expr ; env } -> eval expr env
      | _ -> type_mismatch @@ Error_msg.thaw_non_frozen v
    end
    | ERecord record_body ->
      let%bind value_record_body =
        Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
          let%bind acc = acc_m in
          let%bind v = eval e env in
          return @@ Map.set acc ~key ~data:v
        )
      in
      return @@ VRecord value_record_body
    | EIgnore { ignored ; cont } ->
      let%bind _ : Value.t = eval ignored env in
      eval cont env
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
      let%bind v = eval subject env in
      match
        (* find the matching pattern and add to env any values capture by the pattern *)
        List.find_map patterns ~f:(fun (pat, body) ->
          match pat, v with
          | PAny, _ -> Some (body, env)
          | PVariable id, _ -> Some (body, Env.add env id v)
          | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
              when VariantLabel.equal variant_label label ->
            Some (body, Env.add env payload_id payload)
          | _ -> None
          )
      with
      | Some (e, env) -> eval e env
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | ELet { var ; body ; cont } ->
      let%bind v = eval body env in
      eval cont (Env.add env var v)
    | EAppl { func ; arg } -> begin
      let%bind vfunc = eval func env in
      let%bind varg = eval arg env in
      match vfunc with
      | VId -> return varg
      | VFunClosure { param ; body } ->
        eval body.expr (Env.add body.env param varg)
      | _ -> type_mismatch @@ Error_msg.bad_appl vfunc varg
    end
    (* Operations -- build new expressions *)
    | EBinop { left ; binop ; right } -> begin
      let%bind vleft = eval left env in
      let%bind vright = eval right env in
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
      let%bind v = eval e_not_body env in
      match v with
      | VBool (b, e_b) ->
        return @@ VBool (not b, Expression.not_ e_b) 
      | _ -> type_mismatch @@ Error_msg.bad_not v
    end
    (* Branching *)
    | EIf { cond ; true_body ; false_body } -> begin
      let%bind v = eval cond env in
      match v with
      | VBool (b, e) ->
        let body = if b then true_body else false_body in
        let%bind () = modify @@ Eval_session.hit_branch (Direction.of_bool b) e in
        eval body env
      | _ -> type_mismatch @@ Error_msg.cond_non_bool v
    end
    | ECase { subject ; cases ; default } -> begin
      let%bind v = eval subject env in
      let int_cases = List.map cases ~f:Tuple2.get1 in
      match v with
      | VInt (i, e) -> begin
        let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
        match body_opt with
        | Some body -> (* found a matching case *)
          let other_cases = List.filter int_cases ~f:((<>) i) in
          let%bind () = modify @@ Eval_session.hit_case (Direction.of_int i) e ~other_cases in
          eval body env
        | None -> (* no matching case, so take default case *)
          let%bind () = modify @@ Eval_session.hit_case (Direction.Case_default { not_in = int_cases }) e ~other_cases:int_cases in
          eval default env
      end
      | _ -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* Inputs *)
    | EPick_i ->
      let%bind s = step in
      let key = Stepkey.I s in
      modify_and_return @@ Eval_session.get_input key
    | EPick_b ->
      let%bind s = step in
      let key = Stepkey.B s in
      modify_and_return @@ Eval_session.get_input key
    (* Failure cases *)
    | EAbort -> abort
    | EDiverge -> diverge
  in

  run (eval expr Env.empty) session

(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple interpretations, trying to hit the branches.
*)

module Make (S : Solve.S) (P : Pause.S) (O : Options.V) = struct
  module Intra = Intra_session.Make (S) (P) (O)

  let rec loop (e : Embedded.t) (main_session : Intra.t) (session : Eval_session.t) : Status.Terminal.t P.t =
    let open P in
    let* () = pause () in
    let res = eval_exp ~session e in
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
