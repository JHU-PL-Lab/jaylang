
open Core
open Lang.Ast
open Expr
open Value

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

exception Failed_interp of Status.Eval.t

(*
  The only time we really update the session is on max step or hitting a branch.
  Can I then optimize it to not have to pass it around most of the time?
*)
let eval_exp 
  (session : Eval_session.t)
  (expr : Embedded.t) 
  : Status.Eval.t
  =
  let ref_sess = ref session in
  let ref_step = ref 0 in
  let max_step = Eval_session.get_max_step !ref_sess in

  let type_mismatch s = 
    raise @@ Failed_interp (Eval_session.type_mismatch !ref_sess s) in
  
  (* let open State_M in *)

  let rec eval (expr : Embedded.t) : Value.t =
    incr ref_step;
    if !ref_step > max_step 
    then raise @@ Failed_interp (Eval_session.reach_max_step !ref_sess);
    match expr with
    (* Ints and bools -- constant expressions *)
    | EInt i -> VInt (i, Expression.const_int i)
    | EBool b -> VBool (b, Expression.const_bool b)
    (* Simple -- no different than interpreter *)
    | EVar id -> Env.fetch id
    | EFunction { param ; body } ->
      let snap = Store.capture Env.store in
      VFunClosure { param ; body = { expr = body ; snap } } (* TODO: use a snapshot here *)
    | EId -> VId
    | EFreeze e_freeze_body -> 
      let snap = Store.capture Env.store in
      VFrozen { expr = e_freeze_body ; snap }
    | EVariant { label ; payload = e_payload } -> 
      let payload = eval e_payload in
       VVariant { label ; payload }
    | EProject { record = e_record ; label } -> begin
      let v = eval e_record in
      match v with
      | VRecord record_body -> begin
        match Map.find record_body label with
        | Some v -> v
        | None -> raise @@ Failed_interp (type_mismatch @@ Error_msg.project_missing_label label v)
      end
      | _ -> type_mismatch @@ Error_msg.project_non_record label v
    end
    | EThaw e_frozen -> begin
      let v = eval e_frozen in
      match v with
      | VFrozen { expr ; snap } -> 
        let snap' = Store.capture Env.store in
        Store.restore Env.store snap;
        let res = eval expr in
        Store.restore Env.store snap';
        res
      | _ -> type_mismatch @@ Error_msg.thaw_non_frozen v
    end
    | ERecord record_body ->
      let value_record_body =
        Map.fold record_body ~init:RecordLabel.Map.empty ~f:(fun ~key ~data:e acc ->
          Map.set acc ~key ~data:(eval e)
        )
      in
      VRecord value_record_body
    | EIgnore { ignored ; cont } ->
      let _ : Value.t = eval ignored in
      eval cont
    | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
      let v = eval subject in
      match
        (* find the matching pattern and add to env any values capture by the pattern *)
        List.find_map patterns ~f:(fun (pat, body) ->
          match pat, v with
          | PAny, _ -> Some body
          | PVariable id, _ -> 
            Env.add id v;
            Some body
          | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
              when VariantLabel.equal variant_label label ->
            Env.add payload_id payload;
            Some body
          | _ -> None
          )
      with
      | Some e -> eval e
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | ELet { var ; body ; cont } ->
      Env.add var (eval body);
      eval cont
    | EAppl { func ; arg } -> begin
      let vfunc = eval func in
      let varg = eval arg in
      match vfunc with
      | VId -> varg
      | VFunClosure { param ; body } ->
        let snap = Store.capture Env.store in
        Store.restore Env.store body.snap;
        Env.add param varg;
        let res = eval body.expr in
        Store.restore Env.store snap;
        res
      | _ -> type_mismatch @@ Error_msg.bad_appl vfunc varg
    end
    (* Operations -- build new expressions *)
    | EBinop { left ; binop ; right } -> begin
      let vleft = eval left in
      let vright = eval right in
      let k f e1 e2 op =
        f (Expression.op e1 e2 op)
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
      let v = eval e_not_body in
      match v with
      | VBool (b, e_b) -> VBool (not b, Expression.not_ e_b) 
      | _ -> type_mismatch @@ Error_msg.bad_not v
    end
    (* Branching *)
    | EIf { cond ; true_body ; false_body } -> begin
      let v = eval cond in
      match v with
      | VBool (b, e) ->
        let body = if b then true_body else false_body in
        ref_sess := Eval_session.hit_branch (Direction.of_bool b) e !ref_sess;
        eval body
      | _ -> type_mismatch @@ Error_msg.cond_non_bool v
    end
    | ECase { subject ; cases ; default } -> begin
      let v = eval subject in
      let int_cases = List.map cases ~f:Tuple2.get1 in
      match v with
      | VInt (i, e) -> begin
        let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
        match body_opt with
        | Some body -> (* found a matching case *)
          let other_cases = List.filter int_cases ~f:((<>) i) in
          ref_sess := Eval_session.hit_case (Direction.of_int i) e ~other_cases !ref_sess;
          eval body
        | None -> (* no matching case, so take default case *)
          ref_sess := Eval_session.hit_case (Direction.Case_default { not_in = int_cases }) e ~other_cases:int_cases !ref_sess;
          eval default
      end
      | _ -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* Inputs *)
    | EPick_i -> 
      let sess, input = Eval_session.get_input (Stepkey.I !ref_step) !ref_sess in
      ref_sess := sess;
      input
    | EPick_b ->
      let sess, input = Eval_session.get_input (Stepkey.B !ref_step) !ref_sess in
      ref_sess := sess;
      input
    (* Failure cases *)
    | EAbort -> raise @@ Failed_interp (Eval_session.abort !ref_sess)
    | EDiverge -> raise @@ Failed_interp (Eval_session.diverge !ref_sess)
  in

  try
    let _ = eval expr in
    Eval_session.finish !ref_sess
  with Failed_interp status -> status

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
