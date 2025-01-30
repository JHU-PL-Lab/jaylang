
open Core
open Lang
open Ast
open Expr

open Options.Fun.Infix

open Value

module C_result = struct
  (* a state for the step and session would be really nice, but it is too slow *)
  type 'a t = { v : 'a ; step : int ; session : Symbolic_session.t }

  let get_session ({ session ; _ } : 'a t) : Symbolic_session.t =
    session
end

module CPS_Result_M = struct
  module Err = struct
    type t = Abort | Diverge | Type_mismatch | Reach_max_step
  end

  module C = Monadlib.Continuation.Make (struct type r = Symbolic_session.t end)
  (* module C = struct
    type 'a m = 'a
    let bind x f = f x
    let return x = x
  end *)

  module T = struct
    type 'a m = ('a, Err.t * Symbolic_session.t) result C.m

    let[@inline_always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m = 
      C.bind x (function
        | Ok r -> f r
        | Error e -> C.return (Error e)
      )

    let[@inline_always] return (a : 'a) : 'a m =
      C.return
      @@ Result.return a
  end

  (* include Monadlib.Monad.Make (T) *) (* will replace the line below with this when I need any helpers *)
  include T

  let fail (e : Err.t * Symbolic_session.t) : 'a m =
    C.return
    @@ Result.fail e

  let abort (session : Symbolic_session.t) : 'a m =
    fail (Err.Abort, Symbolic_session.abort session)

  let diverge (session : Symbolic_session.t) : 'a m =
    fail (Err.Diverge, Symbolic_session.diverge session)

  let type_mismatch (session : Symbolic_session.t) (_reason : string) : 'a m =
    fail (Err.Type_mismatch, Symbolic_session.type_mismatch session)

  let reach_max_step (session : Symbolic_session.t) : 'a m =
    fail (Err.Reach_max_step, Symbolic_session.reach_max_step session)
end

(*
  The only time we really update the session is on max step or hitting a branch.
  Can I then optimize it to not have to pass it around most of the time?
*)
let eval_exp 
  ~(session : Symbolic_session.t)
  (expr : Embedded.t) 
  : Symbolic_session.t
  =
  let open CPS_Result_M in
  let max_step = Symbolic_session.get_max_step session in

  let rec eval 
    ~(session : Symbolic_session.t)
    ~(step : int)
    (expr : Embedded.t) 
    (env : Env.t) 
    : Value.t C_result.t m 
    =
    let step = step + 1 in

    if step >= max_step
    then reach_max_step session
    else
      let next ?(step : int = step) ?(session : Symbolic_session.t = session) v =
        return C_result.{ v ; step ; session }
      in

      match expr with
      (* Ints and bools -- constant expressions *)
      | EInt i -> 
        next @@ VInt (i, Expression.const_int i)
      | EBool b -> 
        next @@ VBool (b, Expression.const_bool b)
      (* Simple -- no different than interpreter *)
      | EVar id -> 
        next @@ Env.fetch env id
      | EFunction { param ; body } ->
        next @@ VFunClosure { param ; body = { expr = body ; env } }
      | EId -> 
        next VId
      | EFreeze e_freeze_body -> 
        next @@ VFrozen { expr = e_freeze_body ; env }
      | EVariant { label ; payload = e_payload } -> 
        let%bind { v = payload ; step ; session } = eval ~session ~step e_payload env in
        next ~step ~session @@ VVariant { label ; payload }
      | EProject { record = e_record ; label } -> begin
        let%bind { v ; step ; session } = eval ~session ~step e_record env in
        match v with
        | VRecord record_body -> begin
          match Map.find record_body label with
          | Some v -> next ~step ~session v
          | None -> type_mismatch session "label not found in record"
        end
        | _ -> type_mismatch session "project non record"
      end
      | EThaw e_frozen -> begin
        let%bind { v ; step ; session } = eval ~session ~step e_frozen env in
        match v with
        | VFrozen { expr ; env } -> eval ~step ~session expr env
        | _ -> type_mismatch session "thaw non frozen"
      end
      | ERecord record_body ->
        let%bind C_result.{ v = value_record_body ; step ; session } = 
          Map.fold record_body ~init:(next RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
            let%bind { v = acc ; step ; session } = acc_m in
            let%bind { v ; step ; session } = eval ~step ~session e env in
            next ~step ~session @@ Map.set acc ~key ~data:v
          )
        in
        next ~session ~step @@ VRecord value_record_body
      | EIgnore { ignored ; cont } ->
        let%bind { v = _ ; step ; session } = eval ~step ~session ignored env in
        eval ~step ~session cont env
      | EMatch { subject ; patterns } -> begin (* Note: there cannot be symbolic branching on match *)
        let%bind { v ; step ; session } = eval ~step ~session subject env in
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
        | Some (e, env) -> eval ~step ~session e env
        | None -> type_mismatch session (Format.sprintf "expression not in pattern list\n")
      end
      | ELet { var ; body ; cont } ->
        let%bind { v ; step ; session } = eval ~step ~session body env in
        eval ~step ~session cont (Env.add env var v)
      | EAppl { func ; arg } -> begin
        let%bind { v = vfunc ; step ; session } = eval ~step ~session func env in
        let%bind { v = varg ; step ; session } = eval ~step ~session arg env in
        match vfunc with
        | VId -> next ~step ~session varg
        | VFunClosure { param ; body } ->
          eval ~step ~session body.expr (Env.add body.env param varg)
        | _ -> type_mismatch session "apply non fun"
      end
      (* Operations -- build new expressions *)
      | EBinop { left ; binop ; right } -> begin
        let%bind { v = vleft ; step ; session } = eval ~step ~session left env in
        let%bind { v = vright ; step ; session } = eval ~step ~session right env in
        let k f e1 e2 op =
          next ~step ~session (f (Expression.op e1 e2 op))
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
        | _ -> type_mismatch session "bad binop, which may be mod or divide by 0"
      end
      | ENot e_not_body -> begin
        let%bind { v ; step ; session } = eval ~step ~session e_not_body env in
        match v with
        | VBool (b, e_b) ->
          next ~step ~session @@ VBool (not b, Expression.not_ e_b) 
        | _ -> type_mismatch session "not non bool"
      end
      (* Branching *)
      | EIf { cond ; true_body ; false_body } -> begin
        let%bind { v ; step ; session } = eval ~step ~session cond env in
        match v with
        | VBool (b, e) ->
          let body = if b then true_body else false_body in
          let new_session = Symbolic_session.hit_branch (Direction.of_bool b) e session in
          let%bind { v = res ; step ; session } = eval ~step ~session:new_session body env in
          next ~step ~session res
        | _ -> type_mismatch session "non bool condition"
      end
      | ECase { subject ; cases ; default } -> begin
        let%bind { v ; step ; session } = eval ~step ~session subject env in
        let int_cases = List.map cases ~f:Tuple2.get1 in
        match v with
        | VInt (i, e) -> begin
          let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
          match body_opt with
          | Some body -> (* found a matching case *)
            let other_cases = List.filter int_cases ~f:((<>) i) in
            let new_session = Symbolic_session.hit_case (Direction.of_int i) e ~other_cases session in
            let%bind { v = res ; step ; session } = eval ~step ~session:new_session body env in
            next ~step ~session res
          | None -> (* no matching case, so take default case *)
            let new_session = Symbolic_session.hit_case (Direction.Case_default { not_in = int_cases }) e ~other_cases:int_cases session in
            let%bind { v = res ; step ; session } = eval ~step ~session:new_session default env in
            next ~step ~session res
        end
        | _ -> type_mismatch session "non int case"
      end
      (* Inputs *)
      | EPick_i ->
        let key = Stepkey.Int_key step in
        let session, v = Symbolic_session.get_input key session in
        next ~session v
      | EPick_b ->
        let key = Stepkey.Bool_key step in
        let session, v = Symbolic_session.get_input key session in
        next ~session v
      (* Failure cases *)
      | EAbort -> abort session
      | EDiverge -> diverge session

  in

  eval ~session ~step:0 expr Value.Env.empty (function
    | Ok r -> C_result.get_session r
    | Error (_, session) -> session (* TODO: log the error message *)
    )


(*
  -------------------
  BEGIN CONCOLIC EVAL   
  -------------------

  This sections starts up and runs the concolic evaluator (see the eval_exp above)
  repeatedly to hit all the branches.

  This eval spans multiple symbolic sessions, trying to hit the branches.
*)
let rec loop (e : Embedded.t) (main_session : Session.t) (session : Symbolic_session.t) : Session.Status.t Lwt.t =
  let open Lwt.Infix in
  let%lwt () = Lwt.pause () in
  (* CLog.app (fun m -> m "\n------------------------------\nRunning interpretation (%d) ...\n\n" (Session.run_num session)); *)
  (* let t0 = Caml_unix.gettimeofday () in *)
  let resulting_symbolic = eval_exp ~session e in
  (* let t1 = Caml_unix.gettimeofday () in *)
  (* CLog.app (fun m -> m "Interpretation finished in %fs.\n\n" (t1 -. t0)); *)
  Session.next
  @@ Session.accum_symbolic main_session resulting_symbolic
  >>= begin function
    | `Done status ->
        (* CLog.app (fun m -> m "\n------------------------------\nFinishing concolic evaluation...\n\n"); *)
        (* CLog.app (fun m -> m "Ran %d interpretations.\n" (Session.run_num session)); *)
        (* CLog.app (fun m -> m "Session status: %s.\n" (Session.Status.to_string status)); *)
        Lwt.return status
    | `Next (session, symb_session) -> loop e session symb_session
    end

let lwt_eval : (Embedded.t, Session.Status.t Lwt.t) Options.Fun.a =
  (* Dj_common.Log.init { Dj_common.Global_config.default_config with log_level_concolic = Some Debug }; *)
  Session.of_options
  >>> (Options.Fun.make
  @@ fun r (session, symb_session) (e : Embedded.t) ->
      C_sudu.set_timeout (Core.Time_float.Span.of_sec r.solver_timeout_sec);
      if not r.random then C_random.reset ();
      (* CLog.app (fun m -> m "\nStarting concolic execution...\n"); *)
      Lwt_unix.with_timeout r.global_timeout_sec
      @@ fun () -> loop e session symb_session (* repeatedly evaluate program *)
  )
  |> Options.Fun.thaw