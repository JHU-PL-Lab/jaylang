
(* Concolic main *)

open Core
open Lang.Ast
open Concolic_common

module V = Ceffects.V

(* this should be abstracted *)
type 'a res =  
  | V of 'a V.v
  | E of Ceffects.Err.t

let res_to_err (type a) (x : a res Ceffects.s) : a V.v Ceffects.m =
  Ceffects.bind (Ceffects.make_unsafe x) (function
    | V v -> Ceffects.return v
    | E e -> Ceffects.fail e
  )

let eval_exp : Interp_common.Timestamp.t Concolic.Evaluator.eval =
  fun expr input_feeder ~max_step ->
  let open Ceffects in

  let rec eval (expr : Embedded.t) : V.t m =
    let open V in
    let%bind () = incr_step ~max_step in
    match expr with
    | EUnit -> return VUnit
    | EInt i -> return @@ VInt (i, Overlays.Typed_smt.const_int i)
    | EBool b -> return @@ VBool (b, Overlays.Typed_smt.const_bool b)
    | EVar id -> fetch id
    | EId -> return VId
    (* inputs *)
    | EPick_i () -> get_input Interp_common.Key.Timekey.int_ input_feeder
    | EPick_b () -> get_input Interp_common.Key.Timekey.bool_ input_feeder
    (* operations *)
    | EBinop { left ; binop ; right } -> begin
      let%bind vleft = stern_eval left in
      let%bind vright = stern_eval right in
      let k f e1 e2 op =
        return @@ f (Overlays.Typed_smt.binop op e1 e2)
      in
      let open Overlays.Typed_smt.Binop in
      let v_int n = fun e -> VInt (n, e) in
      let v_bool b = fun e -> VBool (b, e) in
      match binop, vleft, vright with
      | BPlus        , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 + n2)) e1 e2 Plus
      | BMinus       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 - n2)) e1 e2 Minus
      | BTimes       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_int (n1 * n2)) e1 e2 Times
      | BDivide      , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 / n2)) e1 e2 Divide
      | BModulus     , VInt (n1, e1)  , VInt (n2, e2) when n2 <> 0 -> k (v_int (n1 mod n2)) e1 e2 Modulus
      | BEqual       , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 = n2)) e1 e2 Equal
      | BEqual       , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool Bool.(b1 = b2)) e1 e2 Equal
      | BNeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <> n2)) e1 e2 Not_equal
      | BLessThan    , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 < n2)) e1 e2 Less_than
      | BLeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 <= n2)) e1 e2 Less_than_eq
      | BGreaterThan , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 > n2)) e1 e2 Greater_than
      | BGeq         , VInt (n1, e1)  , VInt (n2, e2)              -> k (v_bool (n1 >= n2)) e1 e2 Greater_than_eq
      | BOr          , VBool (b1, e1) , VBool (b2, e2)             -> k (v_bool (b1 || b2)) e1 e2 Or
      | BAnd         , VBool (b1, e1) , VBool (b2, e2)             -> return @@ VBool (b1 && b2, Overlays.Typed_smt.and_ [ e1 ; e2 ])
      | _ -> type_mismatch @@ Error_msg.bad_binop vleft binop vright
    end
    | ENot expr -> begin
      match%bind stern_eval expr with
      | VBool (b, e_b) -> return @@ VBool (not b, Overlays.Typed_smt.not_ e_b)
      | v -> type_mismatch @@ Error_msg.bad_not v
    end
    | EProject { record ; label } -> begin
      match%bind stern_eval record with
      | (VRecord body | VModule body) as v -> begin
        match Map.find body label with
        | Some v -> return (V.cast_up v)
        | None -> type_mismatch @@ Error_msg.project_missing_label label v
      end
      | v -> type_mismatch @@ Error_msg.project_non_record label v
    end
    (* control flow / branches *)
    | EMatch { subject ; patterns  } -> begin
      let%bind v = stern_eval subject in
      match
        List.find_map patterns ~f:(fun (pat, body) ->
          match V.matches v pat with
          | `Matches -> Some (body, fun x -> x)
          | `Matches_with (v', id) -> Some (body, Env.add id v')
          | `No_match -> None
        )
      with
      | Some (e, f) -> local_env f (eval e)
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
    end
    | EIf { cond ; true_body ; false_body } -> begin
      match%bind stern_eval cond with
      | VBool (b, e_b) ->
        let%bind () = incr_time in
        let body = if b then true_body else false_body in
        let%bind () = push_branch (Direction.Bool_direction (b, e_b)) in
        eval body
      | v -> type_mismatch @@ Error_msg.cond_non_bool v
    end
    | ECase { subject ; cases ; default } -> begin
      let int_cases = List.map cases ~f:Tuple2.get1 in
      match%bind stern_eval subject with
      | VInt (i, e_i) -> begin
        let%bind () = incr_time in
        let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
        match body_opt with
        | Some body -> 
          let not_in = List.filter int_cases ~f:((<>) i) in
          let%bind () = push_branch (Direction.Int_direction { dir = Case_int i ; expr = e_i ; not_in }) in
          eval body
        | None -> 
          let%bind () = push_branch (Direction.Int_direction { dir = Case_default ; expr = e_i ; not_in = int_cases }) in
          eval default
      end
      | v -> type_mismatch @@ Error_msg.case_non_int v
    end
    (* closures and applications *)
    | EFunction { param  ; body } ->
      let%bind { env ; _ } = read_env in
      return (VFunClosure { param ; closure = { body ; env }})
    | EFreeze body ->
      let%bind { env ; _ } = read_env in
      return (VFrozen { body ; env })
    | ELet { var ; defn ; body } ->
      let%bind v = eval defn in
      local_env (Env.add var v) (eval body)
    | EIgnore { ignored ; body } ->
      let%bind _ : V.t = eval ignored in
      eval body
    | EAppl { func ; arg } -> begin
      match%bind stern_eval func with
      | VId -> eval arg
      | VFunClosure { param ; closure } ->
        let%bind v = eval arg in 
        local_env (fun _ -> Env.add param v closure.env) (eval closure.body)
      | v -> type_mismatch @@ Error_msg.bad_appl v
    end
    | EThaw expr -> begin
      match%bind stern_eval expr with
      | VFrozen closure ->
        local_env (fun _ -> closure.env) (eval closure.body)
      | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
    end
    (* modules, records, and variants  *)
    | ERecord label_map ->
      let%bind value_record_body =
        Map.fold label_map ~init:(return Lang.Ast.RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
          let%bind acc = acc_m in
          let%bind v = eval e in
          return @@ Map.set acc ~key ~data:v
        )
      in
      return @@ VRecord value_record_body
    | EVariant { label ; payload } ->
      let%bind v = eval payload in
      return (VVariant { label ; payload = v })
    | EModule stmt_ls ->
      let%bind module_body =
        let rec fold_stmts acc_m : Embedded.statement list -> V.t Lang.Ast.RecordLabel.Map.t m = function
          | [] -> acc_m
          | SUntyped { var ; defn } :: tl ->
            let%bind acc = acc_m in
            let%bind v = eval defn in
            local_env (Env.add var v) (
              fold_stmts (return @@ Map.set acc ~key:(Lang.Ast.RecordLabel.RecordLabel var) ~data:v) tl
            )
        in
        fold_stmts (return Lang.Ast.RecordLabel.Map.empty) stmt_ls
      in
      return @@ VModule module_body 
    | EUntouchable e ->
      let%bind v = eval e in
      return (VUntouchable v)
    (* deferral *)
    | EDefer body ->
      let%bind e = read_env in
      let%bind s = get in
      let symb = VSymbol (Interp_common.Timestamp.push s.time) in
      let%bind () = push_deferred_proof symb { body ; env = e.env } in
      let%bind () = incr_time in
      return (cast_up symb)
    (* termination *)
    | EVanish () -> vanish
    | EAbort msg -> abort msg
    (* determinism stuff *)
    | EDet expr -> with_incr_depth (eval expr) (* TODO: this should need to be propagated into deferred expressions! *)
    | EEscapeDet expr -> with_escaped_det (eval expr)
    (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
    | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
    | ETable
    | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"

  (*
    This stern eval may error monadically so that we get propagation of
    errors in relaxed eval.

    We have this because in practice, we don't want to clean up many deferred
    proofs on stern evals. This one is very direct, and it does (most of the time)
    the minimal amount of work to get to whnf.
  *)
  and stern_eval (expr : Embedded.t) : V.whnf m = 
    let%bind v = eval expr in
    let%bind () = incr_step ~max_step in
    V.split v
      ~symb:(fun ((VSymbol t) as sym) ->
        let%bind s = get in
        match Time_map.find_opt t s.symbol_env with
        | Some v -> 
          let%bind () = incr_n_stern_steps in
          return v
        | None -> 
          (* evaluate the deferred proof for this symbol *)
          (* if this fails, the greater symbols get removed, and this error propagates *)
          let%bind v = run_on_deferred_proof sym stern_eval in
          (* update the symbol environment to contain the result  *)
          let%bind () = modify (fun s -> { s with symbol_env = Time_map.add t v s.symbol_env }) in
          return v
      )
      ~whnf:(fun v ->
        (* optionally choose to work on a deferred proof here *)
        let%bind () = incr_n_stern_steps in
        let%bind s = get in
        let%bind b = should_work_on_deferred in
        if b && not (Time_map.is_empty s.pending_proofs) then
          let (t, _) = Time_map.choose s.pending_proofs in
          let%bind v' = run_on_deferred_proof (VSymbol t) stern_eval in
          let%bind () = modify (fun s -> { s with symbol_env = Time_map.add t v' s.symbol_env }) in
          return v
        else 
          (* chose not to work on a deferred proof, or there are no proofs to work on *)
          return v
      )

  (* 
    This does not monadically error.
    Any error is packed into Res, so there is no implicit error propagation.
    This is helpful because we don't want errors propagating and messing with
    our stern semantics.
  *)
  and clean_up_deferred (final : V.ok res) : V.ok res s =
    let%bind s = get in
    match Time_map.choose_opt s.pending_proofs with
    | None -> return final (* done! can finish with how we're told to finish *)
    | Some (t, _) -> (* some cleanup to do, so do it, and then keep looping after that *)
      (* Do some cleanup by running this timestamp *)
      handle_error (run_on_deferred_proof (VSymbol t) stern_eval)
        (fun v' ->
          (* deferred proof evaluated. Put it into the map, and continue on looping. It doesn't affect the final value *)
          let%bind () = modify (fun s -> { s with symbol_env = Time_map.add t v' s.symbol_env }) in
          clean_up_deferred final)
        (function 
          (* | (Concolic.Status.Reach_max_step _) as e -> (* LOUD FIXME: need a status for reach max step, but each step will just re-throw, so this is result-like error propagation currently *)
            (* ran out of steps running deferred proof. Give up totally and say so. *)
            return (Res.E e) *)
          | e ->
            (* deferred proof errored. This means the final value should be overwritten with this new error *)
            clean_up_deferred (E e))

  and begin_stern_loop (expr : Embedded.t) : V.ok res s =
    let%bind r =
      handle_error (stern_eval expr)
        (fun v -> return (V v))
        (fun e -> return (E e))
    in
    clean_up_deferred r
  
  in

  run (res_to_err (begin_stern_loop expr))

module TQ = Concolic.Target_queue.Make (Interp_common.Timestamp)
module F = Concolic.Evaluator.Make (Interp_common.Timestamp) (TQ.BFS) (Overlays.Typed_smt.Make_Z3 ()) (Concolic.Pause.Lwt)

let lwt_eval : (Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r e ->
    let module E = F (struct let r = r end) in
    E.eval e eval_exp

(*
  TODO: the following should be done using Concolic.Driver
*)
open Options.Arrow

let test_with_timeout : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Arrow.t =
  lwt_eval
  >>^ fun res_status ->
    try Lwt_main.run res_status with
    | Lwt_unix.Timeout -> Timeout

let test_bjy =
  Options.Arrow.make
  @@ fun r -> fun bjy -> fun ~do_wrap ~do_type_splay ->
    let expr = Translate.Convert.bjy_to_emb bjy ~do_wrap ~do_type_splay |> Lang.Ast_tools.Utils.pgm_to_module in
    let res = appl test_with_timeout r expr in
    Format.printf "%s\n" (Status.to_loud_string res);
    res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test =
  (fun s -> Lang.Parse.parse_single_pgm_string @@ In_channel.read_all s)
  ^>> test_bjy

(*
  ------------------------------
  TESTING FROM COMMAND LINE ARGS
  ------------------------------
*)

let cdeval =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cdeval") @@
  let+ concolic_args = Options.cmd_arg_term
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ bjy_pgm = Lang.Parse.parse_bjy_file_from_argv in
  Options.Arrow.appl
    test_bjy
    concolic_args
    bjy_pgm
    ~do_wrap
    ~do_type_splay