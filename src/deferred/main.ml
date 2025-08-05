
open Core
open Effects

module E = Lang.Ast.Embedded

let max_step = Interp_common.Step.Step Int.(10 ** 6)

(* Eval may error (monadically) *)
let rec eval (expr : E.t) : Value.t m =
  let open Value in
  let%bind () = incr_step ~max_step in
  match expr with
  | EUnit -> return VUnit
  | EInt i -> return (VInt i)
  | EBool b -> return (VBool b)
  | EVar id -> fetch id
  | EId -> return VId
  (* inputs *)
  | EPick_i -> get_input Interp_common.Key.Timekey.int_
  | EPick_b -> get_input Interp_common.Key.Timekey.bool_
  (* operations *)
  | EBinop { left ; binop ; right } -> begin
    let%bind a = stern_eval left in
    let%bind b = stern_eval right in
    match binop, a, b with
    | BPlus        , VInt n1  , VInt n2              -> return (VInt (n1 + n2))
    | BMinus       , VInt n1  , VInt n2              -> return (VInt (n1 - n2))
    | BTimes       , VInt n1  , VInt n2              -> return (VInt (n1 * n2))
    | BDivide      , VInt n1  , VInt n2 when n2 <> 0 -> return (VInt (n1 / n2))
    | BModulus     , VInt n1  , VInt n2 when n2 <> 0 -> return (VInt (n1 mod n2))
    | BEqual       , VInt n1  , VInt n2              -> return (VBool (n1 = n2))
    | BEqual       , VBool b1 , VBool b2             -> return (VBool Bool.(b1 = b2))
    | BNeq         , VInt n1  , VInt n2              -> return (VBool (n1 <> n2))
    | BLessThan    , VInt n1  , VInt n2              -> return (VBool (n1 < n2))
    | BLeq         , VInt n1  , VInt n2              -> return (VBool (n1 <= n2))
    | BGreaterThan , VInt n1  , VInt n2              -> return (VBool (n1 > n2))
    | BGeq         , VInt n1  , VInt n2              -> return (VBool (n1 >= n2))
    | BAnd         , VBool b1 , VBool b2             -> return (VBool (b1 && b2))
    | BOr          , VBool b1 , VBool b2             -> return (VBool (b1 || b2))
    | _ -> type_mismatch @@ Error_msg.bad_binop a binop b
  end
  | ENot expr -> begin
    match%bind stern_eval expr with
    | VBool b -> return (VBool (not b))
    | v -> type_mismatch @@ Error_msg.bad_not v
  end
  | EProject { record ; label } -> begin
    match%bind stern_eval record with
    | (VRecord body | VModule body) as v -> begin
      match Map.find body label with
      | Some v -> return (Value.cast_up v)
      | None -> type_mismatch @@ Error_msg.project_missing_label label v
    end
    | v -> type_mismatch @@ Error_msg.project_non_record label v
  end
  (* control flow / branches *)
  | EMatch { subject ; patterns  } -> begin
    let%bind v = stern_eval subject in
    match
      List.find_map patterns ~f:(fun (pat, body) ->
        match Value.matches v pat with
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
    | VBool b ->
      let%bind () = incr_time in
      let body = if b then true_body else false_body in
      eval body
    | v -> type_mismatch @@ Error_msg.cond_non_bool v
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind stern_eval subject with
    | VInt i -> begin
      let%bind () = incr_time in
      let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
      match body_opt with
      | Some body -> eval body
      | None -> eval default
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
    let%bind _ : Value.t = eval ignored in
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
      let rec fold_stmts acc_m : E.statement list -> Value.t Lang.Ast.RecordLabel.Map.t m = function
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
  | EDet expr -> with_incr_depth (eval expr)
  | EEscapeDet expr -> with_escaped_det (eval expr)
  (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
  | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
  | ETableCreate
  | ETableAppl _ -> failwith "unhandled table operations in deferred evaluation"

(*
  This stern eval may error monadically so that we get propagation of
  errors in relaxed eval.

  We have this because in practice, we don't want to clean up many deferred
  proofs on stern evals. This one is very direct, and it does (most of the time)
  the minimal amount of work to get to whnf.
*)
and stern_eval (expr : E.t) : Value.whnf m = 
  let%bind v = eval expr in
  let%bind () = incr_step ~max_step in
  Value.split v
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
let rec clean_up_deferred (final : Value.ok Res.t) : Value.ok Res.t s =
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
        | (`XReach_max_step _) as e ->
          (* ran out of steps running deferred proof. Give up totally and say so. *)
          return (Res.E e)
        | e ->
          (* deferred proof errored. This means the final value should be overwritten with this new error *)
          clean_up_deferred (Res.E e))

let begin_stern_loop (expr : E.t) : Value.ok Res.t s =
  let%bind r =
    handle_error (stern_eval expr)
      (fun v -> return (Res.V v))
      (fun e -> return (Res.E e))
  in
  clean_up_deferred r

(* TODO: should probably differentiate between vanish and other errors *)
let deval 
  ?(feeder : Interp_common.Timestamp.t Interp_common.Input_feeder.t = Interp_common.Input_feeder.zero) 
  (pgm : Lang.Ast.Embedded.pgm) 
  : (Value.Without_symbols.t, Err.t) result
  =
  let expr = Lang.Ast_tools.Utils.pgm_to_module pgm in
  match run_on_empty (begin_stern_loop expr) feeder with
  | V v, state, _ -> Ok (Value.subst (Value.cast_up v) ~f:(fun t -> Time_map.find t state.symbol_env))
  | E e, _, _ -> Error e

let deval_with_input_sequence
  (inputs : Interp_common.Input.t list)
  (pgm : Lang.Ast.Embedded.pgm)
  : (Value.Without_symbols.t, Err.t) result
  =
  match inputs with
  | [] -> deval pgm
  | _ ->
    (* Capture all output from interpreter (which translates the input sequence) *)
    let oc_null = Out_channel.create "/dev/null" in
    Format.set_formatter_out_channel oc_null;
    let _, feeder = 
      Interpreter.Interp.eval_pgm_to_time_feeder 
        ~feeder:(Interp_common.Input_feeder.of_sequence inputs)
        pgm
    in
    Format.set_formatter_out_channel Out_channel.stdout;
    deval ~feeder pgm
