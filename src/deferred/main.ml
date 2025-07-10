
open Core
open Effects

module E = Lang.Ast.Embedded

(* Eval may error (monadically) *)
let rec eval (expr : E.t) : Value.t m =
  let open Value in
  match expr with
  | EUnit -> return VUnit
  | EInt i -> return (VInt i)
  | EBool b -> return (VBool b)
  | EVar id -> fetch id
  | EId -> return VId
  (* inputs *)
  | EPick_i () ->
    let%bind () = assert_nondeterminism in
    let%bind () = incr_time in
    let%bind s = get in
    let%bind x = get_input (Utils.Separate.I s.time) in 
    let%bind () = incr_time in
    return x
  | EPick_b () ->
    let%bind () = assert_nondeterminism in
    let%bind () = incr_time in
    let%bind s = get in
    let%bind x = get_input (Utils.Separate.B s.time) in
    let%bind () = incr_time in
    return x
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
      (* no natural way to increment time _after_ the error  *)
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
      let body = if b then true_body else false_body in
      eval body
    | v -> type_mismatch @@ Error_msg.cond_non_bool v
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind stern_eval subject with
    | VInt i -> begin
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
  (* deferal *)
  | EDefer body ->
    let%bind e = read_env in
    let%bind () = incr_time in
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
  | ETable
  | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"

(*
  I wish I had a contract that says this does not error.
*)
and eval_to_err (expr : E.t) : (Value.t, Err.t) result m =
  handle_error 
    (eval expr)
    (fun v -> return (Ok v))
    (fun e -> return (Error e))

(*
  TODO: need to be able to stern eval with an error as the expression.
    Obviously the math allows this, but I don't want to allow values as
    expressions in the code, nor errors as values, so this isn't clean anymore.

  But we don't want to clean up all symbols on every stern eval because we
  use the stern eval inside the relaxed eval. So stern can't be "full".
*)
(*
  Right now, I'm saying this may error (monadically) so that relaxed eval
  gets error propagation.
  However, I'm thinking that maybe it should not error because it doesn't
  have error propagation in itself (per the rules) but instead catches them.
*)
and stern_eval (expr : E.t) : Value.whnf m = 
  let open Value in
  (* We can handle the value rules by relaxed eval first *)
  match%bind eval_to_err expr with
  | Error e -> begin
    match e with
    | `XAbort { msg = _ ; body = t }
    | `XType_mismatch { msg = _ ; body = t }
    | `XVanish t
    | `XUnbound_variable (_, t) ->
      let%bind () = remove_greater_symbols (VSymbol t) in
      fail e (* not sure what to do here. rules say this is fine, techincally *)
      (* May just continue to eval on error here. Not sure. Rules do say to do that though *)
    | `XReach_max_step _ -> failwith "Unhandled reach max step in stern eval"
  end
  | Ok v -> begin
    Value.split v
      ~symb:(fun ((VSymbol t) as sym) ->
        let%bind s = get in
        match Time_map.find_opt t s.symbol_env with
        | Some v -> return v
        | None -> 
          (* evaluate the deferred proof for this symbol *)
          (* if this fails, the greater symbols get removed, and this error propagates *)
          let%bind v = run_on_deferred_proof sym stern_eval in
          (* update the symbol environment to contain the result  *)
          let%bind () = modify (fun s -> { s with symbol_env = Time_map.add t v s.symbol_env }) in
          return v
      )
      ~whnf:return (* may optionally choose to work on deferred proofs here *)
  end

and stern_eval_to_err (expr : E.t) : (Value.whnf, Err.t) result m =
  handle_error 
    (stern_eval expr)
    (fun v -> return (Ok v))
    (fun e -> return (Error e))

(*
  The general idea is we need to stern eval until there are no more proofs to do.

  Stern eval only turns it into whnf, but we need a full on value, not just one
  that appears to be a value at first glance.

  So what we can do is after a stern eval of the main thing, we can just chug along
  on all deferred proofs in the same way, throwing away everything (because if its
  value is needed, then it will be put in the store) and returning the main whnf
  value. In the end, if we're being good, we can substitute all of the values through
  for the symbols.

  I'm currently not handling errors properly. They propagate when they shouldn't.
*)

type t3 =
  | Expr of E.t
  | Value of Value.whnf
  | Err of Err.t

let rec loop (t : t3) : Value.whnf m =
  match t with
  | Expr expr -> begin
    (* Working with an expression. Evaluate it sternly, and loop *)
    match%bind stern_eval_to_err expr with
    | Ok v -> loop (Value v)
    | Error e -> loop (Err e)
  end
  | Value v -> clean_up_deferred (return v)
  | Err e -> clean_up_deferred (fail e)

and clean_up_deferred (finish : Value.whnf m) : Value.whnf m =
  let%bind s = get in
  match Time_map.choose_opt s.pending_proofs with
  | None -> finish
  | Some (t, _) ->
    (* Do some cleanup by running this timestamp *)
    match%bind run_on_deferred_proof (VSymbol t) stern_eval_to_err with
    | Ok v' ->
      (* deferred proof evaluated. Put it into the map, and continue on looping *)
      let%bind () = modify (fun s -> { s with symbol_env = Time_map.add t v' s.symbol_env }) in
      clean_up_deferred finish
    | Error e -> clean_up_deferred (fail e)

(* TODO: need to differentiate between vanish and other errors *)
let[@landmark] deval 
  ?(feeder : Interp_common.Input_feeder.Using_timekey.t = Interp_common.Input_feeder.Using_timekey.zero) 
  (expr : E.t) 
  : (Value.Without_symbols.t, Err.t) result
  =
  match run_on_empty (loop (Expr expr)) feeder with
  | Ok v, s -> Ok (Symbol_map.close_value (Value.cast_up v) s.symbol_env)
  | Error e, _ -> Error e
