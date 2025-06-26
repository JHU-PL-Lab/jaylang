
open Core

let unimplemented () = 
  failwith "unimplemented"

(*
  Still haven't...
    - set up an executable
*)

open Effects

let rec eval (expr : Lang.Ast.Embedded.With_program_points.t) : Value.t m =
  let open Value in
  match expr with
  | EUnit -> return VUnit
  | EInt i -> return (VInt i)
  | EBool b -> return (VBool b)
  | EVar id -> begin
    match%bind fetch id with
    | Some v -> return v
    | None -> unbound_variable id
  end
  | EId -> return VId
  (* inputs *)
  | EPick_i { data = () ; point } -> get_input point
  | EPick_b { data = () ; point = _ } -> unimplemented () (* TODO: bool inputs *)
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
    | _ -> type_mismatch "bad binop; no good error message yet"
  end
  | ENot expr -> begin
    match%bind stern_eval expr with
    | VBool b -> return (VBool (not b))
    | _v -> type_mismatch "bad not"
  end
  | EProject { record ; label } -> begin
    match%bind stern_eval record with
    | (VRecord body | VModule body) -> begin
      match Map.find body label with
      | Some v -> return (Value.cast_up v)
      | None -> type_mismatch "missing label in projection"
    end
    | _v -> type_mismatch "project from non record/module"
  end
  (* control flow / branches *)
  | EMatch { subject  ; patterns  } -> begin
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
    | None -> type_mismatch "missing pattern"
  end
  | EIf { cond ; true_body ; false_body } -> begin
    match%bind stern_eval cond with
    | VBool b ->
      let body = if b then true_body else false_body in
      eval body
    | _v -> type_mismatch "cond on non bool"
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind stern_eval subject with
    | VInt i -> begin
      let body_opt = List.find_map cases ~f:(fun (i', body) -> if i = i' then Some body else None) in
      match body_opt with
      | Some body -> eval body
      | None -> eval default
    end
    | _v -> type_mismatch "non int case" (* this should be impossible *)
  end
  (* closures and applications *)
  | EFunction { param  ; body } ->
    let%bind env = read_env in
    return (VFunClosure { param ; closure = { body ; env }})
  | EFreeze body ->
    let%bind env = read_env in
    return (VFrozen { body ; env })
  | ELet { var ; defn ; body } ->
    let%bind v = eval defn in
    local_env (Env.add var v) (eval body)
  | EIgnore { ignored ; body } ->
    let%bind _ : Value.t = eval ignored in
    eval body
  | EAppl { data = { func ; arg } ; point } -> begin
    match%bind stern_eval func with
    | VFunClosure { param ; closure } ->
      let%bind v = eval arg in 
      with_program_point point (
        local_env (fun _ -> Env.add param v closure.env) (eval closure.body)
      )
    | _v -> type_mismatch "non-func application"
  end
  | EThaw { data = body ; point } ->
    with_program_point point (eval body)
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
  (* termination *)
  | EDiverge { data = () ; point } -> diverge point
  | EAbort { data = msg ; point } -> abort msg point
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> eval expr (* it is fine to ignore these and just eval what's inside for now *)
  (* unhandled and currently aborting -- okay to ignore for now because these are uncommon *)
  | EIntensionalEqual _ -> failwith "unhandled intensional equality in deferred evaluation"
  | ETable
  | ETblAppl _ -> failwith "unhandled table operations in deferred evaluation"

and eval_to_err (expr : Lang.Ast.Embedded.With_program_points.t) : (Value.t, Value.Err.t) result m =
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

  I'm also noticing that at some point we'll want to substitute the values in
  for all the symbols when we return a final value. We don't make the necessary
  substitutions ever in the current rules.
*)
and stern_eval (expr : Lang.Ast.Embedded.With_program_points.t) : Value.whnf m = 
  let open Value in
  (* We can handle the value rules by relaxed eval first *)
  match%bind eval_to_err expr with
  | Error _e ->
    failwith "do error semantics here"
  | Ok v -> begin
    Value.split v
      ~symb:(fun ((VSymbol t) as sym) ->
        let%bind s = get in
        match Timestamp.Map.find_opt t s.symbol_env with
        | Some v -> return v (* TODO: don't just return, but apply proof rule *)
        | None -> 
          (* implicitly, the symbol must be for a deferred proof *)
          run_on_deferred_proof sym stern_eval
      )
      ~whnf:return (* TODO: don't just return, but apply proof rule *)
  end
