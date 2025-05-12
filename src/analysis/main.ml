
open Core
open Lang.Ast
open Grammar
open M
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

  let bad_appl vfunc =
    Format.sprintf "Apply to non-function %s" (Value.to_string vfunc)

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

  let appl_non_table v =
    Format.sprintf "Use non-table `%s` as a table" (Value.to_string v)
end

let type_mismatch s = 
  M.fail @@ Err.type_mismatch s

let[@landmark] rec analyze (e : Embedded.With_callsights.t) : Value.t m =
  log e @@
  match e with
  (* immediate *)
  | EInt 0 -> return VZero
  | EInt i when i > 0 -> return VPosInt
  | EInt _ -> return VNegInt
  | EBool true -> return VTrue
  | EBool false -> return VFalse
  | EVar id -> bind ask_env (Env.find id)
  | EId -> return VId
  (* inputs *)
  | EPick_i -> any_int
  | EPick_b -> any_bool
  (* operations *)
  | EBinop { left ; binop ; right } -> begin
    let%bind v1 = analyze left in
    let%bind v2 = analyze right in
    op v1 binop v2
  end
  | ENot expr -> bind (analyze expr) not_
  (* propagation *)
  | EMatch { subject ; patterns } -> begin
    let%bind v = analyze subject in
    List.find_map patterns ~f:(fun (pat, expr) ->
      match pat, v with
      | PAny, _ -> Some (expr, fun env -> env)
      | PVariable id, _ -> Some (expr, Env.add id v)
      | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
        when Lang.Ast.VariantLabel.equal variant_label label ->
          Some (expr, Env.add payload_id payload)
      | _ -> None
    )
    |> function
      | Some (expr, f) -> local f (analyze expr)
      | None -> type_mismatch @@ Error_msg.pattern_not_found patterns v
  end
  | ERecord record_body ->
    let%bind value_record_body =
      Map.fold record_body ~init:(return RecordLabel.Map.empty) ~f:(fun ~key ~data:e acc_m ->
        let%bind acc = acc_m in
        let%bind v = analyze e in
        return @@ Map.set acc ~key ~data:v
      )
    in
    return @@ VRecord value_record_body
  | EProject { record ; label } -> begin
    match%bind analyze record with
    | VRecord record_body as r -> begin
      match Map.find record_body label with
      | Some v -> return v
      | None -> type_mismatch @@ Error_msg.project_missing_label label r
    end 
    | r -> type_mismatch @@ Error_msg.project_non_record label r
  end
  | EVariant { label ; payload } ->
    let%bind v = analyze payload in
    return @@ VVariant { label ; payload = v }
  (* branching *)
  | EIf { cond ; true_body ; false_body } -> begin
    match%bind analyze cond with
    | VTrue -> analyze true_body
    | VFalse -> analyze false_body
    | v -> type_mismatch @@ Error_msg.cond_non_bool v
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind analyze subject with
    | VNegInt -> vanish (* not ill-typed. Just diverge because there are no negative cases *)
    | VZero -> analyze default
    | VPosInt -> (* relying on the assumption that cases are on positive ints *)
      let%bind (_, expr) = choose cases in
      analyze expr
    | v -> type_mismatch @@ Error_msg.case_non_int v
  end
  (* closures and applications *)
  | EFunction { param ; body } ->
    let%bind env, callstack = ask in
    let%bind () = modify (Store.add callstack env) in
    return @@ VFunClosure { param ; body = { body ; callstack }}
  | EFreeze expr ->
    let%bind env, callstack = ask in
    let%bind () = modify (Store.add callstack env) in
    return @@ VFrozen { body = expr ; callstack }
  | ELet { var ; body ; cont } ->
    let%bind v = analyze body in
    local (Env.add var v) (analyze cont)
  | EIgnore { ignored ; cont } ->
    let%bind _ : Value.t = analyze ignored in
    analyze cont
  | EAppl { appl = { func ; arg } ; callsight } -> begin
    match%bind analyze func with
    | VFunClosure { param ; body = { body ; callstack } } -> begin
      let%bind v = analyze arg in
      let%bind env = find_env callstack in
      with_call callsight
      @@ local (fun _ -> Env.add param v env) (analyze body)
    end
    | VId -> analyze arg
    | v -> type_mismatch @@ Error_msg.bad_appl v 
  end
  | EThaw { appl = expr ; callsight } -> begin
    match%bind analyze expr with
    | VFrozen { body ; callstack } -> begin
      let%bind env = find_env callstack in
      with_call callsight
      @@ local (fun _ -> env) (analyze body)
    end
    | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
  end
  (* termination *)
  | EDiverge -> vanish
  | EAbort msg -> fail @@ Err.abort msg
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> analyze expr
  (* unhandled and currently aborting *)
  | ETable
  | ETblAppl _ -> failwith "unimplemented analysis on tables"
