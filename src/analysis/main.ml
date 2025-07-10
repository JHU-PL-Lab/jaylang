
open Core
open Lang.Ast
open Grammar
open M
open Value

let type_mismatch s = 
  M.fail @@ Err.type_mismatch s

module Error_msg = Lang.Value.Error_msg (Value)

let[@landmark] rec analyze (e : Embedded.With_program_points.t) : Value.t m =
  log e @@
  match e with
  (* immediate *)
  | EUnit -> return VUnit
  | EInt 0 -> return VZero
  | EInt i when i > 0 -> return VPosInt
  | EInt _ -> return VNegInt
  | EBool true -> return VTrue
  | EBool false -> return VFalse
  | EVar id -> bind ask_env (Env.find id)
  | EId -> return VId
  (* inputs *)
  | EPick_i _ -> any_int
  | EPick_b _ -> any_bool
  (* operations *)
  | EBinop { left ; binop ; right } -> begin
    let%bind v1 = analyze left in
    let%bind v2 = analyze right in
    op v1 binop v2
  end
  | ENot expr -> bind (analyze expr) not_
  | EIntensionalEqual _ -> failwith "unimplemented"
  (* propagation *)
  | EMatch { subject ; patterns } -> begin
    let%bind v = analyze subject in
    List.find_map patterns ~f:(fun (pat, expr) ->
      match pat, v with
      | PUntouchable id, VUntouchable v -> Some (expr, Env.add id v)
      | _, VUntouchable _ -> None
      | PAny, _
      | PInt, (VPosInt | VNegInt | VZero)
      | PBool, (VTrue | VFalse)
      | PUnit, VUnit
      | PRecord, VRecord _
      | PModule, VModule _
      | PFun, VFunClosure _ -> Some (expr, fun env -> env)
      | PType, VRecord m ->
        if List.for_all Lang.Ast_tools.Reserved.[ gen ; check ; wrap ] ~f:(Map.mem m)
        then Some (expr, fun env -> env)
        else None
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
  | EModule stmt_ls ->
    let%bind module_body =
      let rec fold_stmts acc_m : Constraints.embedded Expr.With_program_points.statement list -> Value.t RecordLabel.Map.t m = function
        | [] -> acc_m
        | SUntyped { var ; defn } :: tl ->
          let%bind acc = acc_m in
          let%bind v = analyze defn in
          local (Env.add var v) (
            fold_stmts (return @@ Map.set acc ~key:(RecordLabel.RecordLabel var) ~data:v) tl
          )
      in
      fold_stmts (return RecordLabel.Map.empty) stmt_ls
    in
    return (VModule module_body)
  | EProject { record ; label } -> begin
    match%bind analyze record with
    | (VRecord body | VModule body) as r -> begin
      match Map.find body label with
      | Some v -> return v
      | None -> type_mismatch @@ Error_msg.project_missing_label label r
    end 
    | r -> type_mismatch @@ Error_msg.project_non_record label r
  end
  | EVariant { label ; payload } ->
    let%bind v = analyze payload in
    return @@ VVariant { label ; payload = v }
  | EUntouchable e ->
    let%bind v = analyze e in
    return @@ VUntouchable v
  (* branching *)
  | EIf { cond ; true_body ; false_body } -> begin
    match%bind analyze cond with
    | VTrue -> analyze true_body
    | VFalse -> analyze false_body
    | v -> type_mismatch @@ Error_msg.cond_non_bool v
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind analyze subject with
    | VNegInt -> disappear (* not ill-typed. Just vanish because there are no negative cases *)
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
  | ELet { var ; defn ; body } ->
    let%bind v = analyze defn in
    local (Env.add var v) (analyze body)
  | EIgnore { ignored ; body } ->
    let%bind _ : Value.t = analyze ignored in
    analyze body
  | EAppl { data = { func ; arg } ; point } -> begin
    match%bind analyze func with
    | VFunClosure { param ; body = { body ; callstack } } -> begin
      let%bind v = analyze arg in
      let%bind env = find_env callstack in
      with_call point
      @@ local (fun _ -> Env.add param v env) (analyze body)
    end
    | VId -> analyze arg
    | v -> type_mismatch @@ Error_msg.bad_appl v 
  end
  | EThaw { data = expr ; point } -> begin
    match%bind analyze expr with
    | VFrozen { body ; callstack } -> begin
      let%bind env = find_env callstack in
      with_call point
      @@ local (fun _ -> env) (analyze body)
    end
    | v -> type_mismatch @@ Error_msg.thaw_non_frozen v
  end
  (* termination *)
  | EVanish _ -> disappear
  | EAbort { data = msg ; point = _ } -> fail @@ Err.abort msg
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> analyze expr
  (* unhandled and currently aborting *)
  | ETable
  | ETblAppl _ -> failwith "unimplemented analysis on tables"
