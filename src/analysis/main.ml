
open Core
open Lang.Ast
open Grammar
open M
open Value

let rec analyze (e : Embedded.With_callsights.t) : Value.t m =
  match e with
  (* immediate *)
  | EInt 0 -> return VZero
  | EInt i when i > 0 -> return VPosInt
  | EInt _ -> return VNegInt
  | EBool true -> return VTrue
  | EBool false -> return VFalse
  | EVar id -> begin
    let%bind env = ask in
    match Env.find id env with
    | Some v -> return v
    | None -> fail @@ Err.unbound_variable id
  end
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
  | ENot expr ->
    let%bind v = analyze expr in
    not_ v
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
      | None -> fail Err.type_mismatch
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
    | VRecord record_body -> begin
      match Map.find record_body label with
      | Some v -> return v
      | None -> fail Err.type_mismatch
    end 
    | _ -> fail Err.type_mismatch
  end
  | EVariant { label ; payload } ->
    let%bind v = analyze payload in
    return @@ VVariant { label ; payload = v }
  (* branching *)
  | EIf { cond ; true_body ; false_body } -> begin
    match%bind analyze cond with
    | VTrue -> analyze true_body
    | VFalse -> analyze false_body
    | _ -> fail @@ Err.type_mismatch
  end
  | ECase { subject ; cases ; default } -> begin
    match%bind analyze subject with
    | VZero -> analyze default
    | VPosInt -> (* relying on the assumption that cases are on positive ints *)
      let%bind (_, expr) = choose cases in
      analyze expr
    | _ -> fail @@ Err.type_mismatch
  end
  (* todos *)
  | ELet _
  | EAppl _
  | EFunction _
  | EFreeze _
  | EThaw _
  | EIgnore _ -> failwith "unhandled todo"
  (* termination *)
  | EDiverge -> vanish
  | EAbort _ -> fail @@ Err.abort
  (* unhandled and currently ignored *)
  | EDet expr
  | EEscapeDet expr -> analyze expr
  (* unhandled and currently aborting *)
  | ETable
  | ETblAppl _ -> failwith "unimplemented analysis on tables"
