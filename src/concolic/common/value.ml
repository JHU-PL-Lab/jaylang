
open Core

module Concolic_value = struct
  type 'a t = 'a * 'a Expression.t
  let to_string f (v, _) = f v
end

include Lang.Value.Embedded (Concolic_value)

include T

let rec equal (a : t) (b : t) : bool =
  match a, b with
  (* Equality of concolic expressions*)
  | VInt (_, e1), VInt (_, e2) -> Expression.equal e1 e2
  | VBool (_, e1), VBool (_, e2) -> Expression.equal e1 e2
  (* Propogation of equality *)
  | VVariant r1, VVariant r2 ->
    Lang.Ast.VariantLabel.equal r1.label r2.label
    && equal r1.payload r2.payload
  | VRecord r1, VRecord r2 -> Core.Map.equal equal r1 r2
  (* Intensional equality of expressions in closures *)
  | VFunClosure r1, VFunClosure r2 ->
    equal_closure [ r1.param, r2.param ] r1.body r2.body
    (* phys_equal r1.body.expr r2.body.expr *)
  | VFrozen c1, VFrozen c2 -> equal_closure [] c1 c2
  (* Physical equality of mutable values *) (* TODO: confirm this is safe *)
  | VTable r1, VTable r2 -> phys_equal r1.alist r2.alist
  (* Intensional equality *)
  | VUnboundVariable id1, VUnboundVariable id2 -> Lang.Ast.Ident.equal id1 id2
  | VId, VId
  | VTypeMismatch, VTypeMismatch
  | VAbort, VAbort
  | VDiverge, VDiverge -> true
  | _ -> false

(*
  We'll do what we can to check equality of closures.
  This is intensional equality of expressions, where variables
  are looked up in the environments. We do not make an attempt
  to associate values with expressions, so if one closure is
    { env = { x |-> 1 } ; body = x + 1 }
  and the other is
    { env = {} ; body = 1 + 1 }
  we do not equate them. Variables are only used in comparison
  when both are in the same spot.

  New variable bindings are first done by comparing the bodies
  and then by comparing the de Bruijn index at their use.
  Any variable that is not a new binding is compared by the value
  in the environment.
*)
and equal_closure bindings a b =
  let rec equal_expr bindings x y =
    let eq = equal_expr bindings in
    match x, y with
    | Lang.Ast.Expr.EInt i1, Lang.Ast.Expr.EInt i2 -> Int.equal i1 i2
    | EBool b1, EBool b2 -> Bool.equal b1 b2
    | EVar id1, EVar id2 -> begin
      List.find_map bindings ~f:(fun (d1, d2) ->
        if Lang.Ast.Ident.equal id1 d1 then
          Some (Lang.Ast.Ident.equal id2 d2) (* Found bound in left original expression (in `a`), make sure also in right *)
        else if Lang.Ast.Ident.equal id2 d2 then
          Some false (* Found bound in right but not in left, so these are not equivalent identifiers *)
        else
          None
      )
      |> function
        | Some b -> b (* A binding was found. Make sure they're equal *)
        | None -> Option.equal equal (Env.fetch id1 a.env) (Env.fetch id2 b.env) (* No binding. Fetch from environment and compare values *)
    end
    | EBinop r1, EBinop r2 ->
      Lang.Ast.Binop.equal r1.binop r2.binop
      && eq r1.left r2.left
      && eq r1.right r2.right
    | EIf r1, EIf r2 ->
      eq r1.cond r2.cond
      && eq r1.true_body r2.true_body
      && eq r1.false_body r2.false_body
    | ELet r1, ELet r2 ->
      eq r1.body r2.body
      && equal_expr ((r1.var, r2.var) :: bindings) r1.cont r2.cont
    | EAppl r1, EAppl r2 ->
      eq r1.arg r2.arg
      && eq r1.func r2.func
    | EMatch r1, EMatch r2 ->
      eq r1.subject r2.subject
      && List.equal (fun (p1, e1) (p2, e2) ->
        (* inline compare patterns *)
        match p1, p2 with
        | Lang.Ast.Pattern.PAny, PAny -> eq e1 e2
        | PVariable id1, PVariable id2 -> equal_expr ((id1, id2) :: bindings) e1 e2
        | PVariant v1, PVariant v2 ->
          Lang.Ast.VariantLabel.equal v1.variant_label v2.variant_label
          && equal_expr ((v1.payload_id, v2.payload_id) :: bindings) e1 e2
        | _ -> false
      ) r1.patterns r2.patterns
    | EProject r1, EProject r2 ->
      Lang.Ast.RecordLabel.equal r1.label r2.label
      && eq r1.record r2.record
    | ERecord r1, ERecord r2 -> Map.equal eq r1 r2
    | EFunction r1, EFunction r2 ->
      equal_expr ((r1.param, r2.param) :: bindings) r1.body r2.body
    | EVariant r1, EVariant r2 ->
      Lang.Ast.VariantLabel.equal r1.label r2.label
      && eq r1.payload r2.payload
    | ECase r1, ECase r2 ->
      eq r1.subject r2.subject
      && eq r1.default r2.default
      && List.equal (fun (i1, e1) (i2, e2) ->
        Int.equal i1 i2
        && eq e1 e2
       ) r1.cases r2.cases
    | EIgnore r1, EIgnore r2 ->
      eq r1.ignored r2.ignored
      && eq r1.cont r2.cont
    | ETblAppl r1, ETblAppl r2 ->
      eq r1.tbl r2.tbl
      && eq r1.gen r2.gen
      && eq r1.arg r2.arg
    (* Simple propagation *)
    | EFreeze e1, EFreeze e2
    | EThaw e1, EThaw e2
    | EDet e1, EDet e2
    | EEscapeDet e1, EEscapeDet e2
    | ENot e1, ENot e2 -> eq e1 e2
    (* Intensional equality *)
    | ETable, ETable
    | EPick_i, EPick_i
    | EPick_b, EPick_b
    | EId, EId
    | EAbort _, EAbort _ (* ignore abort messages *)
    | EDiverge, EDiverge -> true
    (* Not equal *)
    | _ -> false
  in
  equal_expr bindings a.expr b.expr