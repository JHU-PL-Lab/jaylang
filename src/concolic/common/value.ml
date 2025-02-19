
open Core

include Lang.Value.Embedded (struct type 'a t = 'a * 'a Expression.t let to_string f (v, _) = f v end)

include T

let rec equal_expr (a : Lang.Ast.Embedded.t) (b : Lang.Ast.Embedded.t) : bool =
  match a, b with
  | EInt i1, EInt i2 -> i1 = i2
  | EBool b1, EBool b2 -> Bool.(b1 = b2)
  | EVar id1, EVar id2 -> Lang.Ast.Ident.equal id1 id2
  | EBinop e1, EBinop e2 ->
    Lang.Ast.Binop.equal e1.binop e2.binop
    && equal_expr e1.left e2.left
    && equal_expr e1.right e2.right
  | EIf e1, EIf e2 ->
    equal_expr e1.cond e2.cond
    && equal_expr e1.true_body e2.true_body
    && equal_expr e1.false_body e2.false_body
  | ELet l1, ELet l2 ->
    Lang.Ast.Ident.equal l1.var l2.var
    && equal_expr l1.body l2.body
    && equal_expr l1.cont l2.cont
  | EAppl a1, EAppl a2 ->
    equal_expr a1.func a2.func
    && equal_expr a1.arg a2.arg
  | EMatch m1, EMatch m2 ->
    equal_expr m1.subject m2.subject
    && List.equal (fun p1 p2 ->
      let open Lang.Ast.Pattern in
      match p1, p2 with
      | (PAny, e1), (PAny, e2) -> equal_expr e1 e2
      | (PVariable id1, e1), (PVariable id2, e2) ->
        Lang.Ast.Ident.equal id1 id2
        && equal_expr e1 e2
      | (PVariant v1, e1), (PVariant v2, e2) -> 
        Lang.Ast.VariantLabel.equal v1.variant_label v2.variant_label
        && Lang.Ast.Ident.equal v1.payload_id v2.payload_id
        && equal_expr e1 e2
      | _ -> false
    ) m1.patterns m2.patterns
  | EProject p1, EProject p2 ->
    Lang.Ast.RecordLabel.equal p1.label p2.label
    && equal_expr p1.record p2.record
  | ERecord r1, ERecord r2 -> Map.equal equal_expr r1 r2
  | ENot e1, ENot e2 -> equal_expr e1 e2
  | EPick_i, EPick_i
  | EPick_b, EPick_b
  | EAbort, EAbort
  | EDiverge, EDiverge
  | EId, EId -> true
  | EFunction f1, EFunction f2 ->
    Lang.Ast.Ident.equal f1.param f2.param
    && equal_expr f1.body f2.body
  | EVariant v1, EVariant v2 ->
    Lang.Ast.VariantLabel.equal v1.label v2.label
    && equal_expr v1.payload v2.payload
  | ECase c1, ECase c2 ->
    equal_expr c1.subject c2.subject
    && equal_expr c1.default c2.default
    && List.equal (fun (i1, e1) (i2, e2) ->
      i1 = i2
      && equal_expr e1 e2
    ) c1.cases c2.cases
  | EFreeze e1, EFreeze e2 -> equal_expr e1 e2
  | EThaw e1, EThaw e2 -> equal_expr e1 e2
  | EIgnore e1, EIgnore e2 ->
    equal_expr e1.ignored e2.ignored
    && equal_expr e1.cont e2.cont
  | _ -> false


let rec equal (a : t) (b : t) : bool =
  match a, b with
  | VInt (i1, e1), VInt (i2, e2) -> i1 = i2 && Expression.equal e1 e2
  | VBool (b1, _), VBool (b2, _) -> Bool.(b1 = b2)
  | VFunClosure f1, VFunClosure f2 ->
    let (p1, body1, _) = !f1 in
    let (p2, body2, _) = !f2 in
    Lang.Ast.Ident.equal p1 p2
    && equal_expr body1.expr body2.expr
    && equal_env body1.env body2.env
  | VVariant v1, VVariant v2 ->
    Lang.Ast.VariantLabel.equal v1.label v2.label
    && equal v1.payload v2.payload
  | VRecord r1, VRecord r2 -> Map.equal equal r1 r2
  | VTypeMismatch, VTypeMismatch
  | VAbort, VAbort
  | VDiverge, VDiverge
  | VId, VId -> true
  | VFrozen c1, VFrozen c2 ->
    equal_expr c1.expr c2.expr
    && equal_env c1.env c2.env
  | _ -> false

and equal_env (a : Env.t) (b : Env.t) : bool =
  Map.equal equal a b