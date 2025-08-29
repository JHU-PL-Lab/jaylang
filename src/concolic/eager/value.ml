
open Core

module Make (K : Smt.Symbol.KEY) = struct
  module Concolic_value = Common.Cvalue.Make (K)

  include Lang.Value.Embedded (Concolic_value)

  include T

  (*
    I want a result and a log.
  *)
  module X = struct
    (*
      I actually think I should have a state that lets me accumulate the boolean result and
      also a reader setup so that I can see if we are in a nondeterministic block or not.

      This is because I'm tired of calling (&&) on the result, and a simple `and_bool` that
      modifies the state would be good.
    *)
    include Preface.Make.Writer.Over_monad (Preface.Option.Monad) (Utils.List_monoid.Make (struct type t = (bool, K.t) Smt.Formula.t end))
    let bind x f = bind f x

    let never_equal : 'a t = None

    let assert_bool (b : bool) : unit t =
      if b
      then return ()
      else never_equal

    let eq_map_data (eq : 'a -> 'a -> bool t) (x : ('b, 'a, 'c) Map.t) (y : ('b, 'a, 'c) Map.t) : bool t =
      Map.fold2 x y ~init:(return true) ~f:(fun ~key:_ ~data acc_m ->
        let%bind acc = acc_m in
        match data with
        | `Left _ | `Right _ -> never_equal
        | `Both (v1, v2) ->
          let%bind b = eq v1 v2 in
          return (acc && b)
      )

    let eq_list (eq : 'a -> 'a -> bool t) (x : 'a list) (y : 'a list) : bool t =
      List.fold2 ~init:(return true) ~f:(fun acc_m a b ->
        let%bind acc = acc_m in
        let%bind r = eq a b in
        return (acc && r)
      ) x y
      |> function
        | Core.List.Or_unequal_lengths.Ok b -> b
        | Unequal_lengths -> never_equal
  end

  let rec equal (a : t) (b : t) : bool X.t =
    let open X in
    if phys_equal a b then return true else
    match a, b with
    (* Equality of concolic expressions*)
    | VInt (i1, e1), VInt (i2, e2) -> 
      let%bind () = tell [ Smt.Formula.binop Smt.Binop.Equal e1 e2 ] in
      return (i1 = i2)
    | VBool (b1, e1), VBool (b2, e2) ->
      let%bind () = tell [ Smt.Formula.binop Smt.Binop.Equal e1 e2 ] in
      return Bool.(b1 = b2)
    (* Propogation of equality *)
    | VVariant r1, VVariant r2 ->
      let%bind () = assert_bool @@ Lang.Ast.VariantLabel.equal r1.label r2.label in
      equal r1.payload r2.payload
    | VUntouchable v1, VUntouchable v2 -> equal v1 v2
    | VRecord r1, VRecord r2 -> eq_map_data equal r1 r2
    (* Intensional equality of expressions in closures *)
    | VFunClosure r1, VFunClosure r2 ->
      equal_closure [ r1.param, r2.param ] r1.closure r2.closure
    | VFrozen c1, VFrozen c2 -> 
      equal_closure [] c1 c2
    | VTable r1, VTable r2 -> 
      let%bind b = eq_list (fun (k1, v1) (k2, v2) ->
        let%bind b1 = equal k1 k2 in
        let%bind b2 = equal v1 v2 in
        return (b1 && b2)
        ) r1.alist r2.alist
      in
      return b
    (* Intensional equality *)
    | VUnboundVariable id1, VUnboundVariable id2 ->
      let%bind () = assert_bool @@ Lang.Ast.Ident.equal id1 id2 in
      return true
    | VId, VId
    | VTypeMismatch, VTypeMismatch
    | VAbort, VAbort
    | VVanish, VVanish
    | VUnit, VUnit -> return true
    | _ -> never_equal (* they are structurally different and cannot be equal *)

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
    let open X in
    if phys_equal a b then return true else
    let rec equal_expr bindings x y =
      let eq = equal_expr bindings in
      match x, y with
      | Lang.Ast.Expr.EUnit, Lang.Ast.Expr.EUnit -> return true
      | EInt i1, EInt i2 -> 
        let%bind () = assert_bool @@ Int.equal i1 i2 in
        return true
      | EBool b1, EBool b2 ->
        let%bind () = assert_bool @@ Bool.equal b1 b2 in
        return true
      | EVar id1, EVar id2 -> begin
        List.find_map bindings ~f:(fun (d1, d2) ->
          if Lang.Ast.Ident.equal id1 d1 then
            (* Found bound in left original expression (in `a`), make sure also in right *)
            Some (let%bind () = assert_bool @@ Lang.Ast.Ident.equal id2 d2 in return true)
          else if Lang.Ast.Ident.equal id2 d2 then
            Some never_equal (* Found bound in right but not in left, so these are not equivalent identifiers *)
          else
            None
        )
        |> function
          | Some b -> b (* A binding was found. Make sure they're equal *)
          | None -> (* No binding. Fetch from the environment and compare values *)
            match Env.fetch id1 a.env, Env.fetch id2 b.env with
            | Some v1, Some v2 -> equal v1 v2
            | _ -> never_equal
      end
      | EBinop r1, EBinop r2 ->
        let%bind () = assert_bool @@ Lang.Ast.Binop.equal r1.binop r2.binop in
        let%bind left_eq = eq r1.left r2.left in
        let%bind right_eq = eq r1.right r2.right in
        return (left_eq && right_eq)
      | EIf r1, EIf r2 ->
        let%bind eq_cond = eq r1.cond r2.cond in
        let%bind eq_true_body = eq r1.true_body r2.true_body in
        let%bind eq_false_body = eq r1.false_body r2.false_body in
        return (eq_cond && eq_true_body && eq_false_body)
      | ELet r1, ELet r2 ->
        let%bind eq_defn = eq r1.defn r2.defn in
        let%bind eq_body = equal_expr ((r1.var, r2.var) :: bindings) r1.body r2.body in
        return (eq_defn && eq_body)
      | EAppl r1, EAppl r2 ->
        let%bind eq_arg = eq r1.arg r2.arg in
        let%bind eq_func = eq r1.func r2.func in
        return (eq_arg && eq_func)
      | EMatch r1, EMatch r2 ->
        let%bind eq_subj = eq r1.subject r2.subject in
        let%bind eq_patterns =
          eq_list (fun (p1, e1) (p2, e2) ->
            (* inline compare patterns *)
            match p1, p2 with
            | Lang.Ast.Pattern.PAny, PAny 
            | PInt, PInt
            | PBool, PBool
            | PType, PType
            | PRecord, PRecord
            | PModule, PModule
            | PFun, PFun
            | PUnit, PUnit -> eq e1 e2
            | PVariable id1, PVariable id2 -> equal_expr ((id1, id2) :: bindings) e1 e2
            | PVariant v1, PVariant v2 ->
              if Lang.Ast.VariantLabel.equal v1.variant_label v2.variant_label
              then equal_expr ((v1.payload_id, v2.payload_id) :: bindings) e1 e2
              else never_equal
            | PUntouchable id1, PUntouchable id2 -> equal_expr ((id1, id2) :: bindings) e1 e2
            | _ -> never_equal
          ) r1.patterns r2.patterns
        in
        return (eq_subj && eq_patterns)
      | EProject r1, EProject r2 ->
        if Lang.Ast.RecordLabel.equal r1.label r2.label
        then eq r1.record r2.record
        else never_equal
      | ERecord r1, ERecord r2 -> eq_map_data eq r1 r2
      | EFunction r1, EFunction r2 ->
        equal_expr ((r1.param, r2.param) :: bindings) r1.body r2.body
      | EVariant r1, EVariant r2 ->
        if Lang.Ast.VariantLabel.equal r1.label r2.label
        then eq r1.payload r2.payload
        else never_equal
      | ECase r1, ECase r2 ->
        let%bind eq_subj = eq r1.subject r2.subject in
        let%bind eq_default = eq r1.default r2.default in
        let%bind eq_cases =
          eq_list (fun (i1, e1) (i2, e2) ->
            let%bind () = assert_bool @@ Int.equal i1 i2 in
            eq e1 e2
          ) r1.cases r2.cases
        in
        return (eq_subj && eq_default && eq_cases)
      | EIgnore r1, EIgnore r2 ->
        let%bind eq_ignored = eq r1.ignored r2.ignored in
        let%bind eq_cont = eq r1.ignored r2.ignored in
        return (eq_ignored && eq_cont)
      | ETableAppl r1, ETableAppl r2 ->
        let%bind eq_tbl = eq r1.tbl r2.tbl in
        let%bind eq_gen = eq r1.gen r2.gen in
        let%bind eq_arg = eq r1.arg r2.arg in
        return (eq_tbl && eq_gen && eq_arg)
      (* Simple propagation *)
      | EFreeze e1, EFreeze e2
      | EThaw e1, EThaw e2
      | EDet e1, EDet e2
      | EEscapeDet e1, EEscapeDet e2
      | ENot e1, ENot e2
      | EUntouchable e1, EUntouchable e2
      | EDefer e1, EDefer e2 -> eq e1 e2
      (* Intensional equality *)
      | ETableCreate, ETableCreate
      | EPick_i, EPick_i
      | EPick_b, EPick_b
      | EId, EId
      | EAbort _, EAbort _ (* ignore abort messages *)
      | EVanish (), EVanish () -> return true
      (* Not equal *)
      | _ -> never_equal
    in
    equal_expr bindings a.body b.body

  let equal a b =
    match X.run @@ equal a b with
    | Some (b, exprs) -> b, Smt.Formula.and_ exprs
    | None -> Concolic_value.return_bool false
end

module Default = Make (Interp_common.Step)

include Default
