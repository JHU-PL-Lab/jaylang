open Batteries;;
open On_ast;;

(* *** Generalized transformations for expressions with reader and writer
       support. *** *)

type ('env,'out) env_out_expr_transformer =
  ('env -> On_ast.core_natodefa -> (On_ast.core_natodefa * 'out)) ->
  'env ->
  On_ast.core_natodefa ->
    On_ast.core_natodefa * 'out
;;

(* TODO: Update this later *)
let rec env_out_transform_expr
    (transformer : ('env,'out) env_out_expr_transformer)
    (combiner : 'out -> 'out -> 'out)
    (default : 'out)
    (env : 'env)
    (e : On_ast.core_natodefa)
  : On_ast.core_natodefa * 'out =
  let recurse : 'env -> On_ast.core_natodefa -> On_ast.core_natodefa * 'out =
    env_out_transform_expr transformer combiner default
  in
  let transform_funsig (On_ast.Funsig(name,args,body)) =
    let body_body = body.body in
    let body_tag = body.tag in
    let (body',out') = recurse env body_body in
    let (body'',out'') = transformer recurse env body' in
    (On_ast.Funsig(name,args,expr_desc_with_og_tag body_tag body''), combiner out' out'')
  in
  let (e' : On_ast.core_natodefa), (out' : 'out) =
    match e with
    | On_ast.Var x ->
      (On_ast.Var x, default)
    | On_ast.Input ->
      (On_ast.Input, default)
    | On_ast.Function (x, e1) ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      (On_ast.Function(x, new_expr_desc e1'), out1)
    | On_ast.Appl (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Appl(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Let (x, e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Let(x, new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.LetRecFun (funsigs, e1) ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      let (funsigs', outs) = List.split @@ List.map transform_funsig funsigs in
      let out = List.fold_left combiner out1 outs in
      (On_ast.LetRecFun(funsigs', new_expr_desc e1'), out)
    | On_ast.LetFun (funsig, e1) ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      let (funsig', out2) = transform_funsig funsig in
      (On_ast.LetFun(funsig', new_expr_desc e1'), combiner out1 out2)
    | On_ast.Plus (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Plus(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Minus (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Minus(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Times (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Times(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Divide (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Divide(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Modulus (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Modulus(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Equal(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Equal(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Neq(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Neq(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.LessThan(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.LessThan(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Leq(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Leq(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.GreaterThan(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.GreaterThan(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Geq(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Geq(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.And(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.And(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Or(e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.Or(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Not e1 ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      (On_ast.Not(new_expr_desc e1'), out1)
    | On_ast.If (e1, e2, e3) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let e3_body = e3.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      let (e3', out3) = recurse env e3_body in
      (On_ast.If(new_expr_desc e1', new_expr_desc e2', new_expr_desc e3'), combiner (combiner out1 out2) out3)
    | On_ast.Int n -> (On_ast.Int n, default)
    | On_ast.Bool b -> (On_ast.Bool b, default)
    | On_ast.Record r ->
      let kvpairs, outs =
        On_ast.Ident_map.enum r
        |> Enum.map
          (fun (k,v) ->
             let (v',out) = recurse env v.body in
             ((k,new_expr_desc v'), out)
          )
        |> Enum.uncombine
      in
      let r' = On_ast.Ident_map.of_enum kvpairs in
      let out = Enum.fold combiner default outs in
      (On_ast.Record r', out)
    | On_ast.RecordProj (e1, l) ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      (On_ast.RecordProj(new_expr_desc e1', l), out1)
    | On_ast.Match (e0, branches) ->
      let e0_body = e0.body in
      let (e0', out0) = recurse env e0_body in
      let (branches', outs) =
        List.split @@
        List.map
          (fun (pat,expr) ->
             let (expr',out) = recurse env expr.body in
             ((pat, new_expr_desc expr'), out)
          )
          branches
      in
      let out = List.fold_left combiner out0 outs in
      (On_ast.Match(new_expr_desc e0', branches'), out)
    | On_ast.VariantExpr (l, e1) ->
      let e1_body = e1.body in
      let (e1', out1) = recurse env e1_body in
      (On_ast.VariantExpr(l, new_expr_desc e1'), out1)
    | On_ast.List es ->
      let (es', outs) = List.split @@ List.map (fun e -> recurse env e.body) es in
      let es'' = List.map (fun e -> new_expr_desc e) es' in
      let out = List.fold_left combiner default outs in
      (On_ast.List es'', out)
    | On_ast.ListCons (e1, e2) ->
      let e1_body = e1.body in
      let e2_body = e2.body in
      let (e1', out1) = recurse env e1_body in
      let (e2', out2) = recurse env e2_body in
      (On_ast.ListCons(new_expr_desc e1', new_expr_desc e2'), combiner out1 out2)
    | On_ast.Assert e ->
      let e_body = e.body in
      let (e', out1) = recurse env e_body in
      (On_ast.Assert(new_expr_desc e'), out1)
    | On_ast.Assume e ->
      let e_body = e.body in
      let (e', out1) = recurse env e_body in
      (On_ast.Assume(new_expr_desc e'), out1)
    | On_ast.Untouched s -> (On_ast.Untouched s, default)
    | On_ast.TypeError s -> (On_ast.TypeError s, default)
  in
  let (e'', out'') = transformer recurse env e' in
  (e'', combiner out' out'')
;;

type 'env env_expr_transformer =
  ('env -> On_ast.core_natodefa -> On_ast.core_natodefa) -> 'env -> On_ast.core_natodefa -> On_ast.core_natodefa
;;

let env_transform_expr
    (transformer : 'env env_expr_transformer)
    (env : 'env)
    (e : On_ast.core_natodefa)
  : On_ast.core_natodefa =
  let transformer'
      (recurse : 'env -> On_ast.core_natodefa -> (On_ast.core_natodefa * unit))
      env
      (e : On_ast.core_natodefa)
    : (On_ast.core_natodefa * unit) =
    let recurse' env e =
      let (e'', ()) = recurse env e in e''
    in
    (transformer recurse' env e, ())
  in
  let (e', ()) = env_out_transform_expr transformer' (fun _ _ -> ()) () env e in
  e'
;;

type expr_transformer =
  (On_ast.core_natodefa -> On_ast.core_natodefa) -> On_ast.core_natodefa -> On_ast.core_natodefa
;;

let transform_expr (transformer : expr_transformer) (e : On_ast.core_natodefa)
  : On_ast.core_natodefa =
  let transformer'
      (recurse : unit -> On_ast.core_natodefa -> On_ast.core_natodefa) () (e : On_ast.core_natodefa)
    : On_ast.core_natodefa =
    let recurse' e = recurse () e in
    transformer recurse' e
  in
  env_transform_expr transformer' () e
;;