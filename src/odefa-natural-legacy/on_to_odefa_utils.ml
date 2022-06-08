open Batteries

(* *** Generalized transformations for expressions with reader and writer
       support. *** *)

type ('env, 'out) env_out_expr_transformer =
  ('env -> On_ast.expr -> On_ast.expr * 'out) ->
  'env ->
  On_ast.expr ->
  On_ast.expr * 'out

let rec env_out_transform_expr
    (transformer : ('env, 'out) env_out_expr_transformer)
    (combiner : 'out -> 'out -> 'out) (default : 'out) (env : 'env)
    (e : On_ast.expr) : On_ast.expr * 'out =
  let recurse : 'env -> On_ast.expr -> On_ast.expr * 'out =
    env_out_transform_expr transformer combiner default
  in
  let transform_funsig (On_ast.Funsig (name, args, body)) =
    let body', out' = recurse env body in
    let body'', out'' = transformer recurse env body' in
    (On_ast.Funsig (name, args, body''), combiner out' out'')
  in
  let (e' : On_ast.expr), (out' : 'out) =
    match e with
    | On_ast.Var x -> (On_ast.Var x, default)
    | On_ast.Input -> (On_ast.Input, default)
    | On_ast.Function (x, e1) ->
        let e1', out1 = recurse env e1 in
        (On_ast.Function (x, e1'), out1)
    | On_ast.Appl (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Appl (e1', e2'), combiner out1 out2)
    | On_ast.Let (x, e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Let (x, e1', e2'), combiner out1 out2)
    | On_ast.LetRecFun (funsigs, e1) ->
        let e1', out1 = recurse env e1 in
        let funsigs', outs = List.split @@ List.map transform_funsig funsigs in
        let out = List.fold_left combiner out1 outs in
        (On_ast.LetRecFun (funsigs', e1'), out)
    | On_ast.LetFun (funsig, e1) ->
        let e1', out1 = recurse env e1 in
        let funsig', out2 = transform_funsig funsig in
        (On_ast.LetFun (funsig', e1'), combiner out1 out2)
    | On_ast.Plus (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Plus (e1', e2'), combiner out1 out2)
    | On_ast.Minus (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Minus (e1', e2'), combiner out1 out2)
    | On_ast.Times (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Times (e1', e2'), combiner out1 out2)
    | On_ast.Divide (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Divide (e1', e2'), combiner out1 out2)
    | On_ast.Modulus (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Modulus (e1', e2'), combiner out1 out2)
    | On_ast.Equal (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Equal (e1', e2'), combiner out1 out2)
    | On_ast.Neq (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Neq (e1', e2'), combiner out1 out2)
    | On_ast.LessThan (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.LessThan (e1', e2'), combiner out1 out2)
    | On_ast.Leq (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Leq (e1', e2'), combiner out1 out2)
    | On_ast.GreaterThan (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Leq (e1', e2'), combiner out1 out2)
    | On_ast.Geq (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Geq (e1', e2'), combiner out1 out2)
    | On_ast.And (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.And (e1', e2'), combiner out1 out2)
    | On_ast.Or (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.Or (e1', e2'), combiner out1 out2)
    | On_ast.Not e1 ->
        let e1', out1 = recurse env e1 in
        (On_ast.Not e1', out1)
    | On_ast.If (e1, e2, e3) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        let e3', out3 = recurse env e3 in
        (On_ast.If (e1', e2', e3'), combiner (combiner out1 out2) out3)
    | On_ast.Int n -> (On_ast.Int n, default)
    | On_ast.Bool b -> (On_ast.Bool b, default)
    | On_ast.Record r ->
        let kvpairs, outs =
          On_ast.Ident_map.enum r
          |> Enum.map (fun (k, v) ->
                 let v', out = recurse env v in
                 ((k, v'), out))
          |> Enum.uncombine
        in
        let r' = On_ast.Ident_map.of_enum kvpairs in
        let out = Enum.fold combiner default outs in
        (On_ast.Record r', out)
    | On_ast.RecordProj (e1, l) ->
        let e1', out1 = recurse env e1 in
        (On_ast.RecordProj (e1', l), out1)
    | On_ast.Match (e0, branches) ->
        let e0', out0 = recurse env e0 in
        let branches', outs =
          List.split
          @@ List.map
               (fun (pat, expr) ->
                 let expr', out = recurse env expr in
                 ((pat, expr'), out))
               branches
        in
        let out = List.fold_left combiner out0 outs in
        (On_ast.Match (e0', branches'), out)
    | On_ast.VariantExpr (l, e1) ->
        let e1', out1 = recurse env e1 in
        (On_ast.VariantExpr (l, e1'), out1)
    | On_ast.List es ->
        let es', outs = List.split @@ List.map (recurse env) es in
        let out = List.fold_left combiner default outs in
        (On_ast.List es', out)
    | On_ast.ListCons (e1, e2) ->
        let e1', out1 = recurse env e1 in
        let e2', out2 = recurse env e2 in
        (On_ast.ListCons (e1', e2'), combiner out1 out2)
    | On_ast.Assert e ->
        let e', out = recurse env e in
        (On_ast.Assert e', out)
    | On_ast.Assume e ->
        let e', out = recurse env e in
        (On_ast.Assume e', out)
  in
  let e'', out'' = transformer recurse env e' in
  (e'', combiner out' out'')

type 'env env_expr_transformer =
  ('env -> On_ast.expr -> On_ast.expr) -> 'env -> On_ast.expr -> On_ast.expr

let env_transform_expr (transformer : 'env env_expr_transformer) (env : 'env)
    (e : On_ast.expr) : On_ast.expr =
  let transformer' (recurse : 'env -> On_ast.expr -> On_ast.expr * unit) env
      (e : On_ast.expr) : On_ast.expr * unit =
    let recurse' env e =
      let e'', () = recurse env e in
      e''
    in
    (transformer recurse' env e, ())
  in
  let e', () = env_out_transform_expr transformer' (fun _ _ -> ()) () env e in
  e'

type expr_transformer =
  (On_ast.expr -> On_ast.expr) -> On_ast.expr -> On_ast.expr

let transform_expr (transformer : expr_transformer) (e : On_ast.expr) :
    On_ast.expr =
  let transformer' (recurse : unit -> On_ast.expr -> On_ast.expr) ()
      (e : On_ast.expr) : On_ast.expr =
    let recurse' e = recurse () e in
    transformer recurse' e
  in
  env_transform_expr transformer' () e
