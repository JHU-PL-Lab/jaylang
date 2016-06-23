open Batteries;;
open Ast;;
open Uid;;

module Ident_map = Ast.Ident_map;;

type proof =
  | Var_rule of ident * uid
  | Record_expr_rule of ident * uid
  | Function_expr_rule of ident * uid
  | Int_expr_rule of ident * uid
  | Bool_expr_rule of ident * uid
  | String_expr_rule of ident * uid
  | Ref_expr_rule of ident * uid
  | Var_expr_rule of ident * uid
  | Appl_expr_rule of ident * uid
  | Conditional_expr_rule of ident * uid
  | Deref_expr_rule of ident * uid
  | Update_expr_rule of ident * uid
  | Binary_operation_expr_rule of ident * uid
  | Unary_operation_expr_rule of ident * uid
  | Indexing_expr_rule of ident * uid
  | Let_expr_rule of ident * uid
  | Projection_expr_rule of ident * uid
  | Function_rule of ident * uid
  | Record_pattern_rule of ident * uid
  | Fun_pattern_rule of ident * uid
  | Ref_pattern_rule of ident * uid
  | Int_pattern_rule of ident * uid
  | Bool_pattern_rule of ident * uid
  | String_pattern_rule of ident * uid
  | Nested_var_rule of ident * uid

let first(x,_,_) = x;;
let second(_,x,_) = x;;
let third(_,_,x) = x;;

let disjoint_union m1 m2 =
  Ident_map.merge (fun _ xo yo -> match xo,yo with
      | Some _, Some _ -> raise (Utils.Invariant_failure "Same Idents merged")
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None
    ) m1 m2
;;

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "a__" ^ (string_of_int index) in
  Ast.Var(Ast.Ident(name),None)
;;

let rec pattern_of_nested_pattern p =
  match p with
  | Nested_ast.Record_pattern(_,elements) ->
    (* TO DO - uids for patterns *)
    Ast.Record_pattern (Ident_map.map pattern_of_nested_pattern elements)
  | Nested_ast.Fun_pattern(_) -> Ast.Fun_pattern
  | Nested_ast.Ref_pattern(_) -> Ast.Ref_pattern
  | Nested_ast.Int_pattern(_) -> Ast.Int_pattern
  | Nested_ast.Bool_pattern(_,b) -> Ast.Bool_pattern(b)
  | Nested_ast.String_pattern(_) -> Ast.String_pattern
  | Nested_ast.Any_pattern(_) -> Ast.Any_pattern
;;

let rec function_value_of_nested_function_value
    (Nested_ast.Function(uid,Nested_ast.Nested_var(_,x'),e')) =
  let (body,_,orig_map) = clauses_and_var_of_nested_expr e' in
  let this_map = disjoint_union orig_map (Ident_map.singleton x' (Function_rule(x',uid))) in
  (Ast.Function_value(Ast.Var(x',None),Ast.Expr body), this_map)

and clauses_and_var_of_nested_expr e =
  let x = fresh_var() in
  match x with Ast.Var(id,_) ->
    let (clauses,final_body,map) =
      match e with
      | Nested_ast.Record_expr(u,elements) ->
        let elements' =
          Ident_map.map clauses_and_var_of_nested_expr elements
        in
        let record_body = Ident_map.map second elements' in
        let extra_clauses =
          elements'
          |> Ident_map.enum
          |> Enum.map (snd %> first)
          |> List.of_enum
          |> List.concat
        in
        let singleton = Ident_map.singleton id (Record_expr_rule(id,u)) in
        let combined_map =
          elements'
          |> Ident_map.enum
          |> Enum.map (snd %> third)
          |> Enum.fold disjoint_union singleton
        in
        (extra_clauses,
         Ast.Value_body(Ast.Value_record(Ast.Record_value record_body)),
        combined_map)
      | Nested_ast.Function_expr(u,f) ->
        let (fv,fvmap) = function_value_of_nested_function_value f in
        ([], Ast.Value_body(
            Ast.Value_function(
              fv)), disjoint_union fvmap (Ident_map.singleton id (Function_expr_rule(id,u))))
      | Nested_ast.Int_expr(u,n) ->
        ([], Ast.Value_body(Ast.Value_int(n)), (Ident_map.singleton id (Int_expr_rule(id,u))))
      | Nested_ast.Bool_expr(u,b) ->
        ([], Ast.Value_body(Ast.Value_bool(b)), Ident_map.singleton id (Bool_expr_rule(id,u)))
      | Nested_ast.String_expr(u,s) ->
        ([], Ast.Value_body(Ast.Value_string(s)), Ident_map.singleton id (String_expr_rule(id,u)))
      | Nested_ast.Ref_expr(u,e') ->
        let (cls0,x',orig_map) = clauses_and_var_of_nested_expr e' in
        let this_map = Ident_map.singleton id (Ref_expr_rule(id,u)) in
        (cls0, Ast.Value_body(Ast.Value_ref(Ast.Ref_value(x'))), disjoint_union this_map orig_map)
      | Nested_ast.Var_expr(u,Nested_ast.Nested_var(_,x')) ->
        ([], Ast.Var_body(Ast.Var(x',None)), Ident_map.singleton id (Var_expr_rule(id,u)))
      | Nested_ast.Appl_expr(u,e1,e2) ->
        let this_map = Ident_map.singleton id (Appl_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        let (cls2,x2,map2) = clauses_and_var_of_nested_expr e2 in
        let first_union = disjoint_union map1 map2 in
        (cls1 @ cls2, Ast.Appl_body(x1,x2), disjoint_union this_map first_union)
      | Nested_ast.Conditional_expr(u,e',p,f1,f2) ->
        let this_map = Ident_map.singleton id (Conditional_expr_rule(id,u)) in
        let (fv1, fvmap1) = function_value_of_nested_function_value f1 in
        let (fv2, fvmap2) = function_value_of_nested_function_value f2 in
        let fvmap = disjoint_union fvmap1 fvmap2 in
        let (cls0,x',orig_map) = clauses_and_var_of_nested_expr e' in
        ( cls0
        , Ast.Conditional_body(
            x',
            pattern_of_nested_pattern p,
            fv1,
            fv2)
        , disjoint_union (disjoint_union this_map orig_map) fvmap)
      | Nested_ast.Deref_expr(u,e') ->
        let this_map = Ident_map.singleton id (Deref_expr_rule(id,u)) in
        let (cls0,x',orig_map) = clauses_and_var_of_nested_expr e' in
        ( cls0
        , Ast.Deref_body(x')
        , disjoint_union this_map orig_map)
      | Nested_ast.Update_expr(u,e1,e2) ->
        let this_map = Ident_map.singleton id (Update_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        let (cls2,x2,map2) = clauses_and_var_of_nested_expr e2 in
        let first_union = disjoint_union map1 map2 in
        ( cls1 @ cls2
        , Ast.Update_body(x1,x2)
        , disjoint_union this_map first_union
        )
      | Nested_ast.Binary_operation_expr(u,e1,op,e2) ->
        let this_map = Ident_map.singleton id (Binary_operation_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        let (cls2,x2,map2) = clauses_and_var_of_nested_expr e2 in
        let first_union = disjoint_union map1 map2 in
        ( cls1 @ cls2
        , Ast.Binary_operation_body(x1,op,x2)
        , disjoint_union this_map first_union
        )
      | Nested_ast.Unary_operation_expr(u,op,e1) ->
        let this_map = Ident_map.singleton id (Unary_operation_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        (cls1, Ast.Unary_operation_body(op,x1),disjoint_union map1 this_map)
      | Nested_ast.Indexing_expr(u,e1,e2) ->
          let this_map = Ident_map.singleton id (Indexing_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        let (cls2,x2,map2) = clauses_and_var_of_nested_expr e2 in
        let first_union = disjoint_union map1 map2 in
        ( cls1 @ cls2
        , Ast.Indexing_body(x1,x2)
        , disjoint_union this_map first_union
        )
      | Nested_ast.Let_expr(u,Nested_ast.Nested_var(_,x'),e1,e2) ->
        let this_map = Ident_map.singleton id (Let_expr_rule(id,u)) in
        let (cls1,x1,map1) = clauses_and_var_of_nested_expr e1 in
        let (cls2,x2,map2) = clauses_and_var_of_nested_expr e2 in
        let first_union = disjoint_union map1 map2 in
        ( cls1 @
          [ Ast.Clause(Ast.Var(x',None), Ast.Var_body(x1)) ] @
          cls2
        , Ast.Var_body(x2)
        , disjoint_union this_map first_union
        )
      | Nested_ast.Projection_expr(u,e',i) ->
        let this_map = Ident_map.singleton id (Projection_expr_rule(id,u)) in
        let (cls0,x',orig_map) = clauses_and_var_of_nested_expr e' in
        ( cls0
        , Ast.Projection_body(x', i)
        , disjoint_union orig_map this_map
        )
    in
    (clauses @ [Ast.Clause(x,final_body)], x, map)

let a_translate_nested_expr e =
  let (cls,_,map) = clauses_and_var_of_nested_expr e in
  ((Ast.Expr cls), map)
;;
