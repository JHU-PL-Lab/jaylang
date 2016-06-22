open Batteries;;
open Uid;;

module Ident_map = Ast.Ident_map;;

(* type translation_failure =
   | No_match_clauses_in_match_expr of Swan_ast.expr
   [@@ deriving eq, ord, show]

   exception Translation_failure_exception of translation_failure *)

type proof =
  | Var_rule of uid * uid
  | Function_rule of uid * uid
  | Record_pattern_rule of uid * uid
  | Fun_pattern_rule of uid * uid
  | Ref_pattern_rule of uid * uid
  | Int_pattern_rule of uid * uid
  | Bool_pattern_rule of uid * uid
  | String_pattern_rule of uid * uid
  | Match_pair_rule of uid * uid * uid * uid
  | Record_expr_rule of uid * uid
  | Function_expr_rule of uid * uid
  | Int_expr_rule of uid * uid
  | Bool_expr_rule of uid * uid
  | String_expr_rule of uid * uid
  | Ref_expr_rule of uid * uid
  | Var_expr_rule of uid * uid
  | Appl_expr_rule of uid * uid
  | Conditional_expr_rule of uid * uid
  | If_expr_rule of uid * uid
  | Deref_expr_rule of uid * uid
  | Update_expr_rule of uid * uid
  | Binary_operation_expr_rule of uid * uid
  | Unary_operation_expr_rule of uid * uid
  | Indexing_expr_rule of uid * uid
  | Let_expr_rule of uid * uid
  | Projection_expr_rule of uid * uid
  | Match_expr_rule of uid * uid
  [@@deriving show]
  (* uid of Swan_ast * uid of Nested_ast * uid of Appl_expr * list of Match pair rules*)

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "s__" ^ (string_of_int index) in
  Ast.Ident(name)
;;

let disjoint_union m1 m2 =
  Uid_map.merge (fun _ xo yo -> match xo,yo with
      | Some _, Some _ -> raise (Utils.Invariant_failure "Same UIDs merged")
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None
    ) m1 m2
;;

let nested_var_of_swan_var v =
  let nu = next_uid () in
  match v with
  | Swan_ast.Swan_var(u, x) -> (Nested_ast.Nested_var(nu, x), Uid_map.singleton nu (Var_rule(nu,u)))

let rec nested_pattern_of_swan_pattern p =
  let nu = next_uid () in
  match p with
  | Swan_ast.Fun_pattern u ->
    (Nested_ast.Fun_pattern nu, Uid_map.singleton nu (Fun_pattern_rule(nu,u)))
  | Swan_ast.Ref_pattern u ->
    (Nested_ast.Ref_pattern nu, Uid_map.singleton nu (Ref_pattern_rule(nu,u)))
  | Swan_ast.Int_pattern u ->
    (Nested_ast.Int_pattern nu, Uid_map.singleton nu (Int_pattern_rule(nu, u)))
  | Swan_ast.Bool_pattern(u,b) ->
    (Nested_ast.Bool_pattern(nu,b), Uid_map.singleton nu (Bool_pattern_rule(nu,u)))
  | Swan_ast.String_pattern u ->
    (Nested_ast.String_pattern nu, Uid_map.singleton nu (String_pattern_rule(nu,u)))
  | Swan_ast.Record_pattern (u,elements) ->
    let enum = Ident_map.enum elements in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map nested_pattern_of_swan_pattern values in
    let (patterns, proofs) = Enum.uncombine rec_result in
    let nested_elts = Enum.combine (keys, patterns) in
    let nested_map = Ident_map.of_enum nested_elts in
    let singleton = Uid_map.singleton nu (Record_pattern_rule(nu, u)) in
    let merged_proofs = Enum.fold disjoint_union singleton proofs in
    (Nested_ast.Record_pattern(nu, nested_map), merged_proofs)
(* raise (Utils.Not_yet_implemented "Nested_patter_of_swan Record_pattern") *)
(* (Nested_ast.Record_pattern (nu,Ident_map.map (nested_pattern_of_swan_pattern elements)), *)
(* Uid_map.singleton nu (Record_pattern_rule(nu,u))) *)

let rec nested_function_value_of_swan_function_value
    (Swan_ast.Function(u,v,e')) =
  let nu = next_uid () in
  let body = nested_expr_of_swan_expr e' in
  let this_map = Uid_map.singleton nu (Function_rule(nu,u)) in
  ((Nested_ast.Function(nu, fst (nested_var_of_swan_var v), (fst body))),
   (disjoint_union this_map (snd body)))

and nested_expr_of_swan_expr e : Nested_ast.expr * proof Uid_map.t =
  let nu = next_uid () in
  match e with
  | Swan_ast.Function_expr(u,f) ->
    (Nested_ast.Function_expr(nu, fst (nested_function_value_of_swan_function_value f)), (Uid_map.singleton nu (Function_expr_rule(nu,u))))
  | Swan_ast.Int_expr(u,n) -> (Nested_ast.Int_expr(nu,n), Uid_map.singleton nu (Int_expr_rule(nu,u)))
  | Swan_ast.Bool_expr(u,b) -> (Nested_ast.Bool_expr(nu,b), Uid_map.singleton nu  (Bool_expr_rule(nu,u)))
  | Swan_ast.String_expr(u,s) -> (Nested_ast.String_expr(nu,s), Uid_map.singleton nu (String_expr_rule(nu,u)))
  | Swan_ast.Ref_expr(u,e') ->
    let e_nested = nested_expr_of_swan_expr e' in
    let this_map = Uid_map.singleton nu (Ref_expr_rule(nu,u)) in
    (Nested_ast.Ref_expr(nu,(fst e_nested)), (disjoint_union this_map (snd e_nested)))
  | Swan_ast.Var_expr(u,v) -> (Nested_ast.Var_expr(nu, fst (nested_var_of_swan_var v)), Uid_map.singleton nu (Var_expr_rule(nu,u)))
  | Swan_ast.Appl_expr(u,e1,e2) ->
    let e1_trans = nested_expr_of_swan_expr e1 in
    let e2_trans = nested_expr_of_swan_expr e2 in
    let e_map = disjoint_union (snd e1_trans) (snd e2_trans) in
    let this_map = Uid_map.singleton nu (Appl_expr_rule(nu,u)) in
    (Nested_ast.Appl_expr(nu,(fst (nested_expr_of_swan_expr e1)),
                          (fst (nested_expr_of_swan_expr e2))),
     disjoint_union e_map this_map)
  | Swan_ast.Conditional_expr(u,e',p,f1,f2) ->
    let e_trans = nested_expr_of_swan_expr e' in
    let p_trans = nested_pattern_of_swan_pattern p in
    let f1_trans = nested_function_value_of_swan_function_value f1 in
    let f2_trans = nested_function_value_of_swan_function_value f2 in
    let submap = (disjoint_union (disjoint_union (snd e_trans) (snd p_trans))
                    (disjoint_union (snd f1_trans) (snd f2_trans))) in
    (Nested_ast.Conditional_expr(nu,
                                 (fst e_trans),
                                 (fst p_trans),
                                 (fst f1_trans),
                                 (fst f2_trans)),
     (disjoint_union submap (Uid_map.singleton nu (Conditional_expr_rule(nu,u)))))
  | Swan_ast.If_expr(u,e',e1,e2) ->
    let e_trans = nested_expr_of_swan_expr e' in
    let f1_trans = nested_function_value_of_swan_function_value (Swan_ast.Function(next_uid (), Swan_ast.Swan_var(next_uid (),fresh_var ()), e1)) in
    let f2_trans = (nested_function_value_of_swan_function_value (Swan_ast.Function(next_uid (), Swan_ast.Swan_var(next_uid (),fresh_var ()), e2))) in
    let submap = disjoint_union (disjoint_union (snd f1_trans) (snd f2_trans)) (snd e_trans) in
    let this_map = Uid_map.singleton nu (If_expr_rule(nu,u)) in
    (Nested_ast.Conditional_expr(nu,
                                 fst e_trans,
                                 Nested_ast.Bool_pattern(next_uid (),true),
                                 fst f1_trans,
                                 fst f2_trans
                                ),
     disjoint_union submap this_map)
  | Swan_ast.Deref_expr(u,e') ->
    let e_trans = nested_expr_of_swan_expr e' in
    (Nested_ast.Deref_expr(nu,(fst e_trans)), disjoint_union (snd e_trans)
       (Uid_map.singleton nu (Deref_expr_rule(nu,u))))
  | Swan_ast.Update_expr(u,e1,e2) ->
    let e1_trans = nested_expr_of_swan_expr e1 in
    let e2_trans = nested_expr_of_swan_expr e2 in
    let e_map = disjoint_union (snd e1_trans) (snd e2_trans) in
    let this_map = Uid_map.singleton nu (Update_expr_rule(nu,u)) in
    (Nested_ast.Update_expr(nu,
                            fst (nested_expr_of_swan_expr e1),
                            fst (nested_expr_of_swan_expr e2)),
     disjoint_union e_map this_map)
  | Swan_ast.Binary_operation_expr(u,e1,op,e2) ->
    let e1_trans = nested_expr_of_swan_expr e1 in
    let e2_trans = nested_expr_of_swan_expr e2 in
    let e_map = disjoint_union (snd e1_trans) (snd e2_trans) in
    let this_map = Uid_map.singleton nu (Binary_operation_expr_rule(nu,u)) in
    (Nested_ast.Binary_operation_expr(nu,
                                      fst (nested_expr_of_swan_expr e1), op,
                                      fst (nested_expr_of_swan_expr e2)),
     disjoint_union e_map this_map)
  | Swan_ast.Unary_operation_expr(u,op,e1) ->
    let e_trans = nested_expr_of_swan_expr e1 in
    let this_map = Uid_map.singleton nu (Unary_operation_expr_rule(nu,u)) in
    (Nested_ast.Unary_operation_expr(nu,op,
                                     fst (nested_expr_of_swan_expr e1)),
     disjoint_union (snd e_trans) this_map)
  | Swan_ast.Indexing_expr(u,e1,e2) ->
    let e1_trans = nested_expr_of_swan_expr e1 in
    let e2_trans = nested_expr_of_swan_expr e2 in
    let e_map = disjoint_union (snd e1_trans) (snd e2_trans) in
    let this_map = Uid_map.singleton nu (Indexing_expr_rule(nu,u)) in
    (Nested_ast.Indexing_expr(nu,
                              fst (nested_expr_of_swan_expr e1),
                              fst (nested_expr_of_swan_expr e2)),
     disjoint_union e_map this_map)
  | Swan_ast.Let_expr(u,v,e1,e2) ->
    let e1_trans = nested_expr_of_swan_expr e1 in
    let e2_trans = nested_expr_of_swan_expr e2 in
    let e_map = disjoint_union (snd e1_trans) (snd e2_trans) in
    let this_map = Uid_map.singleton nu (Let_expr_rule(nu,u)) in
    (Nested_ast.Let_expr(nu, fst (nested_var_of_swan_var v),
                         fst (nested_expr_of_swan_expr e1),
                         fst (nested_expr_of_swan_expr e2)),
     disjoint_union e_map this_map)
  | Swan_ast.Projection_expr(u,e',i) ->
    let e_trans = nested_expr_of_swan_expr e' in
    let this_map = Uid_map.singleton nu (Projection_expr_rule(nu,u)) in
    (Nested_ast.Projection_expr(nu, fst (nested_expr_of_swan_expr e'),i),
     disjoint_union (snd e_trans) this_map)
  | Swan_ast.Match_expr(u, e, ms) ->
    let x = Nested_ast.Nested_var(next_uid (),fresh_var ()) in
    let e_trans = nested_expr_of_swan_expr e in
    let rec desugar_matches ms =
      let nu1 = next_uid () in
      let nu2 = next_uid () in
      let cond_u = next_uid () in
      match ms with
      | Swan_ast.Match_pair(mu,p,e')::ms' ->
        let e'_trans = nested_expr_of_swan_expr e' in
        let p_trans = nested_pattern_of_swan_pattern p in
        let ep_map = disjoint_union (snd e'_trans) (snd p_trans) in
        let (desugared_expr, desugared_map) = desugar_matches ms' in
        let f1 = Nested_ast.Function(nu1,Nested_ast.Nested_var(next_uid (),fresh_var ()), fst e'_trans) in
        let f2 = Nested_ast.Function(nu2,Nested_ast.Nested_var(next_uid (),fresh_var ()), desugared_expr) in
        let this_map = disjoint_union desugared_map @@ disjoint_union ep_map @@ Uid_map.singleton cond_u (Match_pair_rule(cond_u, mu, nu1, nu2)) in
        (Nested_ast.Conditional_expr(cond_u,fst e_trans,fst p_trans,f1,f2),
         (disjoint_union (snd (desugar_matches ms')) this_map))
      | _ ->
        let this_map = (Uid_map.singleton cond_u (Appl_expr_rule(cond_u,u))) in
        (Nested_ast.Appl_expr(cond_u,(Nested_ast.Record_expr(nu1,Ident_map.empty)), (Nested_ast.Record_expr(nu2,Ident_map.empty))),
         this_map)
      (* raise (Translation_failure_exception(No_match_clauses_in_match_expr(e))) *)
    in let (e', map) = desugar_matches ms in
    let this_map = (disjoint_union map (Uid_map.singleton nu (Match_expr_rule(nu, u)))) in
    (Nested_ast.Let_expr(nu, x, fst e_trans, e'),
     this_map)
  | Swan_ast.Record_expr(u,t) ->
    let enum = Ident_map.enum t in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map nested_expr_of_swan_expr values in
    let (exprs, proofs) = Enum.uncombine rec_result in
    let nested_elts = Enum.combine (keys, exprs) in
    let nested_map = Ident_map.of_enum nested_elts in
    let singleton = Uid_map.singleton nu (Record_expr_rule(nu, u)) in
    let merged_proofs = Enum.fold disjoint_union singleton proofs in
    (Nested_ast.Record_expr(nu, nested_map), merged_proofs)
;;

let translate_swan_expr_to_nested e =
  nested_expr_of_swan_expr e
;;
