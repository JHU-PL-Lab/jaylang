open Batteries;;
open Uid;;

module Ident_map = Core_ast.Ident_map;;

(* type translation_failure =
   | No_match_clauses_in_match_expr of Swan_ast.expr
   [@@ deriving eq, ord, show]

   exception Translation_failure_exception of translation_failure *)

let lazy_logger = Logger_utils.make_lazy_logger "Swan_translator";;

type log_entry =
  | If_to_conditional of uid * uid
  (** First uid is a conditional and second uid is the if_expr that it came from *)

  | If_true_branch_to_function of uid * uid
  (** First uid is the function for the true branch of If_expr, second is the If_expr itself *)

  | If_false_branch_to_function of uid * uid
  (** First uid is the function for the false branch of If_expr, second is the If_expr itself *)

  | Bad_if_branch_to_function of uid * uid
  (** First uid is the function, second is the If_expr itself *)

  | Inexhaustive_match_branch of uid * uid
  (** First uid is the empty application expr, second is the match expr itself *)

  | Match_branch of uid * uid
  (** First uid is the resulting conditional, second is the match expr itself *)

  [@@deriving eq, show]

(* type proof =
   | Var_rule of uid * uid
   | Function_rule of uid * uid
   | Record_pattern_rule of uid * uid
   | Fun_pattern_rule of uid * uid
   | Ref_pattern_rule of uid * uid
   | Int_pattern_rule of uid * uid
   | Bool_pattern_rule of uid * uid
   | String_pattern_rule of uid * uid
   | Any_pattern_rule of uid * uid
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
   [@@deriving show] *)
(* uid of Swan_ast * uid of Nested_ast * uid of Appl_expr * list of Match pair rules*)

let fresh_var_counter = ref 0;;

let egg_fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "s__" ^ (string_of_int index) in
  Egg_ast.Egg_var(next_uid (), Core_ast.Ident(name))
;;

let nested_fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "s__" ^ (string_of_int index) in
  Nested_ast.Nested_var(next_uid (), Core_ast.Ident(name))
;;

let disjoint_union m1 m2 =
  Uid_map.merge (fun _ xo yo -> match xo,yo with
      | Some _, Some _ -> raise (Utils.Invariant_failure "Same UIDs merged")
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None
    ) m1 m2
;;

let disjoint_unions ms =
  List.fold_left disjoint_union Uid_map.empty ms
;;

type 'a translation_result =
  'a * log_entry Uid_map.t
;;

type 'a translator = 'a -> 'a translation_result
;;

type translator_configuration =
  { continuation_expression_translator : Egg_ast.expr translator;
    top_level_expression_translator : Egg_ast.expr translator;
    continuation_pattern_translator : Egg_ast.pattern translator;
    top_level_pattern_translator : Egg_ast.pattern translator;
  }
;;

type 'a translator_fragment =
  translator_configuration -> 'a translator
;;

let expression_translator_compose
    (t1:Egg_ast.expr translator_fragment)
    (t2:Egg_ast.expr translator_fragment)
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  t1 { tc with continuation_expression_translator = (t2 tc) } e
;;

let pattern_translator_compose
    (t1:Egg_ast.pattern translator_fragment)
    (t2:Egg_ast.pattern translator_fragment)
    (tc:translator_configuration)
    (p:Egg_ast.pattern) =
  t1 { tc with continuation_pattern_translator = (t2 tc) } p
;;

let rec expression_translator_compose_many ts =
  List.reduce expression_translator_compose ts
;;

let rec pattern_translator_compose_many ts =
  List.reduce pattern_translator_compose ts
;;

let translation_close
    (t:Egg_ast.expr translator_fragment)
    (pt:Egg_ast.pattern translator_fragment)
  : (Egg_ast.expr translator * Egg_ast.pattern translator) =
  let rec translator_configuration = {
    continuation_expression_translator = transitive_expression_translator;
    top_level_expression_translator = top_level_expression_translator;
    continuation_pattern_translator = transitive_pattern_translator;
    top_level_pattern_translator = top_level_pattern_translator;
  }
  and top_level_expression_translator e = t translator_configuration e
  and top_level_pattern_translator p = pt translator_configuration p
  and transitive_expression_translator (e:Egg_ast.expr) =
    match e with
    | Egg_ast.Record_expr(uid,fields) ->
      let (trans_fields, unioned_map) =
        fields
        |> Ident_map.enum
        |> Enum.fold (
          fun (trans_fields, unioned_map) (ident, value_expr) ->
            let (trans_expr, map_expr) = top_level_expression_translator value_expr in
            (Ident_map.add ident trans_expr trans_fields, disjoint_union unioned_map map_expr)
        ) (Ident_map.empty, Uid_map.empty)
      in
      (Egg_ast.Record_expr(uid, trans_fields), unioned_map)
    | Egg_ast.Function_expr(uid_e,Egg_ast.Function (uid_f, x,e')) ->
      let (trans_e, map_e) =
        top_level_expression_translator e'
      in
      (Egg_ast.Function_expr(uid_e,Egg_ast.Function (uid_f, x,trans_e)), map_e)
    | Egg_ast.Int_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Bool_expr _ -> (e, Uid_map.empty)
    | Egg_ast.String_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Ref_expr(uid, e') ->
      let (trans_e, map_e) =
        top_level_expression_translator e'
      in
      (Egg_ast.Ref_expr(uid, trans_e), map_e)
    | Egg_ast.Var_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Appl_expr (uid, e1, e2) ->
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Appl_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Conditional_expr(uid, e, p, Egg_ast.Function (uid_f1,x1,e1), Egg_ast.Function (uid_f2,x2,e2)) ->
      let (trans_e, map_e) = top_level_expression_translator e in
      let (trans_e1, map_e1) = top_level_expression_translator e1 in
      let (trans_e2, map_e2) = top_level_expression_translator e2 in
      let unioned_map = disjoint_unions [map_e;map_e1;map_e2] in
      (Egg_ast.Conditional_expr(uid, trans_e, p, Egg_ast.Function (uid_f1,x1,trans_e1), Egg_ast.Function (uid_f2,x2,trans_e2)), unioned_map)
    | Egg_ast.If_expr(uid, e, e1, e2) ->
      let (trans_e, map_e) = top_level_expression_translator e
      in
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_unions [map_e;map_e1;map_e2]
      in (Egg_ast.If_expr(uid, trans_e, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Deref_expr(uid, e) ->
      let (trans_e, map_e) = top_level_expression_translator e in
      (Egg_ast.Deref_expr(uid, trans_e), map_e)
    | Egg_ast.Update_expr(uid, e1, e2) ->
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Update_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Binary_operation_expr(uid, e1, op, e2) ->
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Binary_operation_expr(uid, trans_e1, op, trans_e2), unioned_map)
    | Egg_ast.Unary_operation_expr(uid, op, e) ->
      let (trans_e, map_e) = top_level_expression_translator e in
      (Egg_ast.Unary_operation_expr(uid, op, trans_e), map_e)
    | Egg_ast.Indexing_expr(uid, e1, e2) ->
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Indexing_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Let_expr(uid, x, e1, e2) ->
      let (trans_e1, map_e1) = top_level_expression_translator e1
      in
      let (trans_e2, map_e2) = top_level_expression_translator e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Let_expr(uid, x, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Projection_expr(uid, e, i) ->
      let (trans_e, map_e) = top_level_expression_translator e in
      (Egg_ast.Projection_expr(uid, trans_e, i), map_e)
    | Egg_ast.Match_expr(uid, e, ms) ->
      let (trans_e, map_e) = top_level_expression_translator e in
      let (trans_ms, unioned_map) =
        List.fold_right (
          fun (Egg_ast.Match_pair (uid, p, me)) (trans_ms, unioned_map) ->
            let (trans_me, map_me) = top_level_expression_translator me in
            let unioned_map = disjoint_union unioned_map map_me in
            (Egg_ast.Match_pair (uid, p, trans_me) :: trans_ms, unioned_map)
        ) ms ([], map_e)
      in
      (Egg_ast.Match_expr(uid, trans_e, trans_ms), unioned_map)

  and transitive_pattern_translator (p:Egg_ast.pattern) =
    match p with
    | Egg_ast.Record_pattern (uid,fields) ->
      let (trans_fields, unioned_map) =
        fields
        |> Ident_map.enum
        |> Enum.fold (
          fun (trans_fields, unioned_map) (ident, sub_pattern) ->
            let (trans_pattern, map_pattern) = top_level_pattern_translator sub_pattern in
            (Ident_map.add ident trans_pattern trans_fields, disjoint_union unioned_map map_pattern)
        ) (Ident_map.empty, Uid_map.empty)
      in
      (Egg_ast.Record_pattern(uid, trans_fields), unioned_map)
    | Egg_ast.Fun_pattern _
    | Egg_ast.Ref_pattern _
    | Egg_ast.Int_pattern _
    | Egg_ast.Bool_pattern (_,_)
    | Egg_ast.String_pattern _
    | Egg_ast.Any_pattern _ -> (p, Uid_map.empty)
  in
  (top_level_expression_translator, top_level_pattern_translator)
;;

let rec translate_ifthenelse
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.If_expr(uid, cond, f1, f2) ->
    let x = egg_fresh_var () in
    let var1 = egg_fresh_var () in
    let var2 = egg_fresh_var () in
    let var3 = egg_fresh_var () in
    let var4 = egg_fresh_var () in
    let appl_uid = next_uid () in
    let (cond_trans, cond_map) = tc.top_level_expression_translator cond in
    let (f1_trans, f1_map) = tc.top_level_expression_translator f1 in
    let (f2_trans, f2_map) = tc.top_level_expression_translator f2 in
    let bad_if_branch = Egg_ast.Appl_expr(appl_uid, Egg_ast.String_expr(next_uid (), "{}"), cond_trans) in
    let (bad_trans, bad_map) = tc.top_level_expression_translator bad_if_branch in
    let (true_pattern, true_map) = tc.top_level_pattern_translator (Egg_ast.Bool_pattern(next_uid (), true)) in
    let (false_pattern, false_map) = tc.top_level_pattern_translator (Egg_ast.Bool_pattern(next_uid (), false)) in
    let rec desugar_if cond_trans f1_trans f2_trans =
      let new_uid = next_uid () in
      let f1_uid = next_uid () in
      let f2_uid = next_uid () in
      let desugared_expr =
        Egg_ast.Conditional_expr(
          new_uid,
          cond_trans,
          true_pattern,
          Egg_ast.Function(f1_uid, var1, f1_trans),
          Egg_ast.Function(
            next_uid (),
            var2,
            Egg_ast.Conditional_expr(
              next_uid (),
              cond_trans,
              false_pattern,
              Egg_ast.Function(
                next_uid (),
                var3,
                f2_trans),
              Egg_ast.Function(
                next_uid (),
                var4,
                bad_trans
              )
            )
          )
        )
      in
      let new_map =
        Uid_map.of_enum @@ List.enum @@
        [ (new_uid, If_to_conditional(new_uid,uid))
        ; (f1_uid, If_true_branch_to_function(f1_uid, uid))
        ; (f2_uid, If_false_branch_to_function(f2_uid, uid))
        ; (appl_uid, Bad_if_branch_to_function(appl_uid, uid))
        ]
        in
      let final_map =
        disjoint_unions [new_map; cond_map; f1_map; f2_map; bad_map; true_map; false_map]
        in (desugared_expr, final_map)
    in let (e', map) = desugar_if cond_trans f1_trans f2_trans in
    (Egg_ast.Let_expr(uid, x, cond_trans, e'), map)
  | _ -> tc.continuation_expression_translator e
;;

let rec translate_match
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.Match_expr(uid, e, ms) ->
    let x = egg_fresh_var () in
    let (trans_e, map_e) = tc.top_level_expression_translator e in
    let rec desugar_matches ms =
      let nu1 = next_uid () in
      let nu2 = next_uid () in
      match ms with
      | (Egg_ast.Match_pair(mu,p,e') as m)::ms' ->
        let (trans_e', map_e') = tc.top_level_expression_translator e' in
        let (trans_p, map_p) = tc.top_level_pattern_translator p in
        let (desugared_expr, desugared_map) = desugar_matches ms' in
        let f1 = Egg_ast.Function(nu1, egg_fresh_var (), trans_e') in
        let f2 = Egg_ast.Function(nu2, egg_fresh_var (), desugared_expr) in
        lazy_logger `trace (fun () ->
            Printf.sprintf "Translated match pair \n %s \n into \n %s \n and \n %s \n"
              (Pp_utils.pp_to_string Egg_ast.pp_match_pair m)
              (Pp_utils.pp_to_string Egg_ast.pp_function_value f1)
              (Pp_utils.pp_to_string Egg_ast.pp_function_value f2)
          );
        (Egg_ast.Conditional_expr(mu,trans_e,trans_p,f1,f2),
         (disjoint_unions [desugared_map; map_e; map_e'; map_p
                          ; Uid_map.singleton mu (Match_branch(mu, uid))]))
      | [] ->
        let appl_u = next_uid () in
        let this_map = (Uid_map.singleton appl_u (Inexhaustive_match_branch(appl_u,uid))) in
        (Egg_ast.Appl_expr(appl_u,(Egg_ast.Record_expr(nu1,Ident_map.empty)), trans_e),
         this_map)
    in let (e', map) = desugar_matches ms in
    (Egg_ast.Let_expr(uid, x, trans_e, e'), map)
  | _ -> tc.continuation_expression_translator e
;;

let identity_translator (a : 'a) : 'a translation_result =
  (a, Uid_map.empty)
;;

let identity_translator_fragment _ = identity_translator;;

let expression_translators : Egg_ast.expr translator_fragment list =
  [ translate_ifthenelse;
    translate_match;
    (* TODO: Add `translate_pattern_variables'. *)
  ]
;;

let pattern_translators : Egg_ast.pattern translator_fragment list =
  [ identity_translator_fragment ]
;;
