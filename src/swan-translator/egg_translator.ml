open Batteries;;
open Uid;;

module Ident_map = Core_ast.Ident_map;;

(* type translation_failure =
   | No_match_clauses_in_match_expr of Swan_ast.expr
   [@@ deriving eq, ord, show]

   exception Translation_failure_exception of translation_failure *)

let lazy_logger = Logger_utils.make_lazy_logger "Swan_translator";;

type log_entry =
  | If_to_match of uid * uid
  (** First uid is the resulting `match' and second uid is the `if' where it came from. *)

  | Match_to_application of uid * uid
  (** First uid is the resulting application and second uid is the `match' where it came from. *)

  | Match_to_conditional of uid * uid
  (** First uid is the resulting conditional and second uid is the `match' where it came from. *)

  | Match_to_let of uid * uid
  (** First uid is the resulting `let' and second uid is the `match' where it came from. *)

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
  Uid_map.merge (fun k xo yo -> match xo,yo with
      | Some x, Some y -> raise (Utils.Invariant_failure ("Same UID `" ^ show_uid k ^ "' merged. Left log entry: `" ^ show_log_entry x ^ "'. Right log entry: `" ^ show_log_entry y ^ "'."))
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

let expression_translator_compose_many ts =
  List.reduce expression_translator_compose ts
;;

let pattern_translator_compose_many ts =
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
      let (trans_p, map_p) = top_level_pattern_translator p in
      let (trans_e1, map_e1) = top_level_expression_translator e1 in
      let (trans_e2, map_e2) = top_level_expression_translator e2 in
      let unioned_map = disjoint_unions [map_e;map_p;map_e1;map_e2] in
      (Egg_ast.Conditional_expr(uid, trans_e, trans_p, Egg_ast.Function (uid_f1,x1,trans_e1), Egg_ast.Function (uid_f2,x2,trans_e2)), unioned_map)
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
            let (trans_p, map_p) = top_level_pattern_translator p in
            let (trans_me, map_me) = top_level_expression_translator me in
            let unioned_map = disjoint_unions [unioned_map;map_p;map_me;] in
            (Egg_ast.Match_pair (uid, trans_p, trans_me) :: trans_ms, unioned_map)
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
    | Egg_ast.Var_pattern (_,_)
    | Egg_ast.Any_pattern _ -> (p, Uid_map.empty)
  in
  (top_level_expression_translator, top_level_pattern_translator)
;;

let identity_translator (a : 'a) : 'a translation_result =
  (a, Uid_map.empty)
;;

let identity_translator_fragment _ = identity_translator;;

let translate_ifthenelse
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.If_expr(uid_if, e_condition, e_then, e_else) ->
    let uid_match = next_uid () in
    let (e_trans, map_e) =
      tc.continuation_expression_translator @@
      Egg_ast.Match_expr (
        uid_match, e_condition,
        [ Egg_ast.Match_pair (next_uid (), Egg_ast.Bool_pattern (next_uid (), true), e_then);
          Egg_ast.Match_pair (next_uid (), Egg_ast.Bool_pattern (next_uid (), false), e_else);
        ]
      )
    in
    let map_new = Uid_map.singleton uid_match (If_to_match(uid_match, uid_if)) in
    (e_trans, disjoint_union map_e map_new)
  | _ -> tc.continuation_expression_translator e
;;

let translate_match
    (tc:translator_configuration)
    (e:Egg_ast.expr) =

  let base_translation x_subject =
    let uid_new = next_uid () in
    match e with
    | Egg_ast.Match_expr(uid_match, _, []) ->
      (Egg_ast.Appl_expr (uid_new, Egg_ast.String_expr (next_uid (), "non-function"),
                          Egg_ast.Var_expr (next_uid (), x_subject)),
       uid_new,
       Uid_map.singleton uid_new (Match_to_application(uid_new, uid_match)))
    | Egg_ast.Match_expr(uid_match, _, Egg_ast.Match_pair (_, pattern, e_branch) :: rest_branches) ->
      (Egg_ast.Conditional_expr (
          uid_new, Egg_ast.Var_expr (next_uid (), x_subject), pattern,
          Egg_ast.Function (next_uid (), egg_fresh_var (), e_branch),
          Egg_ast.Function (next_uid (), egg_fresh_var (),
                            Egg_ast.Match_expr (next_uid (), Egg_ast.Var_expr (next_uid (), x_subject),
                                                rest_branches))),
       uid_new,
       Uid_map.singleton uid_new (Match_to_conditional(uid_new, uid_match)))
    | _ -> raise @@ Utils.Invariant_failure "`base_translation' called with something other than `Match_expr'."
  in

  match e with
  | Egg_ast.Match_expr(_, Egg_ast.Var_expr (_, x_subject), _) ->
    let (e_base, _, map_new) = base_translation x_subject in
    let (e_trans, map_e) = tc.continuation_expression_translator e_base in
    (e_trans, disjoint_union map_e map_new)
  | Egg_ast.Match_expr(uid_match, e_subject, _) ->
    let x_subject = egg_fresh_var () in
    let (e_base, uid_new, _) = base_translation x_subject in
    let e_wrapped = (Egg_ast.Let_expr (next_uid (), x_subject, e_subject, e_base)) in
    let (e_trans, map_e) = tc.continuation_expression_translator e_wrapped in
    let map_new = Uid_map.singleton uid_new (Match_to_let(uid_new, uid_match)) in
    (e_trans, disjoint_union map_e map_new)
  | _ -> tc.continuation_expression_translator e
;;

(* let translate_pattern_variables *)
(*     (tc:translator_configuration) *)
(*     (p:Egg_ast.pattern) = *)
(*   match p with *)
(*   | Egg_ast.Var_pattern (uid, _) -> *)
(*     let any_pattern_uid = next_uid () in *)
(*     (Egg_ast.Any_pattern(any_pattern_uid), *)
(*      Uid_map.singleton any_pattern_uid *)
(*        (Pattern_variable_to_any_pattern (any_pattern_uid, uid))) *)
(*   | _ -> tc.continuation_pattern_translator p *)
(* ;; *)

(* let translate_conditional_with_pattern_variable *)
(*     (tc:translator_configuration) *)
(*     (e:Egg_ast.expr) = *)
(*   match e with *)
(*   | Egg_ast.Conditional_expr (uid, e, p, *)
(*                               (Egg_ast.Function (uid_1, x1, e1)), *)
(*                               (Egg_ast.Function (uid_2, x2, e2))) -> *)
(*     let (_, pattern_variables_translator) = *)
(*       translation_close identity_translator_fragment translate_pattern_variables *)
(*     in *)

(*     (\* TODO: Detect whether _there are pattern variables_ and only create the `let' and all else if necessary. *\) *)
(*     (\* TODO: If the pattern is just a variable, skip the `let' *\) *)

(*     ignore uid_1; ignore uid_2; *)
(*     (\* let rec pattern_variable_bindings subject pattern map = *\) *)
(*     (\*   match pattern with *\) *)
(*     (\*   | Swan_ast.Var_pattern (_, ident) -> *\) *)
(*     (\*     ([(ident, subject)], map) *\) *)
(*     (\*   | Swan_ast.Record_pattern (_, fields) -> *\) *)
(*     (\*     Ident_map.fold ( *\) *)
(*     (\*       fun label subpattern result -> *\) *)
(*     (\*         List.append result @@ *\) *)
(*     (\*         pattern_variable_bindings *\) *)
(*     (\*           (Swan_ast.Projection_expr (next_uid (), subject, label)) *\) *)
(*     (\*           subpattern *\) *)
(*     (\*     ) fields [] *\) *)
(*     (\*   | _ -> [] *\) *)
(*     (\* in *\) *)

(*     (\* let prepend_variable_bindings e subject pattern map = *\) *)
(*     (\*   let (bindings, map) = pattern_variable_bindings subject pattern map in *\) *)
(*     (\*   match bindings with *\) *)
(*     (\*   | [] -> (e, map) *\) *)
(*     (\*   | bindings -> *\) *)
(*     (\*     List.fold_right ( *\) *)
(*     (\*       fun (ident, projection) (e, map) -> *\) *)
(*     (\*         let let_new_uid = next_uid () in *\) *)
(*     (\*         let var_new_uid = next_uid () in *\) *)
(*     (\*         (Swan_ast.Let_expr ( *\) *)
(*     (\*             let_new_uid, *\) *)
(*     (\*             Swan_ast.Swan_var (var_new_uid, ident), *\) *)
(*     (\*             projection, *\) *)
(*     (\*             e *\) *)
(*     (\*           ), *\) *)
(*     (\*          disjoint_union map @@ *\) *)
(*     (\*          Uid_map.of_enum @@ List.enum [ *\) *)
(*     (\*            (let_new_uid, Projection_let_in_conditional_with_pattern_variables (let_new_uid, uid)); *\) *)
(*     (\*            (var_new_uid, Projection_var_in_conditional_with_pattern_variables (var_new_uid, uid)); *\) *)
(*     (\*          ]) *\) *)
(*     (\*     ) bindings (e, map) *\) *)
(*     (\* in *\) *)

(*     let x = egg_fresh_var () in *)
(*     let new_let_uid = next_uid () in *)
(*     let new_x_uid = next_uid () in *)
(*     let new_conditional_uid = next_uid () in *)
(*     let new_match_uid = next_uid () in *)
(*     let new_antimatch_uid = next_uid () in *)
(*     let x_var = Egg_ast.Var_expr(new_x_uid, x) in *)
(*     let (e_trans, e_map) = tc.top_level_expression_translator e in *)
(*     let (p_with_variables_trans, p_with_variables_map) = tc.top_level_pattern_translator p in *)
(*     let (p_trans, p_map) = pattern_variables_translator p_with_variables_trans in *)
(*     let (e1_augmented, e1_augmented_map) = (e1, Uid_map.empty) in (\* TODO: I'm a placeholder, REMOVE ME. *\) *)
(*     (\* let (e1_augmented, e1_augmented_map) = prepend_variable_bindings e1 x_var p_trans Uid_map.empty in *\) *)
(*     let (e1_trans, e1_map) = tc.top_level_expression_translator e1_augmented in *)
(*     let (e2_trans, e2_map) = tc.top_level_expression_translator e2 in *)
(*     let this_map = *)
(*       Uid_map.of_enum @@ List.enum [ *)
(*         (new_let_uid, (Conditional_with_pattern_variables_to_let_assignments_of_projection_chains (new_let_uid, uid))); *)
(*         (new_x_uid, (Conditional_with_pattern_variables_to_variable_assignments_of_projection_chains (new_x_uid, uid))); *)
(*         (new_conditional_uid, (Conditional_with_pattern_variables_to_conditional_assignments_of_projection_chains (new_conditional_uid, uid))); *)
(*         (new_match_uid, (Branch_of_conditional_with_pattern_variables (new_match_uid, uid))); *)
(*         (new_antimatch_uid, (Branch_of_conditional_with_pattern_variables (new_antimatch_uid, uid))); *)
(*       ] *)
(*     in *)
(*     let map = *)
(*       disjoint_unions [ this_map; e_map; p_with_variables_map; p_map; e1_augmented_map; e1_map; e2_map; ] *)
(*     in *)
(*     (Egg_ast.Let_expr (new_let_uid, x, e_trans, *)
(*                        Egg_ast.Conditional_expr (new_conditional_uid, x_var, p_trans, *)
(*                                                  (Egg_ast.Function (new_match_uid, x1, e1_trans)), *)
(*                                                  (Egg_ast.Function (new_antimatch_uid, x2, e2_trans)))), *)
(*      map) *)
(*   | _ -> tc.continuation_expression_translator e *)
(* ;; *)

let expression_translators : Egg_ast.expr translator_fragment list =
  [ translate_ifthenelse;
    translate_match;
    (* TODO: Enable this: translate_conditional_with_pattern_variable; *)
  ]
;;

let pattern_translators : Egg_ast.pattern translator_fragment list =
  [ identity_translator_fragment; ]
;;
