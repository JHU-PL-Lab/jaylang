(*

IMPLEMENTATION NOTES

- Progress: To guarantee that the translation process proceeds, the translation
  fragments must call the continuation translator on its results---whether it
  made progress or not.

- Correctness: To guarantee that the translation process fully executes, the
  translation fragments must only refer to other fragments are already
  defined---i.e., that come /before/ it.

- Completeness of mappings: To guarantee that the resugaring process will have
  all the information it needs, the following must hold:

  1. New `uid's must be generated for each AST node introduced during
  translation. (Old nodes being rearranged into new contexts don't need new
  `uid's.)

  2. `uid's must be used only once. (I.e., a subexpression can not be plugged
  into multiple holes in the resulting translation without refreshing---and
  mapping---`uid's.)

  3. Old nodes being destructed must be included in the mapping as auxiliary
  pointers---third position in the `log_entry' data type.

  4. All generated `uid's must be in the mapping. (I.e., every call to `next_uid
  ()' must have a corresponding `log_entry'.)

  5. All mappings from subordinate steps must be unioned. (I.e., every call to
  subordinate translators must have a corresponding `disjoint_union'.)

  6. Each /kind/ of mapping must have its own variant on the `log_entry' data
  type.

- Structure of mappings: A valid mapping is one in which the key is the `uid'
  for the newly generated piece of syntax, and the value is a pair---tagged with
  a `log_entry' variant---`(new_uid, original_uid)'.

*)

open Batteries;;
open Uid;;

module Ident_map = Core_ast.Ident_map;;

let lazy_logger = Logger_utils.make_lazy_logger "Swan_translator";;

type log_entry =
  | Var_pattern_to_any_pattern of uid * uid
  (** First uid is the resulting `any' pattern and second uid is the `var'
      pattern where it came from. *)

  | Conditional_with_pattern_variable_to_conditional of uid * uid
  (** First uid is the resulting conditional with no pattern variables and
      second uid is the conditional with pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_match_branch_in_conditional of uid * uid
  (** First uid is the resulting match branch in the resulting conditional with
      no pattern variables and second uid is the conditional with pattern
      variables where it came from. *)

  | Conditional_with_pattern_variable_to_formal_parameter_in_match_branch_in_conditional of uid * uid
  (** First uid is the resulting formal parameter in the resulting match branch
      in the resulting conditional with no pattern variables and second uid is
      the conditional with pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_projection_in_binding_in_match_branch_in_conditional of uid * uid
  (** First uid is the resulting projection in the resulting binding in the
      resulting match branch in the resulting conditional with no pattern
      variables and second uid is the conditional with pattern variables where
      it came from. *)

  | Conditional_with_pattern_variable_to_variable_in_projection_chain_in_binding_in_match_branch_in_conditional of uid * uid * uid
  (** First uid is the resulting variable in the resulting projection chain in
      the resulting binding in the resulting match branch in the resulting
      conditional with no pattern variables, second uid is the conditional with
      pattern variables where it came from and third uid is the variable in the
      conditional where it came from. *)

  | Conditional_with_pattern_variable_to_identifier_variable_in_projection_chain_in_binding_in_match_branch_in_conditional of uid * uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting projection chain in the resulting binding in the resulting match
      branch in the resulting conditional with no pattern variables, second uid
      is the conditional with pattern variables where it came from and third uid
      is the identifier in the variable in the conditional where it came
      from. *)

  | Conditional_with_pattern_variable_to_binding_in_match_branch_in_conditional of uid * uid
  (** First uid is the resulting binding in the resulting match branch in the
      resulting conditional with no pattern variables and second uid is the
      conditional with pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_variable_in_binding_in_match_branch_in_conditional of uid * uid
  (** First uid is the resulting variable in the resulting binding in the
      resulting match branch in the resulting conditional with no pattern
      variables and second uid is the conditional with pattern variables where
      it came from. *)

  | Conditional_with_pattern_variable_to_variable_in_conditional of uid * uid * uid
  (** First uid is the resulting variable in the resulting conditional with no
      pattern variables, second uid is the conditional with pattern variables
      where it came from and third uid is the variable in the conditional where
      it came from. *)

  | Conditional_with_pattern_variable_to_identifier_in_variable_in_conditional of uid * uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting conditional with no pattern variables, second uid is the
      conditional with pattern variables where it came from and third uid is the
      identifier in the variable in the conditional where it came from. *)

  | Conditional_with_pattern_variable_to_let of uid * uid
  (** First uid is the resulting `let' and second uid is the conditional with
      pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_identifier_in_let of uid * uid
  (** First uid is the resulting identifier in the resulting `let' and second
      uid is the conditional with pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_conditional_in_let of uid * uid
  (** First uid is the resulting conditional in the resulting `let' and second
      uid is the conditional with pattern variables where it came from. *)

  | Conditional_with_pattern_variable_to_variable_in_conditional_in_let of uid * uid
  (** First uid is the resulting variable in the resulting conditional in the
      resulting `let' and second uid is the conditional with pattern variables
      where it came from. *)

  | Conditional_with_pattern_variable_to_identifier_in_variable_in_conditional_in_let of uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting conditional in the resulting `let' and second uid is the
      conditional with pattern variables where it came from. *)

  | Cons_to_record of uid * uid
  (** First uid is the resulting record and second uid is the `cons' where it
      came from. *)

  | Empty_list_to_record of uid * uid
  (** First uid is the resulting record and second uid is the empty list where
      it came from. *)

  | Empty_list_to_record_in_record of uid * uid
  (** First uid is the resulting in record the resulting record and second uid
      is the empty list where it came from. *)

  | List_to_cons of uid * uid
  (** First uid is the resulting `cons' and second uid is the list where it came
      from. *)

  | List_to_list_in_cons of uid * uid
  (** First uid is the resulting list in the resulting `cons' and second uid is
      the list where it came from. *)

  | Match_to_application of uid * uid
  (** First uid is the resulting application and second uid is the `match' where
      it came from. *)

  | Match_to_non_function_in_application of uid * uid
  (** First uid is the resulting non-function in the resulting application and
      second uid is the `match' where it came from. *)

  | Match_to_variable_in_application of uid * uid * uid
  (** First uid is the resulting variable in the resulting application, second
      uid is the `match' where it came from and third uid is the variable in the
      `match' where it came from. *)

  | Match_to_identifier_in_variable_in_application of uid * uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting application, second uid is the `match' where it came from and
      third uid is the identifier in the variable in the `match' where it came
      from. *)

  | Match_to_conditional of uid * uid
  (** First uid is the resulting conditional and second uid is the `match' where
      it came from. *)

  | Match_to_variable_in_conditional of uid * uid * uid
  (** First uid is the resulting variable in the resulting conditional, second
      uid is the `match' where it came from and third uid is the variable in the
      `match' where it came from. *)

  | Match_to_identifier_in_variable_in_conditional of uid * uid * uid
  (** First uid is the resulting variable in the resulting conditional, second
      uid is the `match' where it came from and third is the identifier in the
      variable in the `match' where it came from. *)

  | Match_to_match_branch_in_conditional of uid * uid * uid
  (** First uid is the resulting match branch in the resulting conditional,
      second uid is the `match' where it came from and third is the match pair
      in the `match' where it came from. *)

  | Match_to_identifier_in_match_branch_in_conditional of uid * uid
  (** First uid is the resulting identifier in the resulting match branch in the
      resulting conditional and second uid is the `match' where it came from. *)

  | Match_to_antimatch_branch_in_conditional of uid * uid
  (** First uid is the resulting antimatch branch in the resulting conditional and
      second uid is the `match' where it came from. *)

  | Match_to_identifier_in_antimatch_branch_in_conditional of uid * uid
  (** First uid is the resulting identifier in the resulting antimatch branch in the
      resulting conditional and second uid is the `match' where it came from. *)

  | Match_to_match_in_antimatch_branch_in_conditional of uid * uid
  (** First uid is the resulting match in the resulting antimatch branch in the
      resulting conditional and second uid is the `match' where it came from. *)

  | Match_to_variable_in_match_in_antimatch_branch_in_conditional of uid * uid * uid
  (** First uid is the resulting variable in the resulting match in the
      resulting antimatch branch in the resulting conditional, second uid is the
      `match' where it came from and third uid is the variable from the `match'
      where it came from. *)

  | Match_to_identifier_in_variable_in_match_in_antimatch_branch_in_conditional of uid * uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting match in the resulting antimatch branch in the resulting
      conditional, second uid is the `match' where it came from and third uid is
      the identifier in the variable in the `match' where it came from. *)

  | Match_to_let of uid * uid
  (** First uid is the resulting `let' and second uid is the `match' where it
      came from. *)

  | Match_to_identifier_in_let of uid * uid
  (** First uid is the resulting identifier in the resulting`let' and second uid
      is the `match' where it came from. *)

  | Match_to_match_in_let of uid * uid
  (** First uid is the resulting `match' in the resulting `let' and second uid
      is the `match' where it came from. *)

  | Match_to_variable_in_match_in_let of uid * uid
  (** First uid is the resulting variable in the resulting `match' in the
      resulting `let' and second uid is the `match' where it came from. *)

  | Match_to_identifier_in_variable_in_match_in_let of uid * uid
  (** First uid is the resulting identifier in the resulting variable in the
      resulting `match' in the resulting `let' and second uid is the `match'
      where it came from. *)

  | If_to_match of uid * uid
  (** First uid is the resulting `match' and second uid is the `if' where it
      came from. *)

  | If_to_true_branch_in_match of uid * uid
  (** First uid is the resulting true branch in the resulting match and second
      uid is the `if' where it came from. *)

  | If_to_false_branch_in_match of uid * uid
  (** First uid is the resulting false branch in the resulting match and second
      uid is the `if' where it came from. *)

  | If_to_pattern_in_true_branch_in_match of uid * uid
  (** First uid is the resulting pattern in the resulting true branch in the
      resulting match and second uid is the `if' where it came from. *)

  | If_to_pattern_in_false_branch_in_match of uid * uid
  (** First uid is the resulting pattern in the resulting false branch in the
      resulting match and second uid is the `if' where it came from. *)

  [@@deriving eq, show]

let fresh_var_counter = ref 0;;

let egg_fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "s__" ^ (string_of_int index) in
  Core_ast.Ident(name)
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

let result_with_mappings (e_trans, map_e) maps_extra map_entries_new =
  let map_new = Uid_map.of_enum (List.enum map_entries_new) in
  (e_trans, disjoint_unions (map_e :: map_new :: maps_extra))
;;

let expression_continuation_with_mappings tc e maps_extra map_entries_new =
  result_with_mappings (tc.continuation_expression_translator e) maps_extra map_entries_new
;;

let pattern_continuation_with_mappings tc e maps_extra map_entries_new =
  result_with_mappings (tc.continuation_pattern_translator e) maps_extra map_entries_new
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
    | Egg_ast.List_expr(uid,elements) ->
      let (trans_elements, map_result) =
        elements
        |> List.fold_left (
          fun (trans_elements, map_partial) element ->
            let (e_trans, e_map) = top_level_expression_translator element in
            (e_trans :: trans_elements, disjoint_union map_partial e_map)
        ) ([], Uid_map.empty)
      in
      (Egg_ast.List_expr(uid, List.rev trans_elements), map_result)
    | Egg_ast.Cons_expr (uid, e_element, e_list) ->
      let (trans_e_element, map_e_element) = top_level_expression_translator e_element
      in
      let (trans_e_list, map_e_list) = top_level_expression_translator e_list
      in
      let unioned_map = disjoint_union map_e_element map_e_list in
      (Egg_ast.Cons_expr(uid, trans_e_element, trans_e_list), unioned_map)
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
    | Egg_ast.List_pattern(uid,elements) ->
      let (trans_elements, map_result) =
        elements
        |> List.fold_left (
          fun (trans_elements, map_partial) element ->
            let (e_trans, e_map) = top_level_pattern_translator element in
            (e_trans :: trans_elements, disjoint_union map_partial e_map)
        ) ([], Uid_map.empty)
      in
      (Egg_ast.List_pattern(uid, List.rev trans_elements), map_result)
    | Egg_ast.Cons_pattern(uid, p_element, p_list) ->
      let (p_element_trans, map_p_element) = top_level_pattern_translator p_element in
      let (p_list_trans, map_p_list) = top_level_pattern_translator p_list in
      let map_result = disjoint_unions [map_p_element;map_p_list] in
      (Egg_ast.Cons_pattern(uid, p_element_trans, p_list_trans), map_result)
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

let identity_expression_translator_fragment
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  tc.continuation_expression_translator e
;;

let identity_pattern_translator_fragment
    (tc:translator_configuration)
    (p:Egg_ast.pattern) =
  tc.continuation_pattern_translator p
;;

let translate_pattern_variable
    (tc:translator_configuration)
    (p:Egg_ast.pattern) =

  match p with
  | Egg_ast.Var_pattern (uid_var, _) ->
    let uid_any = next_uid () in
    pattern_continuation_with_mappings tc
      (Egg_ast.Any_pattern (uid_any))
      []
      [(uid_any, Var_pattern_to_any_pattern (uid_any, uid_var));]

  | _ -> tc.continuation_pattern_translator p
;;

let translate_conditional_with_pattern_variable
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.Conditional_expr (uid_conditional, Egg_ast.Var_expr (uid_variable_in_conditional, Egg_ast.Egg_var(uid_identifier_in_variable_in_conditional, x_subject)), p, f1, f2) when Egg_ast.contain_pattern_variable p ->

    let pattern_variable_bindings pattern =
      let rec step pattern accumulated_bindings partial_projection_chain_labels =
        match pattern with
        | Egg_ast.Var_pattern (_, Egg_ast.Egg_var (_, x)) ->
          (x, List.rev partial_projection_chain_labels) :: accumulated_bindings
        | Egg_ast.Record_pattern (_, fields) ->
          Ident_map.fold (
            fun label subpattern accumulated_bindings ->
              step subpattern accumulated_bindings (label :: partial_projection_chain_labels)
          ) fields accumulated_bindings
        | _ -> accumulated_bindings
      in
      step pattern [] []
    in

    let rec build_projection_chain e_subject projection_chain_labels =
      match projection_chain_labels with
      | [] -> (e_subject, Uid_map.empty)
      | projection_chain_label :: rest_projection_chain_labels ->
        let uid_projection_in_binding_in_match_branch_in_conditional' = next_uid () in
        let e' =
          Egg_ast.Projection_expr (uid_projection_in_binding_in_match_branch_in_conditional', e_subject, projection_chain_label)
        in
        result_with_mappings
          (build_projection_chain e' rest_projection_chain_labels)
          []
          [(uid_projection_in_binding_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_projection_in_binding_in_match_branch_in_conditional (uid_projection_in_binding_in_match_branch_in_conditional', uid_conditional));]
    in

    let rec build_pattern_variable_bindings x_subject e_body pattern_variable_bindings =
      match pattern_variable_bindings with
      | [] -> (e_body, Uid_map.empty)
      | (bound_variable, projection_chain_labels) :: rest_pattern_variable_bindings ->
        let uid_variable_in_projection_chain_in_binding_in_match_branch_in_conditional' = next_uid () in
        let uid_identifier_in_variable_in_projection_chain_in_binding_in_match_branch_in_conditional' = next_uid () in
        let uid_binding_in_match_branch_in_conditional' = next_uid () in
        let uid_variable_in_binding_in_match_branch_in_conditional' = next_uid () in
        let (projection_chain, map_projection_chain) =
          build_projection_chain
            (Egg_ast.Var_expr (uid_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', Egg_ast.Egg_var (uid_identifier_in_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', x_subject)))
            projection_chain_labels
        in
        let e' = Egg_ast.Let_expr (uid_binding_in_match_branch_in_conditional', Egg_ast.Egg_var (uid_variable_in_binding_in_match_branch_in_conditional', bound_variable), projection_chain, e_body) in
        result_with_mappings
          (build_pattern_variable_bindings x_subject e' rest_pattern_variable_bindings)
          [map_projection_chain;]
          [(uid_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_variable_in_projection_chain_in_binding_in_match_branch_in_conditional (uid_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', uid_conditional, uid_variable_in_conditional));
           (uid_identifier_in_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_identifier_variable_in_projection_chain_in_binding_in_match_branch_in_conditional (uid_identifier_in_variable_in_projection_chain_in_binding_in_match_branch_in_conditional', uid_conditional, uid_identifier_in_variable_in_conditional));
           (uid_binding_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_binding_in_match_branch_in_conditional (uid_binding_in_match_branch_in_conditional', uid_conditional));
           (uid_variable_in_binding_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_variable_in_binding_in_match_branch_in_conditional (uid_variable_in_binding_in_match_branch_in_conditional', uid_conditional));]
    in

    let preprend_pattern_variable_bindings pattern x_subject (Egg_ast.Function (_, Egg_ast.Egg_var (_, x_formal_parameter), e_body)) =
      let uid_match_branch_in_conditional' = next_uid () in
      let uid_formal_parameter_in_match_branch_in_conditional' = next_uid () in
      let (e_body', map_e_body') = build_pattern_variable_bindings x_subject e_body (pattern_variable_bindings pattern) in
      (Egg_ast.Function (uid_match_branch_in_conditional',
                         Egg_ast.Egg_var (uid_formal_parameter_in_match_branch_in_conditional', x_formal_parameter),
                         e_body'),
       disjoint_unions
         [map_e_body';
          Uid_map.of_enum @@ List.enum
            [(uid_match_branch_in_conditional', Conditional_with_pattern_variable_to_match_branch_in_conditional (uid_match_branch_in_conditional', uid_conditional));
             (uid_formal_parameter_in_match_branch_in_conditional', Conditional_with_pattern_variable_to_formal_parameter_in_match_branch_in_conditional (uid_formal_parameter_in_match_branch_in_conditional', uid_conditional));]])
    in

    let uid_conditional' = next_uid () in
    let uid_variable_in_conditional' = next_uid () in
    let uid_identifier_in_variable_in_conditional' = next_uid () in
    let (_, pattern_variables_translator) = translation_close identity_expression_translator_fragment translate_pattern_variable in
    let (p_trans, map_p) = pattern_variables_translator p in
    let (f1_trans, map_f1) = preprend_pattern_variable_bindings p x_subject f1 in
    expression_continuation_with_mappings tc
      (Egg_ast.Conditional_expr (uid_conditional', Egg_ast.Var_expr (uid_variable_in_conditional', Egg_ast.Egg_var (uid_identifier_in_variable_in_conditional', x_subject)),
                                 p_trans, f1_trans, f2))
      [map_p;map_f1;]
      [(uid_conditional', Conditional_with_pattern_variable_to_conditional (uid_conditional', uid_conditional));
       (uid_variable_in_conditional', Conditional_with_pattern_variable_to_variable_in_conditional (uid_variable_in_conditional', uid_conditional, uid_variable_in_conditional));
       (uid_identifier_in_variable_in_conditional', Conditional_with_pattern_variable_to_identifier_in_variable_in_conditional (uid_identifier_in_variable_in_conditional', uid_conditional, uid_identifier_in_variable_in_conditional));]

  | Egg_ast.Conditional_expr (uid_conditional, e_subject, p, f1, f2) when Egg_ast.contain_pattern_variable p ->
    let uid_let = next_uid () in
    let uid_identifier_in_let = next_uid () in
    let uid_conditional_in_let = next_uid () in
    let uid_variable_in_conditional_in_let = next_uid () in
    let uid_identifier_in_variable_in_conditional_in_let = next_uid () in
    let x_subject = egg_fresh_var () in
    expression_continuation_with_mappings tc
      (Egg_ast.Let_expr (uid_let, Egg_ast.Egg_var (uid_identifier_in_let, x_subject), e_subject,
                         Egg_ast.Conditional_expr (
                           uid_conditional_in_let, Egg_ast.Var_expr (uid_variable_in_conditional_in_let, Egg_ast.Egg_var (uid_identifier_in_variable_in_conditional_in_let, x_subject)), p, f1, f2)))
      []
      [(uid_let, Conditional_with_pattern_variable_to_let (uid_let, uid_conditional));
       (uid_identifier_in_let, Conditional_with_pattern_variable_to_identifier_in_let (uid_identifier_in_let, uid_conditional));
       (uid_conditional_in_let, Conditional_with_pattern_variable_to_conditional_in_let (uid_conditional_in_let, uid_conditional));
       (uid_variable_in_conditional_in_let, Conditional_with_pattern_variable_to_variable_in_conditional_in_let (uid_variable_in_conditional_in_let, uid_conditional));
       (uid_identifier_in_variable_in_conditional_in_let, Conditional_with_pattern_variable_to_identifier_in_variable_in_conditional_in_let (uid_identifier_in_variable_in_conditional_in_let, uid_conditional));]

  | _ -> tc.continuation_expression_translator e
;;

let translate_pattern_cons
    (tc:translator_configuration)
    (p:Egg_ast.pattern) =

  match p with
  | Egg_ast.Cons_pattern(uid_cons, p_element, p_list) ->
    let uid_record = next_uid () in
    pattern_continuation_with_mappings tc
      (Egg_ast.Record_pattern (
          uid_record, Core_ast.Ident_map.of_enum (
            List.enum [((Core_ast.Ident "head"), p_element);
                       ((Core_ast.Ident "tail"), p_list);])))
      []
      [(uid_record, Cons_to_record (uid_record, uid_cons));]

  | _ -> tc.continuation_pattern_translator p
;;

let translate_pattern_list
    (tc:translator_configuration)
    (p:Egg_ast.pattern) =

  match p with
  | Egg_ast.List_pattern(uid_list, []) ->
    let uid_record = next_uid () in
    let uid_record_in_record = next_uid () in
    pattern_continuation_with_mappings tc
      (Egg_ast.Record_pattern (
          uid_record, Core_ast.Ident_map.singleton (Core_ast.Ident "null")
            (Egg_ast.Record_pattern (uid_record_in_record, Core_ast.Ident_map.empty))))
      []
      [(uid_record, Empty_list_to_record (uid_record, uid_list));
       (uid_record_in_record, Empty_list_to_record_in_record (uid_record_in_record, uid_list));]

  | Egg_ast.List_pattern(uid_list, p_head :: p_tail) ->
    let uid_cons = next_uid () in
    let uid_list_in_cons = next_uid () in
    pattern_continuation_with_mappings tc
      (Egg_ast.Cons_pattern (
          uid_cons, p_head, (Egg_ast.List_pattern (uid_list_in_cons, p_tail))))
      []
      [(uid_cons, List_to_cons (uid_cons, uid_list));
       (uid_list_in_cons, List_to_list_in_cons (uid_list_in_cons, uid_list));]

  | _ -> tc.continuation_pattern_translator p
;;

let translate_expression_cons
    (tc:translator_configuration)
    (e:Egg_ast.expr) =

  match e with
  | Egg_ast.Cons_expr(uid_cons, e_element, e_list) ->
    let uid_record = next_uid () in
    expression_continuation_with_mappings tc
      (Egg_ast.Record_expr (
          uid_record, Core_ast.Ident_map.of_enum (
            List.enum [((Core_ast.Ident "head"), e_element);
                       ((Core_ast.Ident "tail"), e_list);])))
      []
      [(uid_record, Cons_to_record (uid_record, uid_cons));]

  | _ -> tc.continuation_expression_translator e
;;

let translate_expression_list
    (tc:translator_configuration)
    (e:Egg_ast.expr) =

  match e with
  | Egg_ast.List_expr(uid_list, []) ->
    let uid_record = next_uid () in
    let uid_record_in_record = next_uid () in
    expression_continuation_with_mappings tc
      (Egg_ast.Record_expr (
          uid_record, Core_ast.Ident_map.singleton (Core_ast.Ident "null")
            (Egg_ast.Record_expr (uid_record_in_record, Core_ast.Ident_map.empty))))
      []
      [(uid_record, Empty_list_to_record (uid_record, uid_list));
       (uid_record_in_record, Empty_list_to_record_in_record (uid_record_in_record, uid_list));]

  | Egg_ast.List_expr(uid_list, e_head :: e_tail) ->
    let uid_cons = next_uid () in
    let uid_list_in_cons = next_uid () in
    expression_continuation_with_mappings tc
      (Egg_ast.Cons_expr (
          uid_cons, e_head, (Egg_ast.List_expr (uid_list_in_cons, e_tail))))
      []
      [(uid_cons, List_to_cons (uid_cons, uid_list));
       (uid_list_in_cons, List_to_list_in_cons (uid_list_in_cons, uid_list));]

  | _ -> tc.continuation_expression_translator e
;;

let translate_match
    (tc:translator_configuration)
    (e:Egg_ast.expr) =

  match e with
  | Egg_ast.Match_expr(uid_match, Egg_ast.Var_expr (uid_variable_in_match, Egg_ast.Egg_var (uid_identifier_in_variable_in_match, x_subject)), []) ->
    let uid_application = next_uid () in
    let uid_non_function_in_application = next_uid () in
    let uid_variable_in_application = next_uid () in
    let uid_identifier_in_variable_in_application = next_uid () in
    expression_continuation_with_mappings tc
      (Egg_ast.Appl_expr (uid_application, Egg_ast.String_expr (uid_non_function_in_application, "non-function"),
                          Egg_ast.Var_expr (uid_variable_in_application, Egg_ast.Egg_var (uid_identifier_in_variable_in_application, x_subject))))
      []
      [(uid_application, Match_to_application (uid_application, uid_match));
       (uid_non_function_in_application, Match_to_non_function_in_application (uid_non_function_in_application, uid_match));
       (uid_variable_in_application, Match_to_variable_in_application (uid_variable_in_application, uid_match, uid_variable_in_match));
       (uid_identifier_in_variable_in_application, Match_to_identifier_in_variable_in_application (uid_identifier_in_variable_in_application, uid_match, uid_identifier_in_variable_in_match));]

  | Egg_ast.Match_expr(
      uid_match, Egg_ast.Var_expr (uid_variable_in_match, Egg_ast.Egg_var (uid_identifier_in_variable_in_match, x_subject)),
      Egg_ast.Match_pair (uid_match_pair_in_match, pattern, e_branch) :: rest_branches) ->
    let uid_conditional = next_uid () in
    let uid_variable_in_conditional = next_uid () in
    let uid_identifier_in_variable_in_conditional = next_uid () in
    let uid_match_branch_in_conditional = next_uid () in
    let uid_identifier_in_match_branch_in_conditional = next_uid () in
    let uid_antimatch_branch_in_conditional = next_uid () in
    let uid_identifier_in_antimatch_branch_in_conditional = next_uid () in
    let uid_match_in_antimatch_branch_in_conditional = next_uid () in
    let uid_variable_in_match_in_antimatch_branch_in_conditional = next_uid () in
    let uid_identifier_in_variable_in_match_in_antimatch_branch_in_conditional = next_uid () in
    let x_match_branch = egg_fresh_var () in
    let x_antimatch_branch = egg_fresh_var () in
    expression_continuation_with_mappings tc
      (Egg_ast.Conditional_expr (
          uid_conditional, Egg_ast.Var_expr (uid_variable_in_conditional, Egg_ast.Egg_var (uid_identifier_in_variable_in_conditional, x_subject)), pattern,
          Egg_ast.Function (uid_match_branch_in_conditional, Egg_ast.Egg_var (uid_identifier_in_match_branch_in_conditional, x_match_branch), e_branch),
          Egg_ast.Function (uid_antimatch_branch_in_conditional, Egg_ast.Egg_var (uid_identifier_in_antimatch_branch_in_conditional, x_antimatch_branch),
                            Egg_ast.Match_expr (uid_match_in_antimatch_branch_in_conditional,
                                                Egg_ast.Var_expr (uid_variable_in_match_in_antimatch_branch_in_conditional,
                                                                  Egg_ast.Egg_var (uid_identifier_in_variable_in_match_in_antimatch_branch_in_conditional, x_subject)),
                                                rest_branches))))
      []
      [
        (uid_conditional, Match_to_conditional (uid_conditional, uid_match));
        (uid_variable_in_conditional, Match_to_variable_in_conditional (uid_variable_in_conditional, uid_match, uid_variable_in_match));
        (uid_identifier_in_variable_in_conditional, Match_to_identifier_in_variable_in_conditional (uid_identifier_in_variable_in_conditional, uid_match, uid_identifier_in_variable_in_match));
        (uid_match_branch_in_conditional, Match_to_match_branch_in_conditional (uid_match_branch_in_conditional, uid_match, uid_match_pair_in_match));
        (uid_identifier_in_match_branch_in_conditional, Match_to_identifier_in_match_branch_in_conditional (uid_identifier_in_match_branch_in_conditional, uid_match));
        (uid_antimatch_branch_in_conditional, Match_to_antimatch_branch_in_conditional (uid_antimatch_branch_in_conditional, uid_match));
        (uid_identifier_in_antimatch_branch_in_conditional, Match_to_identifier_in_antimatch_branch_in_conditional (uid_identifier_in_antimatch_branch_in_conditional, uid_match));
        (uid_match_in_antimatch_branch_in_conditional, Match_to_match_in_antimatch_branch_in_conditional (uid_match_in_antimatch_branch_in_conditional, uid_match));
        (uid_variable_in_match_in_antimatch_branch_in_conditional, Match_to_variable_in_match_in_antimatch_branch_in_conditional (uid_variable_in_match_in_antimatch_branch_in_conditional, uid_match, uid_variable_in_match));
        (uid_identifier_in_variable_in_match_in_antimatch_branch_in_conditional, Match_to_identifier_in_variable_in_match_in_antimatch_branch_in_conditional (uid_identifier_in_variable_in_match_in_antimatch_branch_in_conditional, uid_match, uid_identifier_in_variable_in_match));]

  | Egg_ast.Match_expr(uid_match, e_subject, branches) ->
    let uid_let = next_uid () in
    let uid_identifier_in_let = next_uid () in
    let uid_match_in_let = next_uid () in
    let uid_variable_in_match_in_let = next_uid () in
    let uid_identifier_in_variable_in_match_in_let = next_uid () in
    let x_subject = egg_fresh_var () in
    expression_continuation_with_mappings tc
      (Egg_ast.Let_expr (uid_let, Egg_ast.Egg_var (uid_identifier_in_let, x_subject), e_subject,
                         Egg_ast.Match_expr (
                           uid_match_in_let, Egg_ast.Var_expr (uid_variable_in_match_in_let, Egg_ast.Egg_var (uid_identifier_in_variable_in_match_in_let, x_subject)),
                           branches)))
      []
      [(uid_let, Match_to_let (uid_let, uid_match));
       (uid_identifier_in_let, Match_to_identifier_in_let (uid_identifier_in_let, uid_match));
       (uid_match_in_let, Match_to_match_in_let (uid_match_in_let, uid_match));
       (uid_variable_in_match_in_let, Match_to_variable_in_match_in_let (uid_variable_in_match_in_let, uid_match));
       (uid_identifier_in_variable_in_match_in_let, Match_to_identifier_in_variable_in_match_in_let (uid_identifier_in_variable_in_match_in_let, uid_match));]

  | _ -> tc.continuation_expression_translator e
;;

let translate_ifthenelse
    (tc:translator_configuration)
    (e:Egg_ast.expr) =

  match e with
  | Egg_ast.If_expr(uid_if, e_condition, e_then, e_else) ->
    let uid_match = next_uid () in
    let uid_true_branch_in_match = next_uid () in
    let uid_false_branch_in_match = next_uid () in
    let uid_pattern_in_true_branch_in_match = next_uid () in
    let uid_pattern_in_false_branch_in_match = next_uid () in
    expression_continuation_with_mappings tc
      (Egg_ast.Match_expr (
          uid_match, e_condition,
          [Egg_ast.Match_pair (uid_true_branch_in_match, Egg_ast.Bool_pattern (uid_pattern_in_true_branch_in_match, true), e_then);
           Egg_ast.Match_pair (uid_false_branch_in_match, Egg_ast.Bool_pattern (uid_pattern_in_false_branch_in_match, false), e_else);]))
      []
      [(uid_match, If_to_match (uid_match, uid_if));
       (uid_true_branch_in_match, If_to_true_branch_in_match (uid_true_branch_in_match, uid_if));
       (uid_false_branch_in_match, If_to_false_branch_in_match (uid_false_branch_in_match, uid_if));
       (uid_pattern_in_true_branch_in_match, If_to_pattern_in_true_branch_in_match (uid_pattern_in_true_branch_in_match, uid_if));
       (uid_pattern_in_false_branch_in_match, If_to_pattern_in_false_branch_in_match (uid_pattern_in_false_branch_in_match, uid_if));]

  | _ -> tc.continuation_expression_translator e
;;

let pattern_translators_depending_on_pattern_variable : Egg_ast.pattern translator_fragment list =
  [translate_pattern_list;translate_pattern_cons;]
;;

let translate_all_patterns_depending_on_pattern_variable
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  let (pattern_translator_depending_on_pattern_variable, _) =
    translation_close identity_expression_translator_fragment
      (pattern_translator_compose_many pattern_translators_depending_on_pattern_variable)
  in
  let (p_trans, map_p) = pattern_translator_depending_on_pattern_variable e in
  expression_continuation_with_mappings tc p_trans [map_p] []
;;
let expression_translators : Egg_ast.expr translator_fragment list =
  [ translate_ifthenelse;
    translate_match;
    translate_expression_list;
    translate_expression_cons;
    translate_all_patterns_depending_on_pattern_variable;
    translate_conditional_with_pattern_variable;
  ]
;;

let pattern_translators : Egg_ast.pattern translator_fragment list =
  [ identity_pattern_translator_fragment; ]
;;
