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

type translation_result =
  Egg_ast.expr * log_entry Uid_map.t
;;

type translator = Egg_ast.expr -> translation_result
;;

type translator_configuration =
  { continuation_translator : translator;
    top_level_translator : translator;
  }
;;

type translator_fragment =
  translator_configuration -> translator
;;

let translator_compose
    (t1:translator_fragment)
    (t2:translator_fragment)
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  t1 { tc with continuation_translator = (t2 tc) } e
;;

let rec translator_compose_many ts =
  List.reduce translator_compose ts
;;

let translation_close (t:translator_fragment) : translator =
  let rec top_t e = t {continuation_translator = do_trans; top_level_translator = top_t} e
  and do_trans (e:Egg_ast.expr) =
    match e with
    | Egg_ast.Record_expr(uid,fields) ->
      let (trans_fields, unioned_map) =
        fields
        |> Ident_map.enum
        |> Enum.fold (
          fun (trans_fields, unioned_map) (ident, value_expr) ->
            let (trans_expr, map_expr) = top_t value_expr in
            (Ident_map.add ident trans_expr trans_fields, disjoint_union unioned_map map_expr)
        ) (Ident_map.empty, Uid_map.empty)
      in
      (Egg_ast.Record_expr(uid, trans_fields), unioned_map)
    | Egg_ast.Function_expr(uid_e,Egg_ast.Function (uid_f, x,e')) ->
      let (trans_e, map_e) =
        top_t e'
      in
      (Egg_ast.Function_expr(uid_e,Egg_ast.Function (uid_f, x,trans_e)), map_e)
    | Egg_ast.Int_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Bool_expr _ -> (e, Uid_map.empty)
    | Egg_ast.String_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Ref_expr(uid, e') ->
      let (trans_e, map_e) =
        top_t e'
      in
      (Egg_ast.Ref_expr(uid, trans_e), map_e)
    | Egg_ast.Var_expr _ -> (e, Uid_map.empty)
    | Egg_ast.Appl_expr (uid, e1, e2) ->
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Appl_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Conditional_expr(uid, e, p, fv1, fv2) ->
      let (trans_e, map_e) = top_t e in
      (Egg_ast.Conditional_expr(uid, trans_e, p, fv1, fv2), map_e)
    | Egg_ast.If_expr(uid, e, e1, e2) ->
      let (trans_e, map_e) = top_t e
      in
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_unions [map_e;map_e1;map_e2]
      in (Egg_ast.If_expr(uid, trans_e, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Deref_expr(uid, e) ->
      let (trans_e, map_e) = top_t e in
      (Egg_ast.Deref_expr(uid, trans_e), map_e)
    | Egg_ast.Update_expr(uid, e1, e2) ->
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Update_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Binary_operation_expr(uid, e1, op, e2) ->
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Binary_operation_expr(uid, trans_e1, op, trans_e2), unioned_map)
    | Egg_ast.Unary_operation_expr(uid, op, e) ->
      let (trans_e, map_e) = top_t e in
      (Egg_ast.Unary_operation_expr(uid, op, trans_e), map_e)
    | Egg_ast.Indexing_expr(uid, e1, e2) ->
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Indexing_expr(uid, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Let_expr(uid, x, e1, e2) ->
      let (trans_e1, map_e1) = top_t e1
      in
      let (trans_e2, map_e2) = top_t e2
      in
      let unioned_map = disjoint_union map_e1 map_e2 in
      (Egg_ast.Let_expr(uid, x, trans_e1, trans_e2), unioned_map)
    | Egg_ast.Projection_expr(uid, e, i) ->
      let (trans_e, map_e) = top_t e in
      (Egg_ast.Projection_expr(uid, trans_e, i), map_e)
    | Egg_ast.Match_expr(uid, e, ms) ->
      let (trans_e, map_e) = top_t e in
      (Egg_ast.Match_expr(uid, trans_e, ms), map_e)
  in
  top_t
;;

let rec translate_ifthenelse
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.If_expr(uid, cond, f1, f2) ->
    let var1 = egg_fresh_var () in
    let var2 = egg_fresh_var () in
    let (cond_trans, cond_map) = tc.top_level_translator cond in
    let (f1_trans, f1_map) = tc.top_level_translator f1 in
    let (f2_trans, f2_map) = tc.top_level_translator f2 in
    let new_uid = next_uid () in
    let f1_uid = next_uid () in
    let f2_uid = next_uid () in
    let desugared_expr =
      Egg_ast.Conditional_expr(
        new_uid,
        cond_trans,
        Egg_ast.Bool_pattern(next_uid (), true),
        Egg_ast.Function(f1_uid, var1, f1_trans),
        Egg_ast.Function(f2_uid, var2, f2_trans)
      )
    in
    let new_map =
      Uid_map.of_enum @@ List.enum @@
      [ (new_uid, If_to_conditional(new_uid,uid))
      ; (f1_uid, If_true_branch_to_function(f1_uid, uid))
      ; (f2_uid, If_false_branch_to_function(f2_uid, uid))
      ]
    in
    let final_map =
      disjoint_unions [new_map; cond_map; f1_map; f2_map]
    in
    lazy_logger `trace (fun () ->
        Printf.sprintf "Translation of if rule sends \n %s \n to \n %s \n with uid map \n %s"
          (Pp_utils.pp_to_string Egg_ast.pp_expr e)
          (Pp_utils.pp_to_string Egg_ast.pp_expr desugared_expr)
          (Pp_utils.pp_to_string
             (Pp_utils.pp_map Uid.pp_uid pp_log_entry Uid_map.enum)
              final_map )
      );
    (desugared_expr, final_map)
  | _ -> tc.continuation_translator e
;;

let rec translate_match
    (tc:translator_configuration)
    (e:Egg_ast.expr) =
  match e with
  | Egg_ast.Match_expr(uid, e, ms) ->
    let x = egg_fresh_var () in
    let (trans_e, map_e) = tc.top_level_translator e in
    let rec desugar_matches ms =
      let nu1 = next_uid () in
      let nu2 = next_uid () in
      match ms with
      | (Egg_ast.Match_pair(mu,p,e') as m)::ms' ->
        let (trans_e', map_e') = tc.top_level_translator e' in
        let (desugared_expr, desugared_map) = desugar_matches ms' in
        let f1 = Egg_ast.Function(nu1, egg_fresh_var (), trans_e') in
        let f2 = Egg_ast.Function(nu2, egg_fresh_var (), desugared_expr) in
        lazy_logger `trace (fun () ->
            Printf.sprintf "Translated match pair \n %s \n into \n %s \n and \n %s \n"
              (Pp_utils.pp_to_string Egg_ast.pp_match_pair m)
              (Pp_utils.pp_to_string Egg_ast.pp_function_value f1)
              (Pp_utils.pp_to_string Egg_ast.pp_function_value f2)
          );
        (Egg_ast.Conditional_expr(mu,trans_e,p,f1,f2),
         (disjoint_unions [desugared_map; map_e; map_e'
                          ; Uid_map.singleton mu (Match_branch(mu, uid))]))
      | [] ->
        let appl_u = next_uid () in
        let this_map = (Uid_map.singleton appl_u (Inexhaustive_match_branch(appl_u,uid))) in
        (Egg_ast.Appl_expr(appl_u,(Egg_ast.Record_expr(nu1,Ident_map.empty)), (Egg_ast.Record_expr(nu2,Ident_map.empty))),
         this_map)
    in let (e', map) = desugar_matches ms in
    (Egg_ast.Let_expr(uid, x, trans_e, e'), map)
  | _ -> tc.continuation_translator e
;;

let translators : translator_fragment list =
  [ translate_ifthenelse;
    translate_match;
  ]
;;
