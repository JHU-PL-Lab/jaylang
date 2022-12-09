open Batteries
open Jhupllib
open Jay
open Jay_ast
open Jay_to_jayil_monad
open TranslationMonad

let lazy_logger = Logger_utils.make_lazy_logger "Jay_to_jayil_preliminary"

let _lbl_m name =
  let%bind freshness = freshness_string in
  return @@ Ident (freshness ^ name)

let lbl_empty_m : Ident.t m = _lbl_m "empty"
let lbl_head_m : Ident.t m = _lbl_m "head"
let lbl_cons_m : Ident.t m = _lbl_m "cons"
let lbl_tail_m : Ident.t m = _lbl_m "tail"
let lbl_variant_m (s : string) : Ident.t m = _lbl_m ("variant_" ^ s)
let lbl_value_m : Ident.t m = _lbl_m "value"

let list_expr_to_record recurse (expr_lst : expr_desc list) =
  (* Record labels *)
  let%bind lbl_empty = lbl_empty_m in
  let%bind lbl_head = lbl_head_m in
  let%bind lbl_tail = lbl_tail_m in
  (* Add appropriate types *)
  let empty_list_lbls = Ident_set.singleton lbl_empty in
  let nonempty_list_lbls =
    Ident_set.empty |> Ident_set.add lbl_head |> Ident_set.add lbl_tail
  in
  let%bind () = add_jay_type_mapping empty_list_lbls Jay_ast.ListType in
  let%bind () = add_jay_type_mapping nonempty_list_lbls Jay_ast.ListType in
  (* Make record *)
  let list_maker element acc =
    let%bind clean_elm = recurse element in
    let new_map =
      Ident_map.empty
      |> Ident_map.add lbl_head clean_elm
      |> Ident_map.add lbl_tail acc
    in
    return @@ new_expr_desc @@ Record new_map
  in
  let empty_rec =
    Record
      (Ident_map.add lbl_empty
         (new_expr_desc @@ Record Ident_map.empty)
         Ident_map.empty)
  in
  let%bind record_equivalent =
    list_fold_right_m list_maker expr_lst (new_expr_desc empty_rec)
  in
  let ret = new_expr_desc @@ record_equivalent.body in
  return ret

(* Here  we "cons" the expression with the list during jay-to-jayil translation.
     Simple, but can introduce pitfalls such as:
     - How do we know if what we are consing to is not a list? How do we typecheck?
     - What if we wish to lazily cons, eg. as part of a freeze Fun x -> x :: [y]
   The latter question should be a non-issue due to the encoding, however. - KQ
*)
let list_cons_expr_to_record recurse (expr : expr_desc) (list_expr : expr_desc)
    =
  (* Record labels *)
  (* Note: We need to add extra cons label to distinguish list cons from regular
     lists *)
  let%bind lbl_head = lbl_head_m in
  let%bind lbl_cons = lbl_cons_m in
  let%bind lbl_tail = lbl_tail_m in
  (* Add appropriate types *)
  let lst_lbls =
    Ident_set.empty |> Ident_set.add lbl_head |> Ident_set.add lbl_cons
    |> Ident_set.add lbl_tail
  in
  let%bind () = add_jay_type_mapping lst_lbls Jay_ast.ListType in
  (* Recurse over inner expr *)
  let%bind clean_expr = recurse expr in
  let%bind record_list = recurse list_expr in
  (* Make record *)
  let new_map =
    Ident_map.empty
    |> Ident_map.add lbl_head clean_expr
    |> Ident_map.add lbl_tail record_list
    |> Ident_map.add lbl_cons (new_expr_desc @@ Record Ident_map.empty)
  in
  return @@ new_expr_desc @@ Record new_map

(* This function takes a Variant expression and converts it into a
   Record expression. *)
let variant_expr_to_record recurse (v_label : variant_label)
    (v_expr : expr_desc) : expr_desc m =
  (* Record labels *)
  let (Variant_label v_name) = v_label in
  let%bind lbl_variant = lbl_variant_m v_name in
  let%bind lbl_value = lbl_value_m in
  (* Add appropriate types *)
  let variant_lbl_set =
    Ident_set.empty |> Ident_set.add lbl_variant |> Ident_set.add lbl_value
  in
  let variant_typ = Jay_ast.VariantType v_label in
  let%bind () = add_jay_type_mapping variant_lbl_set variant_typ in
  (* Recurse over inner expr *)
  let%bind encoded_v_expr = recurse v_expr in
  (* Make record *)
  let empty_rec = Record Ident_map.empty in
  let map_with_label =
    Ident_map.add lbl_variant (new_expr_desc empty_rec) Ident_map.empty
  in
  let res_map = Ident_map.add lbl_value encoded_v_expr map_with_label in
  let res_record = new_expr_desc @@ Record res_map in
  return res_record

let encode_pattern (pattern : pattern) : pattern m =
  match pattern with
  (* Encode list patterns *)
  | EmptyLstPat ->
      (* The empty list is encoded as {~empty = {}}
         The corresponding pattern is {~empty = None} *)
      let%bind lbl_empty = lbl_empty_m in
      let%bind () =
        add_jay_type_mapping (Ident_set.singleton lbl_empty) Jay_ast.ListType
      in
      let empty_rec = Ident_map.add lbl_empty None Ident_map.empty in
      return @@ RecPat empty_rec
  | LstDestructPat (hd_var, tl_var) ->
      let%bind lbl_head = lbl_head_m in
      let%bind lbl_tail = lbl_tail_m in
      let lst_lbls =
        Ident_set.empty |> Ident_set.add lbl_head |> Ident_set.add lbl_tail
      in
      let%bind () = add_jay_type_mapping lst_lbls Jay_ast.ListType in
      let new_lbls =
        Ident_map.empty
        |> Ident_map.add lbl_head @@ Some hd_var
        |> Ident_map.add lbl_tail @@ Some tl_var
      in
      return @@ RecPat new_lbls
  (* Encode variant patterns *)
  | VariantPat (v_label, v_var) ->
      let (Variant_label v_name) = v_label in
      let%bind variant_lbl = lbl_variant_m v_name in
      let%bind value_lbl = lbl_value_m in
      let variant_lbls =
        Ident_set.empty |> Ident_set.add variant_lbl |> Ident_set.add value_lbl
      in
      let variant_type = Jay_ast.VariantType v_label in
      let%bind () = add_jay_type_mapping variant_lbls variant_type in
      let record =
        Ident_map.empty
        |> Ident_map.add variant_lbl None
        |> Ident_map.add value_lbl (Some v_var)
      in
      return @@ RecPat record
  (* All other patterns: don't encode *)
  | AnyPat | IntPat | BoolPat | FunPat | RecPat _ | StrictRecPat _ | VarPat _ ->
      return pattern

let encode_match_exprs recurse is_instrumented (match_expr : expr_desc)
    (pat_expr_lst : (pattern * expr_desc) list) =
  (* Transform first expression *)
  let%bind new_match_expr = recurse match_expr in
  (* Transform pattern-expression pairs *)
  let pat_expr_list_changer pat_expr_tuple =
    let curr_pat, curr_expr = pat_expr_tuple in
    if is_instrumented
    then
      let%bind new_pat = encode_pattern curr_pat in
      let%bind new_expr = recurse curr_expr in
      return (new_pat, new_expr)
    else
      match curr_pat with
      | AnyPat | IntPat | BoolPat | FunPat | VariantPat _ | VarPat _
      | EmptyLstPat | LstDestructPat _ ->
          let%bind new_pat = encode_pattern curr_pat in
          let%bind new_expr = recurse curr_expr in
          return (new_pat, new_expr)
      | RecPat rec_pat | StrictRecPat rec_pat ->
          (* TODO: Really cheap trick. How can I improve this? Add an abstract label to primitive records that we create *)
          let%bind hd_lbl = lbl_head_m in
          let%bind tl_lbl = lbl_tail_m in
          let%bind cons_lbl = lbl_cons_m in
          let%bind empty_lbl = lbl_empty_m in
          if Ident_map.mem hd_lbl rec_pat
             || Ident_map.mem tl_lbl rec_pat
             || Ident_map.mem cons_lbl rec_pat
             || Ident_map.mem empty_lbl rec_pat
             || Ident_map.mem (Ident "~untouched") rec_pat
          then
            let%bind new_pat = encode_pattern curr_pat in
            let%bind new_expr = recurse curr_expr in
            return (new_pat, new_expr)
          else
            let%bind expr' = recurse curr_expr in
            let%bind r_name = fresh_name "actual_r" in
            let decl_lbls =
              new_expr_desc @@ RecordProj (new_match_expr, Label "~decl_lbls")
            in
            let ret_pat' = Ident_map.map (fun _ -> None) rec_pat in
            let lbl_var_pairs =
              Ident_map.bindings rec_pat
              |> List.filter_map (fun (l, v) ->
                     match v with Some x -> Some (l, x) | None -> None)
            in
            let%bind rebind_vars =
              list_fold_left_m
                (fun acc (Ident l, x) ->
                  let new_proj =
                    new_expr_desc
                    @@ RecordProj (new_expr_desc @@ Var (Ident r_name), Label l)
                  in
                  let%bind () = add_jay_instrumented new_proj.tag in
                  return @@ new_expr_desc @@ Let (x, new_proj, acc))
                expr' lbl_var_pairs
            in
            let legal_match =
              new_expr_desc
              @@ Match (decl_lbls, [ (StrictRecPat ret_pat', rebind_vars) ])
            in
            let%bind () = add_jay_instrumented decl_lbls.tag in
            (* let%bind () = add_jay_instrumented legal_match.tag in *)
            let pat' =
              RecPat
                (Ident_map.singleton (Ident "~actual_rec") (Some (Ident r_name)))
            in
            return @@ (pat', legal_match)
  in
  let%bind new_pat_expr_lst =
    sequence @@ List.map pat_expr_list_changer pat_expr_lst
  in
  (* Return final match expression *)
  return @@ new_expr_desc @@ Match (new_match_expr, new_pat_expr_lst)

(** Transform a let rec expression into one that uses functions. E.g. let rec f
    n = ... in f 10 becomes let f = fun f' a -> let f = f' f' in ... in let f''
    = f f in f'' 10 E.g. let rec f a = ... with g b = ... in f 10 becomes let f
    = fun f' g' a -> let f'' = f' f' g' in let g'' = g' f' g' in ... in let g =
    fun f' g' b -> let f'' = f' f' g' in let g'' = g' f' g' in ... in let f''' =
    f'' f'' g'' in let g''' = g'' f'' g'' in f''' 10 *)
let letrec_expr_to_fun recurse fun_sig_list rec_e_desc =
  (* Translate inner expression *)
  let%bind transformed_rec_expr = recurse rec_e_desc in
  (* Come up with new names for functions *)
  let original_names = List.map (fun (Funsig (id, _, _)) -> id) fun_sig_list in
  let%bind new_names =
    sequence
    @@ List.map
         (fun (Ident old_name) ->
           let%bind new_name = fresh_name old_name in
           return @@ Ident new_name)
         original_names
  in
  let name_pairs = List.combine original_names new_names in
  (* Create repeated function applications, eg. f g h ... *)
  let%bind appls_for_funs =
    list_fold_left_m
      (fun appl_dict base_fun ->
        let original_fun_name, new_fun_name = base_fun in
        let sub_appl =
          list_fold_left_m
            (fun acc fun_name ->
              let ret =
                new_expr_desc @@ Appl (acc, new_expr_desc @@ Var fun_name)
              in
              let%bind () = add_jay_instrumented ret.tag in
              return ret)
            (new_expr_desc @@ Var new_fun_name)
            new_names
        in
        return @@ Ident_map.add original_fun_name sub_appl appl_dict)
      Ident_map.empty name_pairs
  in
  (* Create let fun expressions *)
  let lt_maker_fun fun_name acc =
    let%bind cur_appl_expr = Ident_map.find fun_name appls_for_funs in
    return @@ new_expr_desc @@ Let (fun_name, cur_appl_expr, acc)
  in
  let%bind transformed_outer_expr =
    list_fold_right_m lt_maker_fun original_names transformed_rec_expr
  in
  let sig_name_pairs = List.combine fun_sig_list new_names in
  (* Final expression *)
  let%bind ret_expr =
    list_fold_right_m
      (fun (fun_sig, fun_new_name) acc ->
        let (Funsig (_, param_list, cur_f_expr)) = fun_sig in
        let%bind transformed_cur_f_expr = recurse cur_f_expr in
        let new_param_list = new_names @ param_list in
        let%bind new_fun_expr =
          list_fold_right_m lt_maker_fun original_names transformed_cur_f_expr
        in
        let new_fun = Function (new_param_list, new_fun_expr) in
        return @@ new_expr_desc @@ Let (fun_new_name, new_expr_desc new_fun, acc))
      sig_name_pairs transformed_outer_expr
  in
  let ret = new_expr_desc @@ ret_expr.body in
  return ret

let desugar (e : expr_desc) : expr_desc m =
  let transformer recurse e_desc =
    let tag = e_desc.tag in
    let%bind is_instrumented = is_jay_instrumented tag in
    let expr = e_desc.body in
    match expr with
    | List e_lst ->
        let%bind expr' = list_expr_to_record recurse e_lst in
        let%bind () = add_jay_expr_mapping expr' e_desc in
        return expr'
    | ListCons (e, e_lst) ->
        let%bind expr' = list_cons_expr_to_record recurse e e_lst in
        let%bind () = add_jay_expr_mapping expr' e_desc in
        return expr'
    | VariantExpr (lbl, e') ->
        let%bind expr' = variant_expr_to_record recurse lbl e' in
        let%bind () = add_jay_expr_mapping expr' e_desc in
        return expr'
    | Match (match_e, pat_e_lst) ->
        let%bind expr' =
          encode_match_exprs recurse is_instrumented match_e pat_e_lst
        in
        let%bind () = add_jay_expr_mapping expr' e_desc in
        let%bind () =
          if is_instrumented then add_jay_instrumented expr'.tag else return ()
          (* let () = print_endline "-----------------" in
             let () = print_endline @@ Jay_ast_pp.show_expr_desc e_desc in
             failwith @@ "Not accounted for: tag = " ^ string_of_int tag *)
        in
        let () = print_endline @@ "This is the transformed match" in
        let () =
          print_endline
          @@ (Pp_utils.pp_to_string Jay_ast_pp.pp_expr_desc_without_tag) expr'
        in
        return expr'
    | LetRecFun (fun_sig_list, rec_e) ->
        let%bind expr' = letrec_expr_to_fun recurse fun_sig_list rec_e in
        let%bind () = add_jay_expr_mapping expr' e_desc in
        return expr'
    | _ -> return e_desc
  in
  let%bind ret_ed = m_transform_expr transformer e in
  return ret_ed
