open Batteries;;
(* open Jhupllib; *)

open On_ast;;
open Translator_utils;;

open TranslationMonad;;

(* This function encodes all list-related patterns with record patterns *)
let rec encode_list_pattern (pat : pattern) : pattern =
  match pat with
  | AnyPat | IntPat | TruePat | FalsePat | FunPat | StringPat | VarPat _ ->
    pat
  | RecPat (rec_map) ->
    RecPat (Ident_map.map (fun pat -> encode_list_pattern pat) rec_map)
  | VariantPat v_content ->
    let Variant (v_label, v_pat) = v_content in
    let new_v_content = Variant (v_label, encode_list_pattern v_pat) in
    VariantPat new_v_content
  | EmptyLstPat ->
    (* The empty list is encoded as {~empty = {}} *)
    let empty_rec =
      Ident_map.add (Ident "~empty") (RecPat (Ident_map.empty)) Ident_map.empty
    in
    RecPat (empty_rec)
  | LstDestructPat (hd_pat, tl_pat) ->
    let clean_hd_pat = encode_list_pattern hd_pat in
    let clean_tl_pat = encode_list_pattern tl_pat in
    let pat_rec_with_head =
      Ident_map.add (Ident "~head") (clean_hd_pat) (Ident_map.empty)
    in
    let pat_rec_with_tail =
      Ident_map.add (Ident "~tail") (clean_tl_pat) (pat_rec_with_head)
    in
    RecPat (pat_rec_with_tail)
;;

(* This function transforms all lists in the expression to records. *)
let rec list_transform (e : expr) : (expr) =
  match e with
  | Var _ | Int _ | Bool _ | String _ -> e
  | Function (param_list, e') ->
    let new_e' = list_transform e' in
    Function (param_list, new_e')
  | Appl (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Appl (new_e1, new_e2)
  | Let (id, e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Let (id, new_e1, new_e2)
  | LetRecFun _ ->
    raise @@
    Failure "encode_variant: LetRecFun should have been desugared by now"
  | LetFun (f_sig, outer_e) ->
    let Funsig(f_name, param_list, inner_e) = f_sig in
    let new_inner_e = list_transform inner_e in
    let new_outer_e = list_transform outer_e in
    let new_funsig = Funsig(f_name, param_list, new_inner_e) in
    LetFun(new_funsig, new_outer_e)
  | Plus (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Plus (new_e1, new_e2)
  | Minus (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Minus (new_e1, new_e2)
  | Equal (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Equal (new_e1, new_e2)
  | LessThan (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    LessThan (new_e1, new_e2)
  | Leq (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Leq (new_e1, new_e2)
  | And (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    And (new_e1, new_e2)
  | Or (e1, e2) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    Or (new_e1, new_e2)
  | Not (e') ->
    let new_e' = list_transform e' in
    Not (new_e')
  | If (e1, e2, e3) ->
    let new_e1 = list_transform e1 in
    let new_e2 = list_transform e2 in
    let new_e3 = list_transform e3 in
    If (new_e1, new_e2, new_e3)
  | Record (rec_map) ->
    let new_map = Ident_map.map
        (fun map_e -> list_transform map_e) rec_map
    in
    Record (new_map)
  | RecordProj (rec_e, lab) ->
    let new_rec_e = list_transform rec_e in
    RecordProj (new_rec_e, lab)
  | VariantExpr (v_name, v_expr) ->
    VariantExpr (v_name, list_transform v_expr)
  | Match (match_e, pat_expr_list) ->
    let new_match_e = list_transform match_e in
    (* routine to pass into List.map... *)
    let pat_expr_list_changer pat_expr_tuple =
      let (curr_pat, curr_expr) = pat_expr_tuple in
      let new_pat = encode_list_pattern curr_pat in
      let new_expr = list_transform curr_expr in
      (new_pat, new_expr)
    in
    let new_pat_expr_list = List.map pat_expr_list_changer pat_expr_list in
    Match (new_match_e, new_pat_expr_list)
  | List (expr_list) ->
    let list_maker = fun element -> fun acc ->
      let clean_elm = list_transform element in
      let new_map_init =
        Ident_map.add (Ident "~head") (clean_elm) Ident_map.empty in
      let new_map_final =
        Ident_map.add (Ident "~tail") (acc) new_map_init in
      Record (new_map_final)
    in
    let empty_rec =
      Record (Ident_map.add
                (Ident "~empty")
                (Record (Ident_map.empty))
                Ident_map.empty)
    in
    let record_equivalent = List.fold_right list_maker expr_list empty_rec
    in
    record_equivalent
  | ListCons (hd_expr, tl_expr) ->
    let clean_hd_expr = list_transform hd_expr in
    let clean_tl_expr = list_transform tl_expr in
    let rec_with_hd = Ident_map.add (Ident "~head") (clean_hd_expr) (Ident_map.empty) in
    let rec_with_tl = Ident_map.add (Ident "~tail") (clean_tl_expr) (rec_with_hd) in
    Record (rec_with_tl)
;;

(* This function takes a Variant expression and converts it into a
   Record expression. *)
let rec variant_expr_to_record (e : expr) : (expr) =
  match e with
  | VariantExpr (v_label, v_expr) ->
    let Variant_label (v_name) = v_label in
    let variant_ident = Ident ("~variant_" ^ v_name) in
    let empty_rec = Record (Ident_map.empty) in
    let map_with_label =
      Ident_map.add variant_ident empty_rec Ident_map.empty
    in
    let encoded_v_expr = encode_variant v_expr in
    let res_map =
      Ident_map.add (Ident "~value") encoded_v_expr map_with_label
    in
    let res_record = Record (res_map) in
    res_record
  | _ ->
    raise @@ Failure
      "variant_expr_to_record: should only be called on an VariantExpr"

(* This function takes a Variant pattern and converts it into a
   Record pattern. *)
and variant_pattern_to_record (p : pattern) : pattern =
  match p with
  | VariantPat (v_content) ->
    let Variant(v_label, pat) = v_content in
    let Variant_label (v_name) = v_label in
    let variant_ident = Ident ("~variant_" ^ v_name) in
    let empty_rec = RecPat (Ident_map.empty) in
    let map_with_label =
      Ident_map.add variant_ident empty_rec Ident_map.empty
    in
    let encoded_v_expr = encode_variant_pattern pat in
    let res_map =
      Ident_map.add (Ident "~value") encoded_v_expr map_with_label
    in
    let res_record = RecPat (res_map) in
    res_record
  | _ ->
    raise @@ Failure
      "variant_pattern_to_record: should only be called on an VariantPat"

(* Function that takes a pattern and converts all of the Variant patterns
   within it to Record patterns. *)
and encode_variant_pattern (p : pattern) : pattern =
  match p with
  | AnyPat | IntPat | TruePat | FalsePat | FunPat | StringPat | VarPat _ ->
    p
  | RecPat (rec_map) ->
    RecPat (Ident_map.map (fun pat -> encode_variant_pattern pat) rec_map)
  | VariantPat _ -> variant_pattern_to_record p
  | EmptyLstPat | LstDestructPat _ ->
    raise @@ Failure "encode_variant: list patterns should be transformed by now"

(* Overall Function that takes an expression and converts all of the Variant
   expressions and patterns within it to Record expressions and patterns. *)
and encode_variant (e : expr) : expr =
  match e with
  | Var _ | Int _ | Bool _ | String _ -> e
  | Function (param_list, e') ->
    let new_e' = encode_variant e' in
    Function (param_list, new_e')
  | Appl (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Appl (new_e1, new_e2)
  | Let (id, e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Let (id, new_e1, new_e2)
  | LetRecFun _ ->
    raise @@
    Failure "encode_variant: LetRecFun should have been desugared by now"
  | LetFun (f_sig, outer_e) ->
    let Funsig(f_name, param_list, inner_e) = f_sig in
    let new_inner_e = encode_variant inner_e in
    let new_outer_e = encode_variant outer_e in
    let new_funsig = Funsig(f_name, param_list, new_inner_e) in
    LetFun(new_funsig, new_outer_e)
  | Plus (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Plus (new_e1, new_e2)
  | Minus (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Minus (new_e1, new_e2)
  | Equal (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Equal (new_e1, new_e2)
  | LessThan (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    LessThan (new_e1, new_e2)
  | Leq (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Leq (new_e1, new_e2)
  | And (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    And (new_e1, new_e2)
  | Or (e1, e2) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    Or (new_e1, new_e2)
  | Not (e') ->
    let new_e' = encode_variant e' in
    Not (new_e')
  | If (e1, e2, e3) ->
    let new_e1 = encode_variant e1 in
    let new_e2 = encode_variant e2 in
    let new_e3 = encode_variant e3 in
    If (new_e1, new_e2, new_e3)
  | Record (rec_map) ->
    let new_map = Ident_map.map
        (fun map_e -> encode_variant map_e) rec_map
    in
    Record (new_map)
  | RecordProj (rec_e, lab) ->
    let new_rec_e = encode_variant rec_e in
    RecordProj (new_rec_e, lab)
  | VariantExpr (_, _) ->
    variant_expr_to_record e
  | Match (match_e, pat_expr_list) ->
    let new_match_e = encode_variant match_e in
    (* routine to pass into List.map to edit all of the pattern/expression
       tuples. *)
    let pat_expr_list_changer pat_expr_tuple =
      let (curr_pat, curr_expr) = pat_expr_tuple in
      let new_pat = encode_variant_pattern curr_pat in
      let new_expr = encode_variant curr_expr in
      (new_pat, new_expr)
    in
    let new_pat_expr_list = List.map pat_expr_list_changer pat_expr_list in
    Match (new_match_e, new_pat_expr_list)
  | List _ | ListCons _ ->
    raise @@
    Failure "encode_variant: Lists should have been transformed at this point"
;;

(* Function that calculates and stores information about how to reach a certain
   variable pattern, and reconstructs the pattern to replace variable patterns
   with "any" patterns.

   Parameters:
    proj_subj: The expression needed to reach the current pattern
               (from a certain record that is in the bottommost layer of the
                expression.)
   pat:        The current pattern

   Return value:
   (pattern * ((ident * expr) list)) :
      The first portion of the tuple is the newly constructed pattern without
      variable patterns.
      The second portion of the tuple contains a tuple dictating a certain
      variable pattern, and the expression needed to access the variable
      (the expression will be used as a stand-in for the variable later.)
*)
let rec encode_var_pat
    (proj_subj : expr)
    (pat : pattern)
  : (pattern * ((ident * expr) list))
  =
  match pat with
  | AnyPat | IntPat | TruePat | FalsePat | FunPat | StringPat ->
    (pat, [])
  | RecPat (pat_map) ->
    (* This routine accumulates the new map (that does not have any variable
       patterns), and the return list. *)
    let (res_pat_map, res_path_list) = Ident_map.fold
        (fun key -> fun pat -> fun acc ->
           let (old_pat_map, old_path_list) = acc in
           let (Ident key_str) = key in
           let cur_label = Label key_str in
           let (new_pat, res_list) = (encode_var_pat (RecordProj(proj_subj, cur_label)) pat)
           in
           let new_pat_map = Ident_map.add key new_pat old_pat_map in
           (new_pat_map, old_path_list @ res_list)
        ) pat_map (Ident_map.empty, [])
    in (RecPat(res_pat_map), res_path_list)
  | VariantPat (_) ->
    raise
    @@ Failure "encode_var_pat : Variant pattern should have been desugared"
  | VarPat (id) ->
    (AnyPat, [(id, proj_subj)])
  | EmptyLstPat | LstDestructPat _ ->
    raise @@ Failure "encode_var_pat: List patterns should have been transformed"
;;

(* Sub-routine that replaces all of the vars that are in the map. *)
(* NOTE: Please don't ask why we wrote this. *)
(* let rec var_replacer
    (e : expr)
    (p_map : expr Ident_map.t)
   : expr =
   match e with
   | Var (id) ->
    (* HERE IS THE BASE CASE *)
    if (Ident_map.mem id p_map) then
      Ident_map.find id p_map
    else e
   | Function (id_list, e') ->
    let replaced_e' = var_replacer e' p_map in
    Function (id_list, replaced_e')
   | Appl (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Appl (replaced_e1, replaced_e2)
   | Let (id, e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Let (id, replaced_e1, replaced_e2)
   | LetRecFun (_) ->
    raise @@ Failure "LetRecFun should have been desugared by now"
   | LetFun (f_sig, outer_expr) ->
    let Funsig(name, param_list, f_expr) = f_sig in
    let replaced_f_expr = var_replacer f_expr p_map in
    let replaced_outer_expr = var_replacer outer_expr p_map in
    LetFun(Funsig(name, param_list, replaced_f_expr), replaced_outer_expr)
   | Plus (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Plus (replaced_e1, replaced_e2)
   | Minus (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Minus (replaced_e1, replaced_e2)
   | Equal (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Equal (replaced_e1, replaced_e2)
   | LessThan (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    LessThan (replaced_e1, replaced_e2)
   | Leq (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Leq (replaced_e1, replaced_e2)
   | And (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    And (replaced_e1, replaced_e2)
   | Or (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Or (replaced_e1, replaced_e2)
   | Not (e') ->
    let replaced_e' = var_replacer e' p_map in
    Not (replaced_e')
   | If (e1, e2, e3) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    let replaced_e3 = var_replacer e3 p_map in
    If (replaced_e1, replaced_e2, replaced_e3)
   | Record (recmap) ->
    let new_recmap = Ident_map.map (fun expr ->
        var_replacer expr p_map) recmap
    in
    Record (new_recmap)
   | RecordProj (e', lab) ->
    let replaced_e' = var_replacer e' p_map in
    RecordProj(replaced_e', lab)
   | Match (e1, p_e_list) ->
    let replaced_e1 = var_replacer e1 p_map in
    let new_p_e_list =
      List.map (fun curr_p_e_pair ->
          let (curr_pat, curr_expr) = curr_p_e_pair in
          let new_expr = var_replacer curr_expr p_map in
          (curr_pat, new_expr)
        ) p_e_list
    in
    Match (replaced_e1, new_p_e_list)
   | Int _ | Bool _ | String _ -> e
   ;; *)


(* Function that finds match statements and go into the patterns to replace
   any variable pattern.
*)
let rec eliminate_var_pat (e : expr): expr m =
  match e with
  | Int _ | Bool _ | String _ | Var _ -> return @@ e
  | Function (id_list, expr) ->
    let%bind clean_expr = eliminate_var_pat expr in
    return @@ Function (id_list, clean_expr)
  | Appl (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Appl (clean_e1, clean_e2)
  | Let (id, e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Let (id, clean_e1, clean_e2)
  | LetRecFun (_, _) ->
    raise
    @@ Failure "rec functions should have been taken care of"
  | LetFun (f_sig, expr) ->
    let (Funsig (f_name, param_list, fun_e)) = f_sig in
    let%bind clean_fun_e = eliminate_var_pat fun_e in
    let%bind clean_expr = eliminate_var_pat expr in
    let new_funsig = Funsig (f_name, param_list, clean_fun_e) in
    return @@ LetFun (new_funsig, clean_expr)
  | Plus (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Plus (clean_e1, clean_e2)
  | Minus (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Minus (clean_e1, clean_e2)
  | Equal (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Equal (clean_e1, clean_e2)
  | LessThan (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ LessThan (clean_e1, clean_e2)
  | Leq (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Leq (clean_e1, clean_e2)
  | And (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ And (clean_e1, clean_e2)
  | Or (e1, e2) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    return @@ Or (clean_e1, clean_e2)
  | Not (expr) ->
    let%bind clean_expr = eliminate_var_pat expr in
    return @@ Not (clean_expr)
  | If (e1, e2, e3) ->
    let%bind clean_e1 = eliminate_var_pat e1 in
    let%bind clean_e2 = eliminate_var_pat e2 in
    let%bind clean_e3 = eliminate_var_pat e3 in
    return @@ If (clean_e1, clean_e2, clean_e3)
  | Record (r_map) ->
    let%bind clean_map =
      r_map
      |> Ident_map.enum
      |> Enum.map
        (fun (k,v) -> let%bind v' = eliminate_var_pat v in return (k,v'))
      |> List.of_enum
      |> sequence
      |> lift1 List.enum
      |> lift1 Ident_map.of_enum
    in
    return @@ Record (clean_map)
  | RecordProj (expr, lab) ->
    let%bind clean_expr = eliminate_var_pat expr in
    return @@ RecordProj (clean_expr, lab)
  | Match (subject, pat_expr_list) ->
    let%bind new_subj_name = fresh_name "match_subject" in
    let new_subj = Var(Ident new_subj_name) in
    let%bind clean_subject = eliminate_var_pat subject in
    (* routine to pass into List.map with the pat_expr_list *)
    let pat_expr_var_changer curr =
      let (curr_pat, curr_expr) = curr in
      (* NOTE: here we clean out the inner expression before we
         erase variable patterns. *)
      let%bind half_clean_expr = eliminate_var_pat curr_expr in
      let (res_pat, path_list) = encode_var_pat new_subj curr_pat in
      let path_enum = List.enum path_list in
      let path_map = Ident_map.of_enum path_enum in
      let new_expr =
        Ident_map.fold (fun curr_id -> fun curr_path ->
            fun acc_e -> Let (curr_id, curr_path, acc_e) )
          path_map half_clean_expr
      in
      return (res_pat, new_expr)
    in
    let%bind new_path_expr_list =
      sequence @@ List.map pat_expr_var_changer pat_expr_list
    in
    let let_e2 = Match(new_subj, new_path_expr_list) in
    return @@ Let(Ident new_subj_name, clean_subject, let_e2)
  | VariantExpr (_, _) ->
    raise @@ Failure
      "eliminate_var_pat: VariantExpr expressions should have been desugared."
  | List _ | ListCons _ ->
    raise @@ Failure
      "eliminate_var_pat: List expressions should have been handled!"
;;
