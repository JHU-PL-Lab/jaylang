open Batteries;;
open Jhupllib;;
open Odefa_ast;;

let lazy_logger = Logger_utils.make_lazy_logger "On_to_odefa";;

(** This creates a fresh name counter so we can easily get fresh names during
    the A-normalization process. *)
let _fresh_name_counter = ref 0;;

(** This function generates a fresh name with the provided prefix. *)
let fresh_name (prefix : string) : string =
  let n = !_fresh_name_counter + 1 in
  _fresh_name_counter := n;
  prefix ^ "_" ^ string_of_int n
;;

let ast_var_from_string (name : string) : Ast.var =
  let new_ident = Ast.Ident (fresh_name name) in
  Ast.Var(new_ident, None)
;;

let fun_curry ident acc =
  let On_ast.Ident(id_string) = ident in
  let ast_ident = Ast.Ident(id_string) in
  let (acc_expr, _) = acc in
  let new_var = ast_var_from_string "var~" in
  let new_clause =
    Ast.Clause(new_var,
               Ast.Value_body(Ast.Value_function(
                   Ast.Function_value(Ast.Var(ast_ident, None), acc_expr)
                 ))) in
  (Ast.Expr([new_clause]), new_var)
;;

let rec rec_transform (e1 : On_ast.expr) : (On_ast.expr) =
  match e1 with
  | Var _ | Int _ | Bool _ | Input -> e1
  | Function (id_list, fe) ->
    let transformed_expr = rec_transform fe in Function (id_list, transformed_expr)
  | Appl (apple1, apple2) ->
    let transformed_expr1 = rec_transform apple1 in
    let transformed_expr2 = rec_transform apple2 in
    Appl (transformed_expr1, transformed_expr2)
  | Let (let_id, lete1, lete2) ->
    let transformed_expr1 = rec_transform lete1 in
    let transformed_expr2 = rec_transform lete2 in
    Let (let_id, transformed_expr1, transformed_expr2)
  | LetFun (funsig, fe) ->
    let (Funsig (id, id_list, bodye')) = funsig in
    let transform_bodye = rec_transform bodye' in
    let transformed_fe = rec_transform fe in
    let new_sig = On_ast.Funsig (id, id_list, transform_bodye) in
    LetFun(new_sig, transformed_fe)
  | LetRecFun (fun_sig_list, rece) ->
    let transformed_rece = rec_transform rece in
    let original_names =
      List.map (fun single_sig ->
          let (On_ast.Funsig (id, _, _)) = single_sig
          in id) fun_sig_list
    in
    let new_names =
      List.map (fun (On_ast.Ident old_name) ->
          let new_name = On_ast.Ident (fresh_name (old_name)) in new_name) original_names
    in
    let name_pairs = List.combine original_names new_names in
    let appls_for_funs = List.fold_left (fun appl_dict -> fun base_fun ->
        let (original_fun_name, new_fun_name) = base_fun in
        let sub_appl =
          List.fold_left
            (fun acc -> fun fun_name -> On_ast.Appl(acc, Var(fun_name)))
            (Var(new_fun_name)) new_names in
        On_ast.Ident_map.add original_fun_name sub_appl appl_dict) On_ast.Ident_map.empty name_pairs
    in
    let let_maker_fun = (fun fun_name -> fun acc ->
        let cur_appl_expr = On_ast.Ident_map.find fun_name appls_for_funs in
        On_ast.Let(fun_name, cur_appl_expr, acc))
    in
    let transformed_outer_expr =
      List.fold_right let_maker_fun original_names transformed_rece
    in
    let sig_name_pairs = List.combine fun_sig_list new_names in
    let ret_expr =
      List.fold_right (fun (fun_sig, fun_new_name) -> fun acc ->
          let (On_ast.Funsig (_, param_list, cur_f_expr)) = fun_sig in
          let transformed_cur_f_expr = rec_transform cur_f_expr in
          let new_param_list = new_names @ param_list in
          let new_fun_expr = List.fold_right let_maker_fun original_names transformed_cur_f_expr in
          On_ast.Let(fun_new_name, Function (new_param_list, new_fun_expr), acc)
        ) sig_name_pairs transformed_outer_expr
    in ret_expr
  | Not e ->
    let transformed_expr = rec_transform e in
    Not(transformed_expr)
  | Plus (pe1, pe2) ->
    let transformed_expr1 = rec_transform pe1 in
    let transformed_expr2 = rec_transform pe2 in
    Plus (transformed_expr1, transformed_expr2)
  | Minus (me1, me2) ->
    let transformed_expr1 = rec_transform me1 in
    let transformed_expr2 = rec_transform me2 in
    Minus (transformed_expr1, transformed_expr2)
  | Times (me1, me2) ->
    let transformed_expr1 = rec_transform me1 in
    let transformed_expr2 = rec_transform me2 in
    Times (transformed_expr1, transformed_expr2)
  | Divide (me1, me2) ->
    let transformed_expr1 = rec_transform me1 in
    let transformed_expr2 = rec_transform me2 in
    Divide (transformed_expr1, transformed_expr2)
  | Modulus (me1, me2) ->
    let transformed_expr1 = rec_transform me1 in
    let transformed_expr2 = rec_transform me2 in
    Modulus (transformed_expr1, transformed_expr2)
  | Equal (eqe1, eqe2) ->
    let transformed_expr1 = rec_transform eqe1 in
    let transformed_expr2 = rec_transform eqe2 in
    Equal (transformed_expr1, transformed_expr2)
  | LessThan (lte1, lte2) ->
    let transformed_expr1 = rec_transform lte1 in
    let transformed_expr2 = rec_transform lte2 in
    LessThan (transformed_expr1, transformed_expr2)
  | Leq (leqe1, leqe2) ->
    let transformed_expr1 = rec_transform leqe1 in
    let transformed_expr2 = rec_transform leqe2 in
    Leq (transformed_expr1, transformed_expr2)
  | And (ande1, ande2) ->
    let transformed_expr1 = rec_transform ande1 in
    let transformed_expr2 = rec_transform ande2 in
    And (transformed_expr1, transformed_expr2)
  | Or (ore1, ore2) ->
    let transformed_expr1 = rec_transform ore1 in
    let transformed_expr2 = rec_transform ore2 in
    Or (transformed_expr1, transformed_expr2)
  | If (ife, thene, elsee) ->
    let transformed_expr1 = rec_transform ife in
    let transformed_expr2 = rec_transform thene in
    let transformed_expr3 = rec_transform elsee in
    If (transformed_expr1, transformed_expr2, transformed_expr3)
;;

(* Function that removes duplicate naming so that we adhere to Odefa's naming
   rules. Is called when we detect a duplicate naming.
   NOTE: We should stop when we encounter a LET statement
   (or frankly a function) that rewrites the var???
*)
let rec replace_duplicate_naming
    (e : On_ast.expr)
    (old_name : On_ast.ident)
    (new_name : On_ast.ident)
  : On_ast.expr =
  match e with
  | Int _ | Bool _ | Input -> e
  | Var (id) ->
    if id = old_name then Var(new_name) else Var(id)
  | Function (id_list, e') ->
    if (List.mem old_name id_list) then e
    else
      let new_e' = replace_duplicate_naming e' old_name new_name in
      Function(id_list, new_e')
  | Appl (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Appl(new_e1, new_e2)
  | Let (id, e1, e2) ->
    if id = old_name then e
    else
      let new_e1 = replace_duplicate_naming e1 old_name new_name in
      let new_e2 = replace_duplicate_naming e2 old_name new_name in
      Let(id, new_e1, new_e2)
  | LetRecFun (_, _) ->
    raise (Failure "rec functions should have been taken care of")
  | LetFun (f_sig, e') ->
    let (On_ast.Funsig(id, id_list, fun_e)) = f_sig in
    (* If the old_name is same as the function name, then we don't want
       to change anything. *)
    if id = old_name then e
    else
      (
        (* If the old_name is same as one of the names of the params, then
           we only want to change the code outside of the function.
        *)
        if List.mem old_name id_list then
          (
            let new_e' = replace_duplicate_naming e' old_name new_name in
            LetFun (f_sig, new_e')
          )
        else (* change both the inside and the outside expressions *)
          (
            let new_inner_e = replace_duplicate_naming fun_e old_name new_name in
            let new_outer_e = replace_duplicate_naming e' old_name new_name in
            let new_funsig = On_ast.Funsig(id, id_list, new_inner_e) in
            LetFun(new_funsig, new_outer_e)
          )
      )
  | Plus (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Plus (new_e1, new_e2)
  | Minus (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Minus (new_e1, new_e2)
  | Times (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Times (new_e1, new_e2)
  | Divide (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Divide (new_e1, new_e2)
  | Modulus (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Modulus (new_e1, new_e2)
  | Equal (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Equal (new_e1, new_e2)
  | LessThan (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    LessThan (new_e1, new_e2)
  | Leq (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Leq (new_e1, new_e2)
  | And (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    And (new_e1, new_e2)
  | Or (e1, e2) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    Or (new_e1, new_e2)
  | Not (e') ->
    let new_e' = replace_duplicate_naming e' old_name new_name in
    Not (new_e')
  | If (e1, e2, e3) ->
    let new_e1 = replace_duplicate_naming e1 old_name new_name in
    let new_e2 = replace_duplicate_naming e2 old_name new_name in
    let new_e3 = replace_duplicate_naming e3 old_name new_name in
    If (new_e1, new_e2, new_e3)
;;

let find_replace_on_fun_params = fun curr_id -> fun acc ->
  let (curr_e, curr_id_list, fun_id_list) = acc in
  if List.mem curr_id curr_id_list then
    (
      let On_ast.Ident(name_string) = curr_id in
      let new_name = On_ast.Ident(fresh_name name_string) in
      let new_e = replace_duplicate_naming curr_e curr_id new_name in
      let new_list = new_name :: curr_id_list in
      let new_fun_list = new_name :: fun_id_list in
      (new_e, new_list, new_fun_list)
    )
  else
    (
      let new_list = curr_id :: curr_id_list in
      let new_fun_list = curr_id :: fun_id_list in
      (curr_e, new_list, new_fun_list)
    )
;;

(* A function that finds all of the duplicate naming that happens for a given
   expression, and then calls replace_duplicate_naming to change the latter
   naming.
*)
let rec find_replace_duplicate_naming
    (e : On_ast.expr)
    (ident_list : On_ast.ident list)
  : (On_ast.expr * On_ast.ident list) =
  Logger_utils.lazy_bracket_log
    (lazy_logger `trace)
    (fun () -> "enter")
    (fun _ -> "exit")
  @@ fun () ->
  match e with
  | Int _ | Bool _ | Var _ | Input -> (e , ident_list)
  | Function (id_list, e') ->
    (* we assume that the id_list of this function consists of unique elements *)
    (* TODO: clean up the naming of id_list *)
    let (init_e, init_id_list) = find_replace_duplicate_naming e' ident_list in
    let (final_e, final_id_list, final_fun_id_list) =
      List.fold_right find_replace_on_fun_params id_list (init_e, init_id_list, []) in
    (Function (final_fun_id_list, final_e), final_id_list)
  | Appl (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Appl(new_e1, new_e2), e2_id_list)
  | Let (id, e1, e2) ->
    let (init_e2, init_e2_id_list) =
      find_replace_duplicate_naming e2 ident_list in
    let (init_e1, init_e1_id_list) =
      find_replace_duplicate_naming e1 init_e2_id_list in
    if List.mem id init_e1_id_list then
      (
        let On_ast.Ident(name_string) = id in
        let new_name = On_ast.Ident(fresh_name name_string) in
        (* let new_e1 = replace_duplicate_naming init_e1 id new_name in *)
        let new_e2 = replace_duplicate_naming init_e2 id new_name in
        let new_list = new_name :: init_e1_id_list in
        (Let(new_name, init_e1, new_e2), new_list)
      )
    else
      (
        let new_list = id :: init_e1_id_list in
        (Let(id, init_e1, init_e2), new_list)
      )
  | LetRecFun _ -> raise (Failure "no let rec")
  | LetFun (f_sig, e') ->
    let Funsig(f_name, param_list, f_e) = f_sig in
    let (outer_e, outer_e_list) = find_replace_duplicate_naming e' ident_list in
    let (init_inner_e, init_list) =
      find_replace_duplicate_naming f_e outer_e_list in
    (* taking care of the funsig part*)
    let (final_inner_e, ident_list', param_list') =
      List.fold_right find_replace_on_fun_params param_list (init_inner_e, init_list, []) in
    (* checking if the let part has conflicts *)
    if List.mem f_name ident_list' then
      (
        (* if there are conflicts, we need to change the id in the let binding,
           and accomodate on the outer expression
        *)
        let On_ast.Ident(name_string) = f_name in
        let new_name = On_ast.Ident(fresh_name name_string) in
        let new_outer_e = replace_duplicate_naming outer_e f_name new_name in
        let new_list = new_name :: ident_list' in
        let new_funsig = On_ast.Funsig(new_name, param_list', final_inner_e) in
        (LetFun(new_funsig, new_outer_e), new_list)
      )
    else
      (
        let new_list = f_name :: ident_list' in
        let new_funsig = On_ast.Funsig(f_name, param_list', final_inner_e) in
        (LetFun(new_funsig, outer_e), new_list)
      )
  | Plus (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Plus(new_e1, new_e2), e2_id_list)
  | Minus (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Minus(new_e1, new_e2), e2_id_list)
  | Times (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Times(new_e1, new_e2), e2_id_list)
  | Divide (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Divide(new_e1, new_e2), e2_id_list)
  | Modulus (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Modulus(new_e1, new_e2), e2_id_list)
  | Equal (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Equal(new_e1, new_e2), e2_id_list)
  | LessThan (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (LessThan(new_e1, new_e2), e2_id_list)
  | Leq (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Leq(new_e1, new_e2), e2_id_list)
  | And (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (And(new_e1, new_e2), e2_id_list)
  | Or (e1, e2) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    (Or(new_e1, new_e2), e2_id_list)
  | Not (e') ->
    let (new_e', e1_id_list) = find_replace_duplicate_naming e' ident_list in
    (Not(new_e'), e1_id_list)
  | If (e1, e2, e3) ->
    let (new_e1, e1_id_list) = find_replace_duplicate_naming e1 ident_list in
    let (new_e2, e2_id_list) = find_replace_duplicate_naming e2 e1_id_list in
    let (new_e3, e3_id_list) = find_replace_duplicate_naming e3 e2_id_list in
    (If(new_e1, new_e2, new_e3), e3_id_list)
;;

(* Sub-routine that replaces all of the vars that are in the map. *)
let rec var_replacer
    (e : On_ast.expr)
    (p_map : On_ast.expr On_ast.Ident_map.t)
  : On_ast.expr =
  match e with
  | Int _ | Bool _ | Input -> e
  | Var (id) ->
    if (On_ast.Ident_map.mem id p_map) then
      On_ast.Ident_map.find id p_map
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
  | Not e ->
    let replaced = var_replacer e p_map in
    Not(replaced)
  | Plus (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Plus (replaced_e1, replaced_e2)
  | Minus (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Minus (replaced_e1, replaced_e2)
  | Times (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Times (replaced_e1, replaced_e2)
  | Divide (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Divide (replaced_e1, replaced_e2)
  | Modulus (e1, e2) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    Modulus (replaced_e1, replaced_e2)
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
  | If (e1, e2, e3) ->
    let replaced_e1 = var_replacer e1 p_map in
    let replaced_e2 = var_replacer e2 p_map in
    let replaced_e3 = var_replacer e3 p_map in
    If (replaced_e1, replaced_e2, replaced_e3)
;;


(* This function will find match statements and go into the patterns to replace
   any variable pattern.
*)
let rec eliminate_var_pat (e : On_ast.expr): On_ast.expr =
  match e with
  | Int _ | Bool _ | Var _ | Input -> e
  | Function (id_list, expr) ->
    let clean_expr = eliminate_var_pat expr in
    Function (id_list, clean_expr)
  | Appl (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Appl (clean_e1, clean_e2)
  | Let (id, e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Let (id, clean_e1, clean_e2)
  | LetRecFun (_, _) ->
    raise (Failure "rec functions should have been taken care of")
  | LetFun (f_sig, expr) ->
    let (On_ast.Funsig (f_name, param_list, fun_e)) = f_sig in
    let clean_fun_e = eliminate_var_pat fun_e in
    let clean_expr = eliminate_var_pat expr in
    let new_funsig = On_ast.Funsig (f_name, param_list, clean_fun_e) in
    LetFun (new_funsig, clean_expr)
  | Plus (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Plus (clean_e1, clean_e2)
  | Minus (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Minus (clean_e1, clean_e2)
  | Times(e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Times(clean_e1, clean_e2)
  | Divide(e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Divide(clean_e1, clean_e2)
  | Modulus(e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Modulus(clean_e1, clean_e2)
  | Equal (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Equal (clean_e1, clean_e2)
  | LessThan (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    LessThan (clean_e1, clean_e2)
  | Leq (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Leq (clean_e1, clean_e2)
  | And (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    And (clean_e1, clean_e2)
  | Or (e1, e2) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    Or (clean_e1, clean_e2)
  | Not (expr) ->
    let clean_expr = eliminate_var_pat expr in
    Not (clean_expr)
  | If (e1, e2, e3) ->
    let clean_e1 = eliminate_var_pat e1 in
    let clean_e2 = eliminate_var_pat e2 in
    let clean_e3 = eliminate_var_pat e3 in
    If (clean_e1, clean_e2, clean_e3)
;;

let rec flatten_binop
    (e1 : On_ast.expr)
    (e2 : On_ast.expr)
    (binop : Ast.binary_operator)
  : (Ast.clause list * Ast.var) =
  let (e1_clist, e1_var) = flatten_expr e1 in
  let (e2_clist, e2_var) = flatten_expr e2 in
  let new_var = ast_var_from_string "var~" in
  let new_clause =
    Ast.Clause (
      new_var,
      Ast.Binary_operation_body (e1_var, binop, e2_var)
    )
  in
  (e1_clist @ e2_clist @ [new_clause], new_var)

and

  flatten_expr (e : On_ast.expr) : (Ast.clause list * Ast.var) =
  match e with
  | Var (id) ->
    let Ident(i_string) = id in
    let new_var = ast_var_from_string i_string in
    let return_var = Ast.Var(Ast.Ident(i_string), None) in
    ([Clause(new_var, Var_body(return_var))], new_var)
  | Input ->
    let new_var = ast_var_from_string "~input" in
    ([Clause(new_var, Input_body)], new_var)
  | Function (id_list, e) ->
    let (fun_c_list, fun_var) = flatten_expr e in
    let body_expr = Ast.Expr(fun_c_list) in
    let (Expr(fun_clause), return_var) = List.fold_right
        fun_curry id_list (body_expr, fun_var)
    in
    (fun_clause, return_var)
  | Appl (e1, e2) ->
    let (e1_clist, e1_var) = flatten_expr e1 in
    let (e2_clist, e2_var) = flatten_expr e2 in
    let new_var = ast_var_from_string "var~" in
    let new_clause =
      Ast.Clause (
        new_var,
        Ast.Appl_body (e1_var, e2_var)
      )
    in
    (e1_clist @ e2_clist @ [new_clause], new_var)
  | Let (var_ident, e1, e2) ->
    let (e1_clist, e1_var) = flatten_expr e1 in
    let (e2_clist, _) = flatten_expr e2 in
    let Ident(var_name) = var_ident in
    let letvar = Ast.Var(Ast.Ident(var_name), None) in
    let assignment_clause = Ast.Clause(letvar, Ast.Var_body(e1_var)) in
    (e1_clist @ [assignment_clause] @ e2_clist, letvar)
  | LetFun (sign, e) ->
    (* TODO: check for bugs!!! *)
    (* Translating the function signature... *)
    let Funsig(fun_name, id_list, fun_e) = sign in
    let (body_clist, body_var) = flatten_expr fun_e in
    let (Expr(fun_clauses), return_var) =
      List.fold_right fun_curry id_list (Expr(body_clist), body_var)
    in
    (* Flattening the "e2"... *)
    let (e_clist, e_var) = flatten_expr e in
    (* Assigning the function to the given function name... *)
    let On_ast.Ident(var_name) = fun_name in
    let letvar = Ast.Var(Ast.Ident(var_name), None) in
    let assignment_clause = Ast.Clause(letvar, Ast.Var_body(return_var)) in
    (fun_clauses @ [assignment_clause] @ e_clist, e_var)
  | LetRecFun (_, _) ->
    (* | LetRecFun (sig_list, e) ->  *)
    raise @@ Utils.Not_yet_implemented "let-fun"
  | Plus (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_plus
  | Minus (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_minus
  | Times (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_times
  | Divide (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_divide
  | Modulus (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_modulus
  | Equal (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_equal_to
  | LessThan (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_less_than
  | Leq (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_less_than_or_equal_to
  | And (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_and
  | Or (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_or
  | Not (e) ->
    let (e_clist, e_var) = flatten_expr e in
    let new_var = ast_var_from_string "var~" in
    let new_var' = ast_var_from_string "var~" in
    let true_clause =
      Ast.Clause(new_var', Ast.Value_body(Ast.Value_bool true))
    in
    let binop_clause =
      Ast.Clause(new_var,
                 Ast.Binary_operation_body(new_var',
                                           Ast.Binary_operator_xor,
                                           e_var))
    in
    (e_clist @ [true_clause; binop_clause], new_var)
  | If (e1, e2, e3) ->
    let (e1_clist, e1_var) = flatten_expr e1 in
    let (e2_clist, _) = flatten_expr e2 in
    let e2_clist = Ast.Expr(e2_clist) in
    let (e3_clist, _) = flatten_expr e3 in
    let e3_clist = Ast.Expr(e3_clist) in
    let new_clausevar = ast_var_from_string "var~" in
    let new_clause =
      Ast.Clause(new_clausevar,
                 Ast.Conditional_body(e1_var, e2_clist, e3_clist)) in
    (e1_clist @ [new_clause], new_clausevar)
  | Int (n) ->
    let int_var = ast_var_from_string "int" in
    let new_clause = Ast.Clause(int_var, Ast.Value_body(Ast.Value_int(n))) in
    ([new_clause], int_var)
  | Bool (b) ->
    let bool_var = ast_var_from_string "bool~" in
    let new_clause = Ast.Clause(bool_var, Ast.Value_body(Ast.Value_bool(b))) in
    ([new_clause], bool_var)
;;

let translate (e : On_ast.expr) : Odefa_ast.Ast.expr =
  let e_transformed = rec_transform e in
  let (e_remove_duplicates, _) = find_replace_duplicate_naming e_transformed [] in
  let (c_list, _) = flatten_expr e_remove_duplicates in
  let Clause(last_var, _) = List.last c_list in
  let res_var = Ast.Var(Ident("~result"), None) in
  let res_clause = Ast.Clause(res_var, Ast.Var_body(last_var)) in
  Ast.Expr(c_list @ [res_clause])
;;
