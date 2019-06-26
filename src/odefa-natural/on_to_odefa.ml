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

let fun_curry ident acc =
  let On_ast.Ident(id_string) = ident in
  let ast_ident = Ast.Ident(id_string) in
  let (acc_expr, _) = acc in
  let new_varname = fresh_name "var~" in
  let new_var = Ast.Var(Ast.Ident(new_varname), None) in
  let new_clause =
    Ast.Clause(new_var,
               Ast.Value_body(Ast.Value_function(
                   Ast.Function_value(Ast.Var(ast_ident, None), acc_expr)
                 ))) in
  (Ast.Expr([new_clause]), new_var)
;;

let rec rec_transform (e1 : On_ast.expr) : (On_ast.expr) =
  match e1 with
  | Var _ | Int _ | Bool _ | String _ -> e1
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
  | Plus (pe1, pe2) ->
    let transformed_expr1 = rec_transform pe1 in
    let transformed_expr2 = rec_transform pe2 in
    Plus (transformed_expr1, transformed_expr2)
  | Minus (me1, me2) ->
    let transformed_expr1 = rec_transform me1 in
    let transformed_expr2 = rec_transform me2 in
    Minus (transformed_expr1, transformed_expr2)
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
  | Record (map) ->
    let transformed_map = On_ast.Ident_map.map (fun value -> rec_transform value) map in
    Record (transformed_map)
  | RecordProj (map, label) ->
    let transformed_map = rec_transform map in
    RecordProj (transformed_map, label)
  | Not (note) ->
    let transformed_expr = rec_transform note in
    Not (transformed_expr)
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
  | Int _ | Bool _ | String _ -> e
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
  | Record (r_map) ->
    let map_changing_fun =
      (fun mexpr ->
         replace_duplicate_naming mexpr old_name new_name
      )
    in
    let new_rmap = On_ast.Ident_map.map map_changing_fun r_map in
    Record (new_rmap)
  | RecordProj (e', lab) ->
    let new_e' = replace_duplicate_naming e' old_name new_name in
    RecordProj (new_e', lab)
;;


let find_replace_on_fun_params = fun acc -> fun curr_id ->
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
  | Int _ | Bool _ | String _ | Var _ -> (e , ident_list)
  | Function (id_list, e') ->
    (* we assume that the id_list of this function consists of unique elements *)
    (* TODO: clean up the naming of id_list *)
    let (init_e, init_id_list) = find_replace_duplicate_naming e' ident_list in
    let (final_e, final_id_list, final_fun_id_list) =
      List.fold_left find_replace_on_fun_params (init_e, init_id_list, []) id_list in
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
      List.fold_left find_replace_on_fun_params (init_inner_e, init_list, []) param_list in
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
  | Record (recmap) ->
    (* We need to traverse the map and check for each one. *)
    let find_replace_on_records = fun key -> fun entry -> fun acc ->
      (* the accumulator is a pair between the new dictionary and the new
         ident list *)
      let (curr_map, curr_ident_list) = acc in
      let (new_entry, new_ident_list) =
        find_replace_duplicate_naming entry curr_ident_list in
      let new_map = On_ast.Ident_map.add key new_entry curr_map in
      (new_map, new_ident_list)
    in
    let base_pair = (On_ast.Ident_map.empty, ident_list) in
    let (res_map, res_ident_list) =
      On_ast.Ident_map.fold find_replace_on_records recmap base_pair in
    (Record (res_map), res_ident_list)
  | RecordProj (e', lab) ->
    let (new_e', e1_id_list) = find_replace_duplicate_naming e' ident_list in
    (RecordProj(new_e', lab), e1_id_list)

;;

let rec flatten_binop
    (e1 : On_ast.expr)
    (e2 : On_ast.expr)
    (binop : Ast.binary_operator)
  : (Ast.clause list * Ast.var) =
  let (e1_clist, e1_var) = flatten_expr e1 in
  let (e2_clist, e2_var) = flatten_expr e2 in
  let new_varname = fresh_name "var~" in
  let new_var = Ast.Var(Ast.Ident(new_varname), None) in
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
    let return_var = Ast.Var(Ast.Ident(i_string), None) in
    ([], return_var)
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
    let new_varname = fresh_name "var~" in
    let new_var = Ast.Var(Ast.Ident(new_varname), None) in
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
    flatten_binop e1 e2 Ast.Binary_operator_int_minus
  | Equal (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_equal_to
  | LessThan (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_int_less_than
  | Leq (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_int_less_than_or_equal_to
  | And (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_bool_and
  | Or (e1, e2) ->
    flatten_binop e1 e2 Ast.Binary_operator_bool_or
  | Not (e) ->
    let (e_clist, e_var) = flatten_expr e in
    let new_varname = fresh_name "var~" in
    let new_var = Ast.Var(Ast.Ident(new_varname), None) in
    let new_clause =
      Ast.Clause(new_var,
                 Ast.Unary_operation_body(Ast.Unary_operator_bool_not, e_var))
    in
    (e_clist @ [new_clause], new_var)
  | If (e1, e2, e3) ->
    (* TODO: there will be another version of a conditional where we can
       do pattern matching. *)
    (* NOTE: this is translation from an if statement. Thus e1 will be always
       matched with true. *)
    let (e1_clist, e1_var) = flatten_expr e1 in
    let (e2_clist, _) = flatten_expr e2 in
    let e2_varname = Ast.Ident(fresh_name "var~") in
    let e2_var = Ast.Var(e2_varname, None) in
    let e2_funval = Ast.Function_value(e2_var, Expr(e2_clist)) in
    let (e3_clist, _) = flatten_expr e3 in
    let e3_varname = Ast.Ident(fresh_name "var~") in
    let e3_var = Ast.Var(e3_varname, None) in
    let e3_funval = Ast.Function_value(e3_var, Expr(e3_clist)) in
    let new_clausename = Ast.Ident(fresh_name "var~") in
    let new_clausevar = Ast.Var(new_clausename, None) in
    let new_clause =
      Ast.Clause(new_clausevar,
                 Ast.Conditional_body(e1_var, Ast.Bool_pattern(true),
                                      e2_funval, e3_funval)) in
    (e1_clist @ [new_clause], new_clausevar)
  | Int (n) ->
    let int_varname = fresh_name "int~" in
    let int_var = Ast.Var(Ast.Ident(int_varname), None) in
    let new_clause = Ast.Clause(int_var, Ast.Value_body(Ast.Value_int(n))) in
    ([new_clause], int_var)
  | Bool (b) ->
    let bool_varname = fresh_name "bool~" in
    let bool_var = Ast.Var(Ast.Ident(bool_varname), None) in
    let new_clause = Ast.Clause(bool_var, Ast.Value_body(Ast.Value_bool(b))) in
    ([new_clause], bool_var)
  | String (s) ->
    let string_varname = fresh_name "string~" in
    let string_var = Ast.Var(Ast.Ident(string_varname), None) in
    let new_clause = Ast.Clause(string_var, Ast.Value_body(Ast.Value_string(s)))
    in
    ([new_clause], string_var)
  | Record (recexpr_map) ->
    let record_enum = On_ast.Ident_map.enum recexpr_map in
    (* function for Enum.fold that generates the clause list and the
       id -> var map for Odefa's record *)
    let flatten_and_map acc ident_expr_tuple :
      (Ast.clause list * Ast.var Ast.Ident_map.t) =
      let (clist, recmap) = acc in
      let (id, e) = ident_expr_tuple in
      let On_ast.Ident(id_string) = id in
      let ast_id = Ast.Ident(id_string) in
      let (e_clist, e_var) = flatten_expr e in
      let new_clist = clist @ e_clist in
      let new_map = Ast.Ident_map.add ast_id e_var recmap in
      (new_clist, new_map)
    in
    let empty_acc = ([], Ast.Ident_map.empty) in
    let (clist, map) = Enum.fold flatten_and_map empty_acc record_enum in
    let rec_varname = fresh_name "record~" in
    let rec_var = Ast.Var(Ast.Ident(rec_varname), None) in
    let new_clause = Ast.Clause(rec_var,
                                Ast.Value_body(Ast.Value_record(
                                    Ast.Record_value (map)
                                  ))) in
    (clist @ [new_clause], rec_var)
  | RecordProj (e, lab) ->
    let (e_clist, e_var) = flatten_expr e in
    let On_ast.Label(l_string) = lab in
    let l_ident = Ast.Ident(l_string) in
    let new_varname = fresh_name "var~" in
    let new_var = Ast.Var(Ast.Ident(new_varname), None) in
    let new_clause = Ast.Clause(new_var,
                                Ast.Projection_body(e_var, l_ident)
                               ) in
    (e_clist @ [new_clause], new_var)
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
