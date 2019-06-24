open Batteries;;
open Jhupllib;;
open Odefa_ast;;

(** This module contains utilities for picking fresh names.
    Plucked from Compiler labs. Will change if necessary. *)

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
  let (c_list, _) = flatten_expr e in
     Ast.Expr(c_list)
;;
