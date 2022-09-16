open Batteries
open Jhupllib
open Jayil
open Jay
open Ast_tools
open Jay_to_jayil_monad
open TranslationMonad

(* **** Expression flattening **** *)

(** Create new jayil variable with mapping to jay expr *)

let new_jayil_var (e_desc : Jay_ast.expr_desc) (var_name : string) : Ast.var m =
  let%bind var = fresh_var var_name in
  let%bind () = add_jayil_jay_mapping var e_desc in
  return var

let new_jayil_inst_var (e_desc : Jay_ast.expr_desc) (var_name : string) :
    Ast.var m =
  let%bind var = new_jayil_var e_desc var_name in
  let%bind () = add_instrument_var var in
  return var

(* TODO: When is this ever relavant? *)

(** Returns the body of a function or conditional with its return variable *)
let nonempty_body (e_desc : Jay_ast.expr_desc)
    ((body, var) : Ast.clause list * Ast.var) : (Ast.clause list * Ast.var) m =
  match body with
  | [] ->
      let%bind x = fresh_var "var" in
      let%bind () = add_jayil_jay_mapping x e_desc in
      return @@ ([ Ast.Clause (x, Var_body var) ], x)
  | _ -> return (body, var)

(** Create a new abort clause with multiple conditional clause variables *)
let add_abort_expr (e_desc : Jay_ast.expr_desc) (_ : Ast.var list) : Ast.expr m
    =
  let%bind abort_var = fresh_var "ab" in
  let%bind () = add_jayil_jay_mapping abort_var e_desc in
  let abort_clause = Ast.Clause (abort_var, Abort_body) in
  return @@ Ast.Expr [ abort_clause ]

let jay_to_jayil_ident (Jay_ast.Ident id) = Ast.Ident id

(** Flatten a pattern. The second value in the pair is a list of projection
    clauses associated with a record or variable pattern; these will be appended
    to the front of the pattern's expression. *)
let flatten_pattern (e_desc : Jay_ast.expr_desc) (pat_var : Ast.var)
    (pattern : Jay_ast.pattern) : (Ast.pattern * Ast.clause list) m =
  match pattern with
  | Jay_ast.AnyPat -> return (Ast.Any_pattern, [])
  | Jay_ast.IntPat -> return (Ast.Int_pattern, [])
  | Jay_ast.BoolPat -> return (Ast.Bool_pattern, [])
  | Jay_ast.FunPat -> return (Ast.Fun_pattern, [])
  | Jay_ast.RecPat rec_pat ->
      let rec_pat' =
        rec_pat |> Jay_ast.Ident_map.keys
        |> Enum.map jay_to_jayil_ident
        |> Ast.Ident_set.of_enum
      in
      let%bind projections =
        rec_pat |> Jay_ast.Ident_map.enum |> List.of_enum
        |> list_fold_left_m
             (fun acc (lbl, var) ->
               match var with
               | Some v ->
                   let v' = jay_to_jayil_ident v in
                   let lbl' = jay_to_jayil_ident lbl in
                   let ast_var = Ast.Var (v', None) in
                   let%bind () = add_jayil_jay_mapping ast_var e_desc in
                   let%bind () = add_instrument_var ast_var in
                   let ast_body = Ast.Projection_body (pat_var, lbl') in
                   return @@ acc @ [ Ast.Clause (ast_var, ast_body) ]
               | None -> return acc)
             []
      in
      return (Ast.Rec_pattern rec_pat', projections)
  (* | Jay_ast.StrictRecPat rec_pat ->
     let rec_pat' =
       rec_pat
       |> Jay_ast.Ident_map.keys
       |> Enum.map jay_to_jayil_ident
       |> Ast.Ident_set.of_enum
     in
     let%bind projections =
       rec_pat
       |> Jay_ast.Ident_map.enum
       |> List.of_enum
       |> list_fold_left_m
           (fun acc (lbl, var) ->
             match var with
             | Some v ->
               let v' = jay_to_jayil_ident v in
               let lbl' = jay_to_jayil_ident lbl in
               let ast_var = Ast.Var (v', None) in
               let%bind () = add_jayil_jay_mapping ast_var e_desc in
               let%bind () = add_instrument_var ast_var in
               let ast_body = Ast.Projection_body(pat_var, lbl') in
               return @@ acc @ [(Ast.Clause (ast_var, ast_body))]
             | None -> return acc
           )
           []
     in
     return (Ast.Strict_rec_pattern rec_pat', projections) *)
  | Jay_ast.VarPat var_pat ->
      let (Jay_ast.Ident var_id) = var_pat in
      let ast_var = Ast.Var (Ident var_id, None) in
      let%bind () = add_jayil_jay_mapping ast_var e_desc in
      let ast_body = Ast.Var_body pat_var in
      let clause = Ast.Clause (ast_var, ast_body) in
      return (Ast.Any_pattern, [ clause ])
  | Jay_ast.VariantPat _ ->
      raise
      @@ Utils.Invariant_failure
           "flatten_pattern: Variants patterns should have been encoded!"
  | Jay_ast.EmptyLstPat | Jay_ast.LstDestructPat _ ->
      raise
      @@ Utils.Invariant_failure
           "flatten_pattern: List patterns should have been encoded!"

(** Flatten a function *)
let flatten_fun ?(binding_name = (None : Jay_ast.Ident.t option))
    (e_desc : Jay_ast.expr_desc) (param_names : Jay_ast.Ident.t list)
    (body : Ast.expr) : (Ast.expr * Ast.var) m =
  list_fold_right_m
    (fun (param : Jay_ast.Ident.t) ((e : Ast.expr), (_ : Ast.Var.t)) ->
      let id = jay_to_jayil_ident param in
      let%bind (new_var : Ast.var) =
        match binding_name with
        | None -> fresh_var "flatten_fun"
        | Some (Ident s) -> fresh_var s
      in
      let%bind () = add_jayil_jay_mapping new_var e_desc in
      let fun_val = Ast.Value_function (Function_value (Var (id, None), e)) in
      let fun_body = Ast.Value_body fun_val in
      let new_clause = Ast.Clause (new_var, fun_body) in
      let expr' : Ast.expr = Ast.Expr [ new_clause ] in
      return (expr', new_var))
    param_names
    (body, retv body)

(** Flatten a binary operation *)
let rec flatten_binop (expr_desc : Jay_ast.expr_desc)
    (e1_desc : Jay_ast.expr_desc) (e2_desc : Jay_ast.expr_desc)
    (binop : Ast.binary_operator) : (Ast.clause list * Ast.var) m =
  let%bind e1_clist, e1_var = flatten_expr e1_desc in
  let%bind e2_clist, e2_var = flatten_expr e2_desc in
  let%bind notop_var = fresh_var "binop" in
  let%bind () = add_jayil_jay_mapping notop_var expr_desc in
  let binop_body = Ast.Binary_operation_body (e1_var, binop, e2_var) in
  let new_clause = Ast.Clause (notop_var, binop_body) in
  return (e1_clist @ e2_clist @ [ new_clause ], notop_var)

(* TODO: Add untouched check logic here; hack solution, might want to rethink
         in the future. In the spec this is supposed to be part of the instrumentation
         step. *)
and flatten_pattern_match (expr_desc : Jay_ast.expr_desc) (subj_var : Ast.var)
    (pat_e_list : (Jay_ast.pattern * Jay_ast.expr_desc) list) :
    (Ast.clause list * Ast.var) m =
  let rec convert_match ((pat, e) : Jay_ast.pattern * Jay_ast.expr_desc)
      (accum : Ast.expr * Ast.clause list) : (Ast.expr * Ast.clause list) m =
    (* Conditional expression *)
    let cond_expr_inner, match_cls_list_tail = accum in
    (* Variables *)
    let%bind bool_var = new_jayil_inst_var expr_desc "m_match_bool" in
    let%bind cond_var = new_jayil_inst_var expr_desc "m_match_cond" in
    (* Clauses and expressions *)
    (* TODO:
       Hack: Depending on what patterns we have, if we fall into the base
       cases (bool or int), AND that the match expression in question here
       has a tag that we mapped an error ident to, then we'll record the
       mapping between this ident and bool_var.
       Question: How to then connect this bool_var to the abort?
    *)
    let%bind flat_pat, new_clauses = flatten_pattern expr_desc subj_var pat in
    let%bind c_list, _ = flatten_expr e in
    let c_list' = new_clauses @ c_list in
    let match_clause = Ast.Clause (bool_var, Match_body (subj_var, flat_pat)) in
    let%bind flat_curr_expr = return @@ Ast.Expr c_list' in
    let cond_body =
      Ast.Conditional_body (bool_var, flat_curr_expr, cond_expr_inner)
    in
    let cond_clause = Ast.Clause (cond_var, cond_body) in
    return (Ast.Expr [ cond_clause ], match_clause :: match_cls_list_tail)
  in
  let%bind innermost = add_abort_expr expr_desc [] in
  let%bind cond_expr, match_cls_list =
    list_fold_right_m convert_match pat_e_list (innermost, [])
  in

  (* Predicates - one big disjunction of atomic formulae *)
  let create_or_clause cls_1 cls_2 =
    let (Ast.Clause (m_var_1, _)) = cls_1 in
    let (Ast.Clause (m_var_2, _)) = cls_2 in
    let%bind m_match_or = new_jayil_inst_var expr_desc "m_match_or" in
    let binop_body =
      Ast.Binary_operation_body (m_var_1, Binary_operator_or, m_var_2)
    in
    return @@ Ast.Clause (m_match_or, binop_body)
  in
  let create_or_list accum match_cls =
    match accum with
    | [] -> return [ match_cls ]
    | cls :: _ ->
        let%bind new_or_cls = create_or_clause match_cls cls in
        return @@ (new_or_cls :: match_cls :: accum)
  in
  let%bind pred_cls_list = list_fold_left_m create_or_list [] match_cls_list in

  (* Putting it all together *)
  let (Ast.Clause (match_pred, _)) = List.hd pred_cls_list in
  let%bind cond_var = new_jayil_inst_var expr_desc "match" in

  let%bind abort_expr = add_abort_expr expr_desc [ cond_var ] in
  let cond_cls =
    Ast.Clause (cond_var, Conditional_body (match_pred, cond_expr, abort_expr))
  in

  return (List.rev pred_cls_list @ [ cond_cls ], cond_var)

(* let (Ast.Expr cc) = cond_expr in
     let (Ast.Clause (match_pred, _)) =
     List.hd cc (* Never raises b/c pat_e_list must be nonempty *)
   in

   let%bind pred_cls_list = return match_cls_list in
   return (List.rev pred_cls_list @ cc, match_pred) *)

(** Flatten an entire expression (i.e. convert jay into jayil code) *)
and flatten_expr (expr_desc : Jay_ast.expr_desc) : (Ast.clause list * Ast.var) m
    =
  let recurse = flatten_expr in
  let exp = expr_desc.body in
  (* let og_tag = expr_desc.tag in *)
  match exp with
  | Var id ->
      let%bind alias_var = fresh_var "var" in
      let (Ident i_string) = id in
      let id_var = Ast.Var (Ident i_string, None) in
      let%bind () = add_jayil_jay_mapping alias_var expr_desc in
      let%bind () = add_jayil_jay_mapping id_var expr_desc in
      return ([ Ast.Clause (alias_var, Var_body id_var) ], alias_var)
  | Input ->
      let%bind input_var = fresh_var "input" in
      let%bind () = add_jayil_jay_mapping input_var expr_desc in
      return ([ Ast.Clause (input_var, Input_body) ], input_var)
  | Function (id_list, e) ->
      let%bind fun_c_list, _ = nonempty_body expr_desc @@@ recurse e in
      let body_expr = Ast.Expr fun_c_list in
      let%bind Expr fun_clause, return_var =
        flatten_fun expr_desc id_list body_expr
      in
      return (fun_clause, return_var)
  | Appl (e1, e2) ->
      let%bind e1_clist, e1_var = recurse e1 in
      let%bind e2_clist, e2_var = recurse e2 in
      let%bind appl_var = fresh_var "appl" in
      let%bind () = add_jayil_jay_mapping appl_var expr_desc in
      let new_clause = Ast.Clause (appl_var, Ast.Appl_body (e1_var, e2_var)) in
      return (e1_clist @ e2_clist @ [ new_clause ], appl_var)
  | Let (var_ident, e1, e2) ->
      let%bind e1_clist, e1_var = recurse e1 in
      let%bind e2_clist, e2_var = recurse e2 in
      let (Ident var_name) = var_ident in
      let lt_var = Ast.Var (Ident var_name, None) in
      let%bind () = add_jayil_jay_mapping lt_var expr_desc in
      let assignment_clause = Ast.Clause (lt_var, Var_body e1_var) in
      return (e1_clist @ [ assignment_clause ] @ e2_clist, e2_var)
  | LetFun (sign, e) ->
      (* TODO: check for bugs!!! *)
      (* Translating the function signature... *)
      let (Funsig (fun_name, id_list, fun_e)) = sign in
      let%bind body_clist, _ = nonempty_body expr_desc @@@ recurse fun_e in
      let%bind Expr fun_clauses, return_var =
        flatten_fun ~binding_name:(Some fun_name) expr_desc id_list
          (Expr body_clist)
      in
      (* Flattening the "e2"... *)
      let%bind e_clist, e_var = recurse e in
      (* Assigning the function to the given function name... *)
      let (Jay_ast.Ident var_name) = fun_name in
      let lt_var = Ast.Var (Ident var_name, None) in
      let%bind () = add_jayil_jay_mapping lt_var expr_desc in
      let assignment_clause = Ast.Clause (lt_var, Var_body return_var) in
      return (fun_clauses @ [ assignment_clause ] @ e_clist, e_var)
  | LetRecFun (_, _) ->
      raise
      @@ Utils.Invariant_failure
           "LetRecFun should not have been passed to flatten_expr"
  | Plus (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_plus
  | Minus (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_minus
  | Times (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_times
  | Divide (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_divide
  | Modulus (e1, e2) ->
      flatten_binop expr_desc e1 e2 Ast.Binary_operator_modulus
  | Equal (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_equal_to
  | Neq (e1, e2) ->
      flatten_binop expr_desc e1 e2 Ast.Binary_operator_not_equal_to
  | LessThan (e1, e2) ->
      flatten_binop expr_desc e1 e2 Ast.Binary_operator_less_than
  | Leq (e1, e2) ->
      flatten_binop expr_desc e1 e2 Ast.Binary_operator_less_than_or_equal_to
  | GreaterThan (e1, e2) ->
      (* Reverse e1 and e2 *)
      flatten_binop expr_desc e2 e1 Ast.Binary_operator_less_than
  | Geq (e1, e2) ->
      (* Reverse e1 and e2 *)
      flatten_binop expr_desc e2 e1 Ast.Binary_operator_less_than_or_equal_to
  | And (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_and
  | Or (e1, e2) -> flatten_binop expr_desc e1 e2 Ast.Binary_operator_or
  | Not e ->
      let%bind e_clist, e_var = recurse e in
      let%bind notop_var = fresh_var "notop" in
      let%bind () = add_jayil_jay_mapping notop_var expr_desc in
      let not_body = Ast.Not_body e_var in
      let binop_clause = Ast.Clause (notop_var, not_body) in
      return (e_clist @ [ (* true_clause;  *) binop_clause ], notop_var)
  | If (e1, e2, e3) ->
      (* TODO: there will be another version of a conditional where we can
         do pattern matching. *)
      (* NOTE: this is translation from an if statement. Thus e1 will be always
         matched with true. *)
      let%bind e1_clst, e1_var = recurse e1 in
      let%bind e2_clst, _ = nonempty_body expr_desc @@@ recurse e2 in
      let%bind e3_clst, _ = nonempty_body expr_desc @@@ recurse e3 in
      let%bind if_var = fresh_var "if" in
      let%bind () = add_jayil_jay_mapping if_var expr_desc in
      let if_body = Ast.Conditional_body (e1_var, Expr e2_clst, Expr e3_clst) in
      let if_clause = Ast.Clause (if_var, if_body) in
      return (e1_clst @ [ if_clause ], if_var)
  | Int n ->
      let%bind int_var = fresh_var "int" in
      let%bind () = add_jayil_jay_mapping int_var expr_desc in
      let new_clause = Ast.Clause (int_var, Ast.Value_body (Ast.Value_int n)) in
      return ([ new_clause ], int_var)
  | Bool b ->
      let%bind bool_var = fresh_var "bool" in
      let%bind () = add_jayil_jay_mapping bool_var expr_desc in
      let new_clause =
        Ast.Clause (bool_var, Ast.Value_body (Ast.Value_bool b))
      in
      return ([ new_clause ], bool_var)
  | Record recexpr_map ->
      (* function for Enum.fold that generates the clause list and the
         id -> var map for JayIL's record *)
      let flatten_and_map acc ident_expr_tuple :
          (Ast.clause list * Ast.var Ast.Ident_map.t) m =
        let clist, recmap = acc in
        let id, e = ident_expr_tuple in
        let (Jay_ast.Ident id_string) = id in
        let ast_id = Ast.Ident id_string in
        let%bind e_clist, e_var = recurse e in
        let new_clist = clist @ e_clist in
        let new_map = Ast.Ident_map.add ast_id e_var recmap in
        return (new_clist, new_map)
      in
      let empty_acc = ([], Ast.Ident_map.empty) in
      let%bind clist, map =
        Jay_ast.Ident_map.enum recexpr_map
        |> List.of_enum
        |> list_fold_left_m flatten_and_map empty_acc
      in
      let%bind rec_var = fresh_var "record" in
      let%bind () = add_jayil_jay_mapping rec_var expr_desc in
      let new_body = Ast.Value_body (Ast.Value_record (Ast.Record_value map)) in
      let new_clause = Ast.Clause (rec_var, new_body) in
      return (clist @ [ new_clause ], rec_var)
  | RecordProj (rec_expr, lab) ->
      let%bind e_clist, e_var = recurse rec_expr in
      let (Jay_ast.Label l_string) = lab in
      let l_ident = Ast.Ident l_string in
      let%bind proj_var = fresh_var "proj" in
      let%bind () = add_jayil_jay_mapping proj_var expr_desc in
      let new_clause =
        Ast.Clause (proj_var, Ast.Projection_body (e_var, l_ident))
      in
      return (e_clist @ [ new_clause ], proj_var)
  | Match (subject, pat_e_list) ->
      (* We need to flatten the subject first *)
      let%bind subject_clause_list, subj_var = recurse subject in
      (* Flatten the pattern-expr list *)
      let%bind match_clause_list, cond_var =
        flatten_pattern_match expr_desc subj_var pat_e_list
      in
      return (subject_clause_list @ match_clause_list, cond_var)
  | VariantExpr (_, _) ->
      raise
      @@ Utils.Invariant_failure
           "flatten_expr: VariantExpr expressions should have been desugared."
  | List _ | ListCons _ ->
      raise
      @@ Utils.Invariant_failure
           "flatten_expr: List expressions should have been handled!"
  (* TODO: This should happen in the instrumentation phase *)
  | Assert e ->
      let%bind flattened_exprs, last_var = recurse e in
      (* Helper function *)
      let add_var var_name =
        let%bind var = fresh_var var_name in
        let%bind () = add_jayil_jay_mapping var expr_desc in
        return var
      in
      (* Variables *)
      let%bind assert_pred = add_var "assert_pred" in
      let%bind assert_result = add_var "assert_res" in
      let%bind assert_result_inner = add_var "assert_res_true" in
      (* Clauses *)
      let alias_clause = Ast.Clause (assert_pred, Var_body last_var) in
      (* We use an empty record as the result value, since no valid operation can
         done on it (any projection will fail, and no binop, application, nor
         conditional will work either).  It's a hack (especially if we match on
         it), but it will do for now. *)
      let res_value = Ast.Value_record (Record_value Ast.Ident_map.empty) in
      let t_path =
        Ast.Expr [ Clause (assert_result_inner, Value_body res_value) ]
      in
      let%bind f_path = add_abort_expr expr_desc [ assert_result ] in
      let cond_clause =
        Ast.Clause
          (assert_result, Conditional_body (assert_pred, t_path, f_path))
      in
      let all_clauses = flattened_exprs @ [ alias_clause; cond_clause ] in
      return (all_clauses, assert_result)
  | Assume e ->
      let%bind flattened_exprs, last_var = recurse e in
      let%bind assume_var = fresh_var "assume" in
      let%bind () = add_jayil_jay_mapping assume_var expr_desc in
      let new_clause = Ast.Clause (assume_var, Assume_body last_var) in
      return (flattened_exprs @ [ new_clause ], assume_var)
  | Error (Jay_ast.Ident x) ->
      let%bind error_var = fresh_var "error_var" in
      let error_clause =
        Ast.Clause (error_var, Ast.Var_body (Ast.Var (Ast.Ident x, None)))
      in
      let%bind () = add_jayil_jay_mapping error_var expr_desc in
      (* Helper function *)
      let add_var var_name =
        let%bind var = fresh_var var_name in
        let%bind () = add_jayil_jay_mapping var expr_desc in
        return var
      in
      (* Variables *)
      let%bind assert_pred = add_var "assert_pred" in
      let%bind assert_result = add_var "assert_res" in
      let%bind assert_result_inner = add_var "assert_res_true" in
      (* Clauses *)
      let alias_clause = Ast.Clause (assert_pred, Var_body error_var) in
      (* We use an empty record as the result value, since no valid operation can
         done on it (any projection will fail, and no binop, application, nor
         conditional will work either).  It's a hack (especially if we match on
         it), but it will do for now. *)
      let res_value = Ast.Value_record (Record_value Ast.Ident_map.empty) in
      let t_path =
        Ast.Expr [ Clause (assert_result_inner, Value_body res_value) ]
      in
      let%bind f_path = add_abort_expr expr_desc [ assert_result ] in
      let cond_clause =
        Ast.Clause
          (assert_result, Conditional_body (assert_pred, t_path, f_path))
      in
      let all_clauses = [ error_clause ] @ [ alias_clause; cond_clause ] in
      return (all_clauses, assert_result)

let flatten e =
  let%bind c_list, _ = flatten_expr e in
  return c_list
