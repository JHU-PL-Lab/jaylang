open Batteries
open Jhupllib
open Odefa_ast
open Ast_tools
open On_to_odefa_preliminary
open On_to_odefa_monad

(** In this module we will translate from natodefa to odefa in the following
    order:

    * Desugar let rec, lists, variants, and list/variant patterns * Alphatize
    program again (do this after to allow above to introduce dupes) * Flatten
    natodefa expressions to odefa expressions * Instrument odefa with type/error
    constriants *)

open TranslationMonad

let lazy_logger = Logger_utils.make_lazy_logger "On_to_odefa"

(* **** Variable alphatization **** *)

(** Determines all variables contained within a pattern. *)
let rec pat_vars (pat : On_ast.pattern) : On_ast.Ident_set.t =
  let open On_ast in
  match pat with
  | AnyPat | IntPat | BoolPat | FunPat -> Ident_set.empty
  | RecPat record ->
      record |> Ident_map.enum
      |> Enum.fold
           (fun idents (_, x_opt) ->
             match x_opt with
             | Some x -> Ident_set.add x idents
             | None -> idents)
           Ident_set.empty
  | VariantPat (_, x) -> Ident_set.singleton x
  | VarPat x -> Ident_set.singleton x
  | EmptyLstPat -> Ident_set.empty
  | LstDestructPat (x1, x2) ->
      Ident_set.empty |> Ident_set.add x1 |> Ident_set.add x2

(** Performs variable substitution on a pattern. *)
let pat_rename_vars (name_map : On_ast.Ident.t On_ast.Ident_map.t)
    (pattern : On_ast.pattern) : On_ast.pattern =
  let open On_ast in
  match pattern with
  | AnyPat | IntPat | BoolPat | FunPat -> pattern
  | RecPat record ->
      let record' =
        record |> Ident_map.enum
        |> Enum.map (fun (lbl, x_opt) ->
               match x_opt with
               | Some x -> (lbl, Some (Ident_map.find_default x x name_map))
               | None -> (lbl, None))
        |> Ident_map.of_enum
      in
      RecPat record'
  | VariantPat (lbl, x) ->
      let x' = Ident_map.find_default x x name_map in
      VariantPat (lbl, x')
  | VarPat x ->
      let x' = Ident_map.find_default x x name_map in
      VarPat x'
  | EmptyLstPat -> pattern
  | LstDestructPat (h, t) ->
      let h' = Ident_map.find_default h h name_map in
      let t' = Ident_map.find_default t t name_map in
      LstDestructPat (h', t')

(** Performs alpha substitution on a given expression. *)
let rec rename_variable (old_name : On_ast.ident) (new_name : On_ast.ident)
    (e : On_ast.expr) : On_ast.expr =
  let open On_ast in
  (* NOTE: the generic homomorphism routine m_env_transform_expr does not allow
     us to change the environment of the homomorphism as we descend or to block
     descending into a given subtree, so we can't use it here. *)
  let recurse = rename_variable old_name new_name in
  match e with
  | Int _ | Bool _ | Input -> e
  | Var id -> if id = old_name then Var new_name else Var id
  | Function (id_list, e') ->
      if List.exists (Ident.equal old_name) id_list
      then e
      else Function (id_list, recurse e')
  | Let (id, e1, e2) ->
      let new_e1 = recurse e1 in
      if id = old_name
      then Let (id, new_e1, e2)
      else
        let new_e2 = recurse e2 in
        Let (id, new_e1, new_e2)
  | LetFun (f_sig, e') ->
      let (Funsig (id, id_list, fun_e)) = f_sig in
      (* If old_name is same as the function name, then don't change anything *)
      if id = old_name
      then e
      else if (* If old_name is same as one of the names of the params, then
                  we only want to change the code outside of the function. *)
              List.exists (Ident.equal old_name) id_list
      then
        let new_e' = recurse e' in
        LetFun (f_sig, new_e')
      else
        (* change both the inside and the outside expressions *)
        let new_inner_e = recurse fun_e in
        let new_outer_e = recurse e' in
        let new_funsig = Funsig (id, id_list, new_inner_e) in
        LetFun (new_funsig, new_outer_e)
  | LetRecFun (f_sigs, e') ->
      let function_names =
        f_sigs |> List.enum
        |> Enum.map (fun (Funsig (name, _, _)) -> name)
        |> Ident_set.of_enum
      in
      let f_sigs' =
        if Ident_set.mem old_name function_names
        then f_sigs
        else
          List.map
            (fun (Funsig (name, params, body)) ->
              if List.exists (Ident.equal old_name) params
              then Funsig (name, params, body)
              else Funsig (name, params, recurse body))
            f_sigs
      in
      let e'' =
        if Ident_set.mem old_name function_names then e' else recurse e'
      in
      LetRecFun (f_sigs', e'')
  | Match (e0, cases) ->
      let e0' = recurse e0 in
      let cases' =
        List.map
          (fun (pattern, body) ->
            if Ident_set.mem old_name (pat_vars pattern)
            then (pattern, body)
            else (pattern, recurse body))
          cases
      in
      Match (e0', cases')
  | Appl (e1, e2) -> Appl (recurse e1, recurse e2)
  | Plus (e1, e2) -> Plus (recurse e1, recurse e2)
  | Minus (e1, e2) -> Minus (recurse e1, recurse e2)
  | Times (e1, e2) -> Times (recurse e1, recurse e2)
  | Divide (e1, e2) -> Divide (recurse e1, recurse e2)
  | Modulus (e1, e2) -> Modulus (recurse e1, recurse e2)
  | Equal (e1, e2) -> Equal (recurse e1, recurse e2)
  | Neq (e1, e2) -> Neq (recurse e1, recurse e2)
  | LessThan (e1, e2) -> LessThan (recurse e1, recurse e2)
  | Leq (e1, e2) -> Leq (recurse e1, recurse e2)
  | GreaterThan (e1, e2) -> GreaterThan (recurse e1, recurse e2)
  | Geq (e1, e2) -> Geq (recurse e1, recurse e2)
  | And (e1, e2) -> And (recurse e1, recurse e2)
  | Or (e1, e2) -> Or (recurse e1, recurse e2)
  | Not e1 -> Not (recurse e1)
  | If (e1, e2, e3) -> If (recurse e1, recurse e2, recurse e3)
  | Record m -> Record (Ident_map.map recurse m)
  | RecordProj (e1, lbl) -> RecordProj (recurse e1, lbl)
  | VariantExpr (lbl, e1) -> VariantExpr (lbl, recurse e1)
  | List es -> List (List.map recurse es)
  | ListCons (e1, e2) -> ListCons (recurse e1, recurse e2)
  | Assert e -> Assert (recurse e)
  | Assume e -> Assume (recurse e)

(** This function alphatizes an entire expression. If a variable is defined more
    than once in the given expression, all but one of the declarations will be
    alpha-renamed to a fresh name. *)
let alphatize (e : On_ast.expr) : On_ast.expr m =
  let open On_ast in
  (* Given a list of identifiers, a list of expressions, and a list of
     previously declared identifiers, this helper routine renames all previously
     declared identifiers which appear in the list within all of the
     expressions.  The returned values are the renamed list of identifiers,
     the renamed expressions, the new set of declared identifiers, and a
     dictionary mapping the identifiers which were renamed onto their new
     values. *)
  let rec ensure_exprs_unique_names (names : Ident.t list) (exprs : expr list)
      (prev_declared : Ident_set.t) :
      (Ident.t list * expr list * Ident_set.t * Ident.t Ident_map.t) m =
    match names with
    | [] -> return ([], exprs, prev_declared, Ident_map.empty)
    | name :: more_names ->
        let%bind more_names', exprs', prev_declared', renaming' =
          ensure_exprs_unique_names more_names exprs prev_declared
        in
        if Ident_set.mem name prev_declared'
        then
          let (Ident s) = name in
          let%bind new_s = fresh_name s in
          let new_name = Ident new_s in
          let%bind () = add_natodefa_var_mapping new_name name in
          let exprs'' = List.map (rename_variable name new_name) exprs' in
          let prev_declared'' = Ident_set.add new_name prev_declared' in
          let renaming'' = Ident_map.add name new_name renaming' in
          return (new_name :: more_names', exprs'', prev_declared'', renaming'')
        else
          let prev_declared'' = Ident_set.add name prev_declared' in
          return (name :: more_names', exprs', prev_declared'', renaming')
  in
  let ensure_expr_unique_names names expr seen =
    let%bind names', exprs', seen', renaming' =
      ensure_exprs_unique_names names [ expr ] seen
    in
    return (names', List.hd exprs', seen', renaming')
  in
  let rec walk (expr : expr) (seen_declared : Ident_set.t) :
      (expr * Ident_set.t) m =
    let zero () =
      raise @@ Jhupllib_utils.Invariant_failure "list changed size"
    in
    let%bind expr', seen_declared' =
      match expr with
      (* In leaf cases, no new variables are defined and so we have no work to
         do. *)
      | Var _ | Input | Int _ | Bool _ -> return (expr, seen_declared)
      | Function (params, body) ->
          (* Recurse on the body to ensure that it is internally alphatized. *)
          let%bind body', seen_declared' = walk body seen_declared in
          (* FIXME?: assuming that parameters in functions are not duplicated;
                    probably should verify that somewhere *)
          let%bind params', body'', seen_declared'', _ =
            ensure_expr_unique_names params body' seen_declared'
          in
          return (Function (params', body''), seen_declared'')
      | Appl (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return @@ (Appl (e1', e2'), seen_declared'')
      | Let (x, e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          let%bind xs, es, seen_declared''', _ =
            ensure_exprs_unique_names [ x ] [ e1'; e2' ] seen_declared''
          in
          let%orzero [ x' ], [ e1''; e2'' ] = (xs, es) in
          return (Let (x', e1'', e2''), seen_declared''')
      | LetRecFun (funsigs, expr) ->
          let%bind funsigs'rev, seen_declared' =
            list_fold_left_m
              (fun (acc, seen) (Funsig (name, params, body)) ->
                let%bind body', seen' = walk body seen in
                return (Funsig (name, params, body') :: acc, seen'))
              ([], seen_declared) funsigs
          in
          let funsigs' = List.rev funsigs'rev in
          (* FIXME?: assuming that parameters in functions are not duplicated;
                    probably should verify that somewhere *)
          (* FIXME?: assuming that function names in recursive groups are not
                    duplicated; probably should verify that somewhere *)
          (* First, make sure that all of the function *names* are unique. *)
          let function_names, function_bodies =
            funsigs'
            |> List.map (fun (Funsig (name, _, body)) -> (name, body))
            |> List.split
          in
          let%bind function_names', out_exprs, seen_declared'', _ =
            ensure_exprs_unique_names function_names (expr :: function_bodies)
              seen_declared'
          in
          let%orzero (expr' :: function_bodies') = out_exprs in
          let funsigs'' =
            List.combine function_names' function_bodies'
            |> List.combine funsigs'
            |> List.map (fun (Funsig (_, params, _), (name, body)) ->
                   Funsig (name, params, body))
          in
          (* Now, for each function, make sure that the *parameters* are unique. *)
          let%bind funsigs'''_rev, seen_declared''' =
            funsigs''
            |> list_fold_left_m
                 (fun (out_funsigs, seen) (Funsig (name, params, body)) ->
                   let%bind params', body', seen', _ =
                     ensure_expr_unique_names params body seen
                   in
                   return (Funsig (name, params', body') :: out_funsigs, seen'))
                 ([], seen_declared'')
          in
          return (LetRecFun (List.rev funsigs'''_rev, expr'), seen_declared''')
      | LetFun (funsig, expr) ->
          (* FIXME?: assuming that parameters in functions are not duplicated;
                    probably should verify that somewhere *)
          (* Unpack signature *)
          let (Funsig (name, params, body)) = funsig in
          (* Recurse on the second expression to ensure that it is internally
             alphatized. *)
          let%bind expr', seen_declared' = walk expr seen_declared in
          (* Perform renamings on any names which we have already seen from the
             outside. *)
          let%bind names', expr'', seen_declared'', _ =
            ensure_expr_unique_names [ name ] expr' seen_declared'
          in
          let%orzero [ name' ] = names' in
          (* Recurse on the body expression to ensure that it is internally
             alphatized. *)
          let%bind body', seen_declared''' = walk body seen_declared'' in
          (* Perform renamings on any names which we have already seen from the
             outside. *)
          let%bind params', body'', seen_declared'''', _ =
            ensure_expr_unique_names params body' seen_declared'''
          in
          return
            (LetFun (Funsig (name', params', body''), expr''), seen_declared'''')
      | Plus (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Plus (e1', e2'), seen_declared'')
      | Minus (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Minus (e1', e2'), seen_declared'')
      | Times (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Times (e1', e2'), seen_declared'')
      | Divide (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Divide (e1', e2'), seen_declared'')
      | Modulus (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Modulus (e1', e2'), seen_declared'')
      | Equal (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Equal (e1', e2'), seen_declared'')
      | Neq (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Neq (e1', e2'), seen_declared'')
      | LessThan (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (LessThan (e1', e2'), seen_declared'')
      | Leq (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Leq (e1', e2'), seen_declared'')
      | GreaterThan (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (GreaterThan (e1', e2'), seen_declared'')
      | Geq (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Geq (e1', e2'), seen_declared'')
      | And (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (And (e1', e2'), seen_declared'')
      | Or (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (Or (e1', e2'), seen_declared'')
      | Not e1 ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          return (Not e1', seen_declared')
      | If (e1, e2, e3) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          let%bind e3', seen_declared''' = walk e3 seen_declared'' in
          return (If (e1', e2', e3'), seen_declared''')
      | Record mapping ->
          let%bind mapping', seen_declared' =
            mapping |> Ident_map.enum |> List.of_enum
            |> list_fold_left_m
                 (fun (acc, seen) (lbl, expr) ->
                   let%bind expr', seen' = walk expr seen in
                   return ((lbl, expr') :: acc, seen'))
                 ([], seen_declared)
            |> lift1 (fun (acc, seen) ->
                   (Ident_map.of_enum @@ List.enum acc, seen))
          in
          return (Record mapping', seen_declared')
      | RecordProj (e1, lbl) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          return (RecordProj (e1', lbl), seen_declared')
      | Match (e0, cases) ->
          let%bind e0', seen_declared' = walk e0 seen_declared in
          let%bind cases_rev, seen_declared'' =
            list_fold_left_m
              (fun (acc, seen) (pat, body) ->
                let%bind body', seen' = walk body seen in
                let var_list = Ident_set.to_list @@ pat_vars pat in
                let%bind _, body'', seen'', renaming =
                  ensure_expr_unique_names var_list body' seen'
                in
                let pat' = pat_rename_vars renaming pat in
                return ((pat', body'') :: acc, seen''))
              ([], seen_declared') cases
          in
          let cases' = List.rev cases_rev in
          return (Match (e0', cases'), seen_declared'')
      | VariantExpr (lbl, e1) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          return (VariantExpr (lbl, e1'), seen_declared')
      | List es ->
          let%bind es'rev, seen_declared' =
            es
            |> list_fold_left_m
                 (fun (ret, seen) e ->
                   let%bind e', seen' = walk e seen in
                   return (e' :: ret, seen'))
                 ([], seen_declared)
          in
          return (List (List.rev es'rev), seen_declared')
      | ListCons (e1, e2) ->
          let%bind e1', seen_declared' = walk e1 seen_declared in
          let%bind e2', seen_declared'' = walk e2 seen_declared' in
          return (ListCons (e1', e2'), seen_declared'')
      | Assert e ->
          let%bind e', seen_declared' = walk e seen_declared in
          return (Assert e', seen_declared')
      | Assume e ->
          let%bind e', seen_declared' = walk e seen_declared in
          return (Assume e', seen_declared')
    in
    return (expr', seen_declared')
  in
  lift1 fst @@ walk e Ident_set.empty

(* **** Expression flattening **** *)

(** Create new odefa variable with mapping to natodefa expr *)

let new_odefa_var (expr : On_ast.expr) (var_name : string) : Ast.var m =
  let%bind var = fresh_var var_name in
  let%bind () = add_odefa_natodefa_mapping var expr in
  return var

let new_odefa_inst_var (expr : On_ast.expr) (var_name : string) : Ast.var m =
  let%bind var = new_odefa_var expr var_name in
  let%bind () = add_instrument_var var in
  return var

(** Returns the body of a function or conditional with its return variable *)
let nonempty_body (expr : On_ast.expr) ((body, var) : Ast.clause list * Ast.var)
    : (Ast.clause list * Ast.var) m =
  match body with
  | [] ->
      let%bind x = fresh_var "var" in
      let%bind () = add_odefa_natodefa_mapping x expr in
      return @@ ([ Ast.Clause (x, Var_body var) ], x)
  | _ -> return (body, var)

(** Create a new abort clause with multiple conditional clause variables *)
let add_abort_expr (expr : On_ast.expr) (_ : Ast.var list) : Ast.expr m =
  let%bind abort_var = fresh_var "ab" in
  let%bind () = add_odefa_natodefa_mapping abort_var expr in
  let abort_clause = Ast.Clause (abort_var, Abort_body) in
  return @@ Ast.Expr [ abort_clause ]

let on_to_odefa_ident (On_ast.Ident id) = Ast.Ident id

(** Flatten a pattern. The second value in the pair is a list of projection
    clauses associated with a record or variable pattern; these will be appended
    to the front of the pattern's expression. *)
let flatten_pattern (expr : On_ast.expr) (pat_var : Ast.var)
    (pattern : On_ast.pattern) : (Ast.pattern * Ast.clause list) m =
  match pattern with
  | On_ast.AnyPat -> return (Ast.Any_pattern, [])
  | On_ast.IntPat -> return (Ast.Int_pattern, [])
  | On_ast.BoolPat -> return (Ast.Bool_pattern, [])
  | On_ast.FunPat -> return (Ast.Fun_pattern, [])
  | On_ast.RecPat rec_pat ->
      let rec_pat' =
        rec_pat |> On_ast.Ident_map.keys |> Enum.map on_to_odefa_ident
        |> Ast.Ident_set.of_enum
      in
      let%bind projections =
        rec_pat |> On_ast.Ident_map.enum |> List.of_enum
        |> list_fold_left_m
             (fun acc (lbl, var) ->
               match var with
               | Some v ->
                   let v' = on_to_odefa_ident v in
                   let lbl' = on_to_odefa_ident lbl in
                   let ast_var = Ast.Var (v', None) in
                   let%bind () = add_odefa_natodefa_mapping ast_var expr in
                   let%bind () = add_instrument_var ast_var in
                   let ast_body = Ast.Projection_body (pat_var, lbl') in
                   return @@ acc @ [ Ast.Clause (ast_var, ast_body) ]
               | None -> return acc)
             []
      in
      return (Ast.Rec_pattern rec_pat', projections)
  | On_ast.VarPat var_pat ->
      let (On_ast.Ident var_id) = var_pat in
      let ast_var = Ast.Var (Ident var_id, None) in
      let%bind () = add_odefa_natodefa_mapping ast_var expr in
      let ast_body = Ast.Var_body pat_var in
      let clause = Ast.Clause (ast_var, ast_body) in
      return (Ast.Any_pattern, [ clause ])
  | On_ast.VariantPat _ ->
      raise
      @@ Utils.Invariant_failure
           "flatten_pattern: Variants patterns should have been encoded!"
  | On_ast.EmptyLstPat | On_ast.LstDestructPat _ ->
      raise
      @@ Utils.Invariant_failure
           "flatten_pattern: List patterns should have been encoded!"

(** Flatten a function *)
let flatten_fun ?(binding_name = (None : On_ast.Ident.t option))
    (expr : On_ast.expr) (param_names : On_ast.Ident.t list) (body : Ast.expr) :
    (Ast.expr * Ast.var) m =
  list_fold_right_m
    (fun (param : On_ast.Ident.t) ((e : Ast.expr), (_ : Ast.Var.t)) ->
      let id = on_to_odefa_ident param in
      let%bind (new_var : Ast.var) =
        match binding_name with
        | None -> fresh_var "flatten_fun"
        | Some (Ident s) -> fresh_var s
      in
      let%bind () = add_odefa_natodefa_mapping new_var expr in
      let fun_val = Ast.Value_function (Function_value (Var (id, None), e)) in
      let fun_body = Ast.Value_body fun_val in
      let new_clause = Ast.Clause (new_var, fun_body) in
      let expr' : Ast.expr = Ast.Expr [ new_clause ] in
      return (expr', new_var))
    param_names
    (body, retv body)

(** Flatten a binary operation *)
let rec flatten_binop (expr : On_ast.expr) (e1 : On_ast.expr) (e2 : On_ast.expr)
    (binop : Ast.binary_operator) : (Ast.clause list * Ast.var) m =
  let%bind e1_clist, e1_var = flatten_expr e1 in
  let%bind e2_clist, e2_var = flatten_expr e2 in
  let%bind binop_var = fresh_var "binop" in
  let%bind () = add_odefa_natodefa_mapping binop_var expr in
  let binop_body = Ast.Binary_operation_body (e1_var, binop, e2_var) in
  let new_clause = Ast.Clause (binop_var, binop_body) in
  return (e1_clist @ e2_clist @ [ new_clause ], binop_var)

(** Flatten either the equal or not equal binary operation. This involves
    instrumenting both operations in nested conditionals. *)
and flatten_eq_binop (expr : On_ast.expr) (e1 : On_ast.expr) (e2 : On_ast.expr)
    (binop_int : Ast.binary_operator) (binop_bool : Ast.binary_operator) :
    (Ast.clause list * Ast.var) m =
  (* e1 and e2 *)
  let%bind e1_clist, e1_var = flatten_expr e1 in
  let%bind e2_clist, e2_var = flatten_expr e2 in
  (* Helper functions *)
  let add_var = new_odefa_inst_var expr in
  let create_match_clause pat pat_name =
    let%bind m_bl = add_var ("m_eq_binop_l_" ^ pat_name) in
    let%bind m_br = add_var ("m_eq_binop_r_" ^ pat_name) in
    let%bind m_b = add_var ("m_eq_binop_" ^ pat_name) in
    let m_clause_l = Ast.Clause (m_bl, Match_body (e1_var, pat)) in
    let m_clause_r = Ast.Clause (m_br, Match_body (e2_var, pat)) in
    let m_clause =
      Ast.Clause (m_b, Binary_operation_body (m_bl, Binary_operator_and, m_br))
    in
    return ([ m_clause_l; m_clause_r; m_clause ], m_b)
  in
  (* Int matching: ml = x ~ int; mr = y ~ int; m1 = ml and mr *)
  let%bind clist_int, m_int = create_match_clause Ast.Int_pattern "int" in
  (* Bool matching: ml = x ~ bool; mr = y ~ bool; m1 = ml and mr *)
  let%bind clist_bool, m_bool = create_match_clause Ast.Bool_pattern "bool" in
  (* Instrumentation predicate: m = m1 or m2 *)
  let%bind m_b = add_var "m_eq_binop" in
  let m_clause_body =
    Ast.Binary_operation_body (m_int, Binary_operator_or, m_bool)
  in
  let m_clause = Ast.Clause (m_b, m_clause_body) in
  let clist_predicates = clist_int @ clist_bool @ [ m_clause ] in
  (* Inner conditionals *)
  let create_conditional pred pat_name t_operator f_expr =
    let%bind c_binop' = add_var ("eq_binop_" ^ pat_name ^ "_inner") in
    let%bind c_binop = add_var ("eq_binop_" ^ pat_name) in
    let t_expr =
      Ast.Expr
        [
          Clause (c_binop', Binary_operation_body (e1_var, t_operator, e2_var));
        ]
    in
    return @@ Ast.Clause (c_binop, Conditional_body (pred, t_expr, f_expr))
  in
  (* Bool conditional: m2 ? ( x xnor y ) : ( abort ) *)
  let%bind inner_abort = add_abort_expr expr [] in
  let%bind cond_bool =
    create_conditional m_bool "bool" binop_bool inner_abort
  in
  (* Int conditional: m1 ? ( x == y ) : ( [see above] ) *)
  let%bind cond_int =
    create_conditional m_int "int" binop_int (Ast.Expr [ cond_bool ])
  in
  (* Conditional: m ? ( [see above] ) : ( abort ) *)
  let%bind c_binop = add_var "eq_binop" in
  let%bind outer_abort = add_abort_expr expr [ c_binop ] in
  let cond =
    Ast.Clause
      (c_binop, Conditional_body (m_b, Ast.Expr [ cond_int ], outer_abort))
  in
  (* Putting it all together *)
  return (e1_clist @ e2_clist @ clist_predicates @ [ cond ], c_binop)

and flatten_pattern_match (expr : On_ast.expr) (subj_var : Ast.var)
    (pat_e_list : (On_ast.pattern * On_ast.expr) list) :
    (Ast.clause list * Ast.var) m =
  let rec convert_match ((pat, e) : On_ast.pattern * On_ast.expr)
      (accum : Ast.expr * Ast.clause list) : (Ast.expr * Ast.clause list) m =
    (* Conditional expression *)
    let cond_expr_inner, match_cls_list_tail = accum in
    (* Variables *)
    let%bind bool_var = new_odefa_inst_var expr "m_match_bool" in
    let%bind cond_var = new_odefa_inst_var expr "m_match_cond" in
    (* Clauses and expressions *)
    let%bind flat_pat, new_clauses = flatten_pattern expr subj_var pat in
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
  let%bind innermost = add_abort_expr expr [] in
  let%bind cond_expr, match_cls_list =
    list_fold_right_m convert_match pat_e_list (innermost, [])
  in
  (* Predicates - one big disjunction of atomic formulae *)
  let create_or_clause cls_1 cls_2 =
    let (Ast.Clause (m_var_1, _)) = cls_1 in
    let (Ast.Clause (m_var_2, _)) = cls_2 in
    let%bind m_match_or = new_odefa_inst_var expr "m_match_or" in
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
  let (Ast.Clause (match_pred, _)) =
    List.hd pred_cls_list (* Never raises b/c pat_e_list must be nonempty *)
  in
  let%bind cond_var = new_odefa_inst_var expr "match" in
  let%bind abort_expr = add_abort_expr expr [ cond_var ] in
  let cond_cls =
    Ast.Clause (cond_var, Conditional_body (match_pred, cond_expr, abort_expr))
  in
  return (List.rev pred_cls_list @ [ cond_cls ], cond_var)

(** Flatten an entire expression (i.e. convert natodefa into odefa code) *)
and flatten_expr (expr : On_ast.expr) : (Ast.clause list * Ast.var) m =
  (* let%bind () = update_natodefa_expr exp in *)
  match expr with
  | Var id ->
      let%bind alias_var = fresh_var "var" in
      let (Ident i_string) = id in
      let id_var = Ast.Var (Ident i_string, None) in
      let%bind () = add_odefa_natodefa_mapping alias_var expr in
      let%bind () = add_odefa_natodefa_mapping id_var expr in
      return ([ Ast.Clause (alias_var, Var_body id_var) ], alias_var)
  | Input ->
      let%bind input_var = fresh_var "input" in
      let%bind () = add_odefa_natodefa_mapping input_var expr in
      return ([ Ast.Clause (input_var, Input_body) ], input_var)
  | Function (id_list, e) ->
      let%bind fun_c_list, _ = nonempty_body expr @@@ flatten_expr e in
      let body_expr = Ast.Expr fun_c_list in
      let%bind Expr fun_clause, return_var =
        flatten_fun expr id_list body_expr
      in
      return (fun_clause, return_var)
  | Appl (e1, e2) ->
      let%bind e1_clist, e1_var = flatten_expr e1 in
      let%bind e2_clist, e2_var = flatten_expr e2 in
      let%bind appl_var = fresh_var "appl" in
      let%bind () = add_odefa_natodefa_mapping appl_var expr in
      let new_clause = Ast.Clause (appl_var, Ast.Appl_body (e1_var, e2_var)) in
      return (e1_clist @ e2_clist @ [ new_clause ], appl_var)
  | Let (var_ident, e1, e2) ->
      let%bind e1_clist, e1_var = flatten_expr e1 in
      let%bind e2_clist, e2_var = flatten_expr e2 in
      let (Ident var_name) = var_ident in
      let lt_var = Ast.Var (Ident var_name, None) in
      let%bind () = add_odefa_natodefa_mapping lt_var expr in
      let assignment_clause = Ast.Clause (lt_var, Var_body e1_var) in
      return (e1_clist @ [ assignment_clause ] @ e2_clist, e2_var)
  | LetFun (sign, e) ->
      (* TODO: check for bugs!!! *)
      (* Translating the function signature... *)
      let (Funsig (fun_name, id_list, fun_e)) = sign in
      let%bind body_clist, _ = nonempty_body expr @@@ flatten_expr fun_e in
      let%bind Expr fun_clauses, return_var =
        flatten_fun ~binding_name:(Some fun_name) expr id_list (Expr body_clist)
      in
      (* Flattening the "e2"... *)
      let%bind e_clist, e_var = flatten_expr e in
      (* Assigning the function to the given function name... *)
      let (On_ast.Ident var_name) = fun_name in
      let lt_var = Ast.Var (Ident var_name, None) in
      let%bind () = add_odefa_natodefa_mapping lt_var expr in
      let assignment_clause = Ast.Clause (lt_var, Var_body return_var) in
      return (fun_clauses @ [ assignment_clause ] @ e_clist, e_var)
  | LetRecFun (_, _) ->
      raise
      @@ Utils.Invariant_failure
           "LetRecFun should not have been passed to flatten_expr"
  | Plus (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_plus
  | Minus (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_minus
  | Times (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_times
  | Divide (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_divide
  | Modulus (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_modulus
  (* TODO: check whether it's in instrumentation or not *)
  | Equal (e1, e2) ->
      flatten_binop expr e1 e2 Ast.Binary_operator_equal_to
      (* flatten_eq_binop expr e1 e2 Ast.Binary_operator_equal_to
         Ast.Binary_operator_equal_to *)
  | Neq (e1, e2) ->
      flatten_binop expr e1 e2 Ast.Binary_operator_not_equal_to
      (* flatten_eq_binop expr e1 e2 Ast.Binary_operator_not_equal_to
         Ast.Binary_operator_xor *)
  | LessThan (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_less_than
  | Leq (e1, e2) ->
      flatten_binop expr e1 e2 Ast.Binary_operator_less_than_or_equal_to
  | GreaterThan (e1, e2) ->
      (* Reverse e1 and e2 *)
      flatten_binop expr e2 e1 Ast.Binary_operator_less_than
  | Geq (e1, e2) ->
      (* Reverse e1 and e2 *)
      flatten_binop expr e2 e1 Ast.Binary_operator_less_than_or_equal_to
  | And (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_and
  | Or (e1, e2) -> flatten_binop expr e1 e2 Ast.Binary_operator_or
  | Not e ->
      let%bind e_clist, e_var = flatten_expr e in
      let%bind true_var = fresh_var "true" in
      let%bind binop_var = fresh_var "binop" in
      let%bind () = add_odefa_natodefa_mapping true_var (Bool true) in
      let%bind () = add_odefa_natodefa_mapping binop_var expr in
      let binop = Ast.Binary_operator_xor in
      let true_body = Ast.Value_body (Value_bool true) in
      let binop_body = Ast.Binary_operation_body (e_var, binop, true_var) in
      let true_clause = Ast.Clause (true_var, true_body) in
      let binop_clause = Ast.Clause (binop_var, binop_body) in
      return (e_clist @ [ true_clause; binop_clause ], binop_var)
  | If (e1, e2, e3) ->
      (* TODO: there will be another version of a conditional where we can
         do pattern matching. *)
      (* NOTE: this is translation from an if statement. Thus e1 will be always
         matched with true. *)
      let%bind e1_clst, e1_var = flatten_expr e1 in
      let%bind e2_clst, _ = nonempty_body expr @@@ flatten_expr e2 in
      let%bind e3_clst, _ = nonempty_body expr @@@ flatten_expr e3 in
      let%bind if_var = fresh_var "if" in
      let%bind () = add_odefa_natodefa_mapping if_var expr in
      let if_body = Ast.Conditional_body (e1_var, Expr e2_clst, Expr e3_clst) in
      let if_clause = Ast.Clause (if_var, if_body) in
      return (e1_clst @ [ if_clause ], if_var)
  | Int n ->
      let%bind int_var = fresh_var "int" in
      let%bind () = add_odefa_natodefa_mapping int_var expr in
      let new_clause = Ast.Clause (int_var, Ast.Value_body (Ast.Value_int n)) in
      return ([ new_clause ], int_var)
  | Bool b ->
      let%bind bool_var = fresh_var "bool" in
      let%bind () = add_odefa_natodefa_mapping bool_var expr in
      let new_clause =
        Ast.Clause (bool_var, Ast.Value_body (Ast.Value_bool b))
      in
      return ([ new_clause ], bool_var)
  | Record recexpr_map ->
      (* function for Enum.fold that generates the clause list and the
         id -> var map for Odefa's record *)
      let flatten_and_map acc ident_expr_tuple :
          (Ast.clause list * Ast.var Ast.Ident_map.t) m =
        let clist, recmap = acc in
        let id, e = ident_expr_tuple in
        let (On_ast.Ident id_string) = id in
        let ast_id = Ast.Ident id_string in
        let%bind e_clist, e_var = flatten_expr e in
        let new_clist = clist @ e_clist in
        let new_map = Ast.Ident_map.add ast_id e_var recmap in
        return (new_clist, new_map)
      in
      let empty_acc = ([], Ast.Ident_map.empty) in
      let%bind clist, map =
        On_ast.Ident_map.enum recexpr_map
        |> List.of_enum
        |> list_fold_left_m flatten_and_map empty_acc
      in
      let%bind rec_var = fresh_var "record" in
      let%bind () = add_odefa_natodefa_mapping rec_var expr in
      let new_body = Ast.Value_body (Ast.Value_record (Ast.Record_value map)) in
      let new_clause = Ast.Clause (rec_var, new_body) in
      return (clist @ [ new_clause ], rec_var)
  | RecordProj (rec_expr, lab) ->
      let%bind e_clist, e_var = flatten_expr rec_expr in
      let (On_ast.Label l_string) = lab in
      let l_ident = Ast.Ident l_string in
      let%bind proj_var = fresh_var "proj" in
      let%bind () = add_odefa_natodefa_mapping proj_var expr in
      let new_clause =
        Ast.Clause (proj_var, Ast.Projection_body (e_var, l_ident))
      in
      return (e_clist @ [ new_clause ], proj_var)
  | Match (subject, pat_e_list) ->
      (* We need to flatten the subject first *)
      let%bind subject_clause_list, subj_var = flatten_expr subject in
      (* Flatten the pattern-expr list *)
      let%bind match_clause_list, cond_var =
        flatten_pattern_match expr subj_var pat_e_list
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
  | Assert e ->
      let%bind flattened_exprs, last_var = flatten_expr e in
      (* Helper function *)
      let add_var var_name =
        let%bind var = fresh_var var_name in
        let%bind () = add_odefa_natodefa_mapping var expr in
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
      let%bind f_path = add_abort_expr expr [ assert_result ] in
      let cond_clause =
        Ast.Clause
          (assert_result, Conditional_body (assert_pred, t_path, f_path))
      in
      let all_clauses = flattened_exprs @ [ alias_clause; cond_clause ] in
      return (all_clauses, assert_result)
  | Assume e ->
      let%bind flattened_exprs, last_var = flatten_expr e in
      let%bind assume_var = fresh_var "assume" in
      let%bind () = add_odefa_natodefa_mapping assume_var expr in
      let new_clause = Ast.Clause (assume_var, Assume_body last_var) in
      return (flattened_exprs @ [ new_clause ], assume_var)

let debug_transform_on (trans_name : string) (transform : 'a -> On_ast.expr m)
    (e : 'a) : On_ast.expr m =
  let%bind e' = transform e in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (On_ast_pp.show_expr e')) ;
  return e'

let debug_transform_odefa (trans_name : string)
    (transform : 'a -> Ast.clause list m) (e : 'a) : Ast.clause list m =
  let%bind c_list = transform e in
  let e' = Ast.Expr c_list in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Ast_pp.show_expr e')) ;
  return c_list

let translate ?(translation_context = None) ?(is_instrumented = false)
    (e : On_ast.expr) : Ast.expr * On_to_odefa_maps.t =
  let (e_m_with_info : (Ast.expr * On_to_odefa_maps.t) m) =
    (* Odefa transformations *)
    let flatten e : Ast.clause list m =
      let%bind c_list, _ = flatten_expr e in
      return c_list
    in
    let instrument c_list : Ast.clause list m =
      if is_instrumented
      then Odefa_instrumentation.instrument_clauses c_list
      else return c_list
    in
    let add_first_result c_list : Ast.clause list m =
      return c_list >>= Odefa_instrumentation.add_first_var
      >>= Odefa_instrumentation.add_result_var
    in
    (* Translation sequence *)
    lazy_logger `debug (fun () ->
        Printf.sprintf "Initial program:\n%s" (On_ast_pp.show_expr e)) ;
    let%bind translation_result =
      return e
      >>= debug_transform_on "desugaring" preliminary_encode_expr
      >>= debug_transform_on "alphatization" alphatize
      >>= debug_transform_odefa "flattening" flatten
      >>= debug_transform_odefa "instrumentation" instrument
      >>= debug_transform_odefa "adding ~result" add_first_result
    in
    let%bind odefa_on_maps = odefa_natodefa_maps in
    lazy_logger `debug (fun () ->
        Printf.sprintf "Odefa to natodefa maps:\n%s"
          (On_to_odefa_maps.show odefa_on_maps)) ;
    return (Ast.Expr translation_result, odefa_on_maps)
  in
  (* Set up context and run *)
  let context =
    match translation_context with
    | None -> new_translation_context ~is_natodefa:true ()
    | Some ctx -> ctx
  in
  run context e_m_with_info
(* NOTE: commenting this out for DDSE because it has a tendency to eliminate
   unnecessary variables and we use those as targets *)
(* |> eliminate_aliases *)
