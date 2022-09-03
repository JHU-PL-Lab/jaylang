open Batteries
open Jhupllib
open Jayil
open Jay
open Jay_instrumentation
open Ast_tools
open Jay_to_jayil_preliminary
open Jay_to_jayil_monad

(** In this module we will translate from natodefa to odefa in the following
    order: * Desugar let rec, lists, variants, and list/variant patterns *
    Alphatize program again (do this after to allow above to introduce dupes) *
    Flatten natodefa expressions to odefa expressions * Instrument odefa with
    type/error constriants *)

open TranslationMonad

let lazy_logger = Logger_utils.make_lazy_logger "Jay_to_jayil"

(* let show_expr = Pp_utils.pp_to_string Jay_ast_pp.pp_expr;; *)

(* **** Variable alphatization **** *)

(** Determines all variables contained within a pattern. *)
let rec pat_vars (pat : Jay_ast.pattern) : Jay_ast.Ident_set.t =
  let open Jay_ast in
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
let pat_rename_vars (name_map : Jay_ast.Ident.t Jay_ast.Ident_map.t)
    (pattern : Jay_ast.pattern) : Jay_ast.pattern =
  let open Jay_ast in
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

(* TODO: Probably should change the type of the function. Operate on expr_desc directly? *)
(* Doesn't create new subtrees -- should preserve the tags just fine *)

(** Performs alpha substitution on a given expression. *)
let rec rename_variable (old_name : Jay_ast.ident) (new_name : Jay_ast.ident)
    (e_desc : Jay_ast.expr_desc) : Jay_ast.expr_desc =
  let open Jay_ast in
  (* NOTE: the generic homomorphism routine m_env_transform_expr does not allow
     us to change the environment of the homomorphism as we descend or to block
     descending into a given subtree, so we can't use it here. *)
  let recurse = rename_variable old_name new_name in
  let e = e_desc.body in
  let renamed_expr =
    match e with
    | Int _ | Bool _ | Input | Error _ -> e
    | Var id -> if id = old_name then Var new_name else Var id
    | Function (id_list, e') ->
        if List.exists (Ident.equal old_name) id_list
        then e
        else
          let e'' = recurse e' in
          Function (id_list, e'')
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
                else
                  let new_body = recurse body in
                  Funsig (name, params, new_body))
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
    | Appl (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Appl (e1', e2')
    | Plus (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Plus (e1', e2')
    | Minus (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Minus (e1', e2')
    | Times (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Times (e1', e2')
    | Divide (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Divide (e1', e2')
    | Modulus (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Modulus (e1', e2')
    | Equal (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Equal (e1', e2')
    | Neq (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Neq (e1', e2')
    | LessThan (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        LessThan (e1', e2')
    | Leq (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Leq (e1', e2')
    | GreaterThan (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        GreaterThan (e1', e2')
    | Geq (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Geq (e1', e2')
    | And (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        And (e1', e2')
    | Or (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        Or (e1', e2')
    | Not e1 ->
        let e1' = recurse e1 in
        Not e1'
    | If (e1, e2, e3) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        let e3' = recurse e3 in
        If (e1', e2', e3')
    | Record m -> Record (Ident_map.map (fun ed -> recurse ed) m)
    | RecordProj (e1, lbl) ->
        let e1' = recurse e1 in
        RecordProj (e1', lbl)
    | VariantExpr (lbl, e1) ->
        let e1' = recurse e1 in
        VariantExpr (lbl, e1')
    | List es -> List (List.map (fun ed -> recurse ed) es)
    | ListCons (e1, e2) ->
        let e1' = recurse e1 in
        let e2' = recurse e2 in
        ListCons (e1', e2')
    | Assert e ->
        let e' = recurse e in
        Assert e'
    | Assume e ->
        let e' = recurse e in
        Assume e'
  in
  new_expr_desc renamed_expr

(** This function alphatizes an entire expression. If a variable is defined more
    than once in the given expression, all but one of the declarations will be
    alpha-renamed to a fresh name. *)
let alphatize (e : Jay_ast.expr_desc) : Jay_ast.expr_desc m =
  let open Jay_ast in
  (* Given a list of identifiers, a list of expressions, and a list of
     previously declared identifiers, this helper routine renames all previously
     declared identifiers which appear in the list within all of the
     expressions.  The returned values are the renamed list of identifiers,
     the renamed expressions, the new set of declared identifiers, and a
     dictionary mapping the identifiers which were renamed onto their new
     values. *)
  (* This function also shouldn't create any new subtrees, thus safe to keep og tags *)
  let rec ensure_exprs_unique_names (names : Ident.t list)
      (exprs : expr_desc list) (prev_declared : Ident_set.t) :
      (Ident.t list * expr_desc list * Ident_set.t * Ident.t Ident_map.t) m =
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
          let%bind () = add_jay_var_mapping new_name name in
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
  let rec walk (expr_desc : expr_desc) (seen_declared : Ident_set.t) :
      (expr_desc * Ident_set.t) m =
    let zero () =
      raise @@ Jhupllib_utils.Invariant_failure "list changed size"
    in
    let%bind expr', seen_declared' =
      let expr = expr_desc.body in
      match expr with
      (* In leaf cases, no new variables are defined and so we have no work to
         do. *)
      | Var _ | Input | Int _ | Bool _ | Error _ -> return (expr, seen_declared)
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
    let expr_desc' = { tag = expr_desc.tag; body = expr' } in
    return (expr_desc', seen_declared')
  in
  lift1 fst @@ walk e Ident_set.empty

(* **** Expression flattening **** *)

(** Create new odefa variable with mapping to natodefa expr *)

let new_odefa_var (e_desc : Jay_ast.expr_desc) (var_name : string) : Ast.var m =
  let%bind var = fresh_var var_name in
  let%bind () = add_jayil_jay_mapping var e_desc in
  return var

let new_odefa_inst_var (e_desc : Jay_ast.expr_desc) (var_name : string) :
    Ast.var m =
  let%bind var = new_odefa_var e_desc var_name in
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
    let%bind bool_var = new_odefa_inst_var expr_desc "m_match_bool" in
    let%bind cond_var = new_odefa_inst_var expr_desc "m_match_cond" in
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
    let%bind m_match_or = new_odefa_inst_var expr_desc "m_match_or" in
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
  let%bind cond_var = new_odefa_inst_var expr_desc "match" in
  let%bind abort_expr = add_abort_expr expr_desc [ cond_var ] in
  let cond_cls =
    Ast.Clause (cond_var, Conditional_body (match_pred, cond_expr, abort_expr))
  in
  return (List.rev pred_cls_list @ [ cond_cls ], cond_var)

(** Flatten an entire expression (i.e. convert natodefa into odefa code) *)
and flatten_expr (expr_desc : Jay_ast.expr_desc) : (Ast.clause list * Ast.var) m
    =
  (* let%bind () = update_natodefa_expr exp in *)
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
         id -> var map for Odefa's record *)
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

let debug_transform_on (trans_name : string)
    (transform : 'a -> Jay_ast.expr_desc m) (e : 'a) : Jay_ast.expr_desc m =
  let%bind e' = transform e in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Jay_ast.show_expr e'.body)) ;
  return e'

let debug_transform_odefa (trans_name : string)
    (transform : 'a -> Ast.clause list Jay_to_jayil_monad.TranslationMonad.m)
    (e : 'a) : Ast.clause list m =
  let%bind c_list = transform e in
  let e' = Ast.Expr c_list in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Ast_pp.show_expr e')) ;
  return c_list

let debug_transform_odefa' (trans_name : string)
    (transform :
      'a -> Ast.clause list Jay_to_jayil_monad_inst.TranslationMonad.m) (e : 'a)
    : Ast.clause list Jay_to_jayil_monad_inst.TranslationMonad.m =
  let open Jay_to_jayil_monad_inst.TranslationMonad in
  let%bind c_list = transform e in
  let e' = Ast.Expr c_list in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Ast_pp.show_expr e')) ;
  return c_list

let translate ?(translation_context = None) ?(is_instrumented = true)
    (e : Jay_ast.expr_desc) :
    Ast.expr
    * Jay_instrumentation.Jayil_instrumentation_maps.t
    * Jay_to_jayil_maps.t =
  let (e_m_with_info, ctx)
        : Ast.expr Jay_to_jayil_monad_inst.TranslationMonad.m
          * Jay_to_jayil_monad.translation_context =
    (* Translation from Natodefa to Odefa *)
    let open Jay_to_jayil_monad.TranslationMonad in
    let flatten e =
      let%bind c_list, _ = flatten_expr e in
      return c_list
    in
    (* Phase one - translation *)
    (* Step one: Encode lists, variants, match, and let rec
       Step two: Alphatize the expressions
       Step three: Flatten to a-normalized form (Natodefa -> Odefa)
    *)
    lazy_logger `debug (fun () ->
        Printf.sprintf "Initial program:\n%s" (Jay_ast.show_expr e.body)) ;
    let (translation_result_p1_m : Ast.clause list m) =
      return e
      >>= debug_transform_on "desugaring" preliminary_encode_expr
      >>= debug_transform_on "alphatization" alphatize
      >>= debug_transform_odefa "flattening" flatten
    in
    let context =
      match translation_context with
      | None -> new_translation_context ~is_jay:true ()
      | Some ctx -> ctx
    in
    let translation_result_p1, ctx =
      run_verbose context translation_result_p1_m
    in
    (* End of phase one *)
    (* Phase two: Instrumentation *)
    (* In this phase, if the instrumentation flag is set to true, we will add
       first-order instrumentation checks, as well as a first and a final
       variable.
    *)
    let open Jay_to_jayil_monad_inst.TranslationMonad in
    let instrument c_list : Ast.clause list m =
      if is_instrumented
      then Instrumentation.instrument_clauses c_list
      else return c_list
    in
    let add_first_result c_list : Ast.clause list m =
      return c_list >>= Instrumentation.add_first_var
      >>= Instrumentation.add_result_var
    in
    let (translation_result_p2_m : Ast.clause list m) =
      return translation_result_p1
      >>= debug_transform_odefa' "instrumentation" instrument
      >>= debug_transform_odefa' "adding ~result" add_first_result
    in
    let res =
      translation_result_p2_m >>= fun m ->
      Jay_to_jayil_monad_inst.TranslationMonad.return (Ast.Expr m)
    in
    (res, ctx)
  in
  (* Set up context and run *)
  let natodefa_inst_map =
    Jay_to_jayil_maps.get_natodefa_inst_map ctx.tc_odefa_natodefa_mappings
  in
  let init_ctx_ph2 =
    Jay_to_jayil_monad_inst.new_translation_context_from_natodefa
      natodefa_inst_map
  in
  let ctx' =
    { init_ctx_ph2 with tc_fresh_name_counter = ctx.tc_fresh_name_counter }
  in
  let res = Jay_to_jayil_monad_inst.TranslationMonad.run ctx' e_m_with_info in
  let odefa_on_maps = ctx.tc_odefa_natodefa_mappings in
  let inst_maps = ctx'.tc_odefa_instrumentation_mappings in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Odefa to natodefa maps:\n%s"
        (Jay_to_jayil_maps.show odefa_on_maps)) ;
  lazy_logger `debug (fun () ->
      Printf.sprintf "Odefa instrumentation maps:\n%s"
        (Jayil_instrumentation_maps.show inst_maps)) ;
  (res, inst_maps, odefa_on_maps)
