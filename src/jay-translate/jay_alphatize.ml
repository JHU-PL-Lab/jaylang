open Batteries
open Jay
open Jay_to_jayil_monad
open TranslationMonad

(* **** Variable alphatization **** *)

(** Determines all variables contained within a pattern. *)
let rec pat_vars (pat : Jay_ast.pattern) : Jay_ast.Ident_set.t =
  let open Jay_ast in
  match pat with
  | AnyPat | IntPat | BoolPat | FunPat -> Ident_set.empty
  | RecPat record | StrictRecPat record ->
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
  | StrictRecPat record ->
      let record' =
        record |> Ident_map.enum
        |> Enum.map (fun (lbl, x_opt) ->
               match x_opt with
               | Some x -> (lbl, Some (Ident_map.find_default x x name_map))
               | None -> (lbl, None))
        |> Ident_map.of_enum
      in
      StrictRecPat record'
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

(** Performs alpha substitution on a given expression. Doesn't create new
    subtrees -- should preserve the tags just fine *)
let rec rename_variable (old_name : Jay_ast.ident) (new_name : Jay_ast.ident)
    (e_desc : Jay_ast.expr_desc) : Jay_ast.expr_desc =
  let open Jay_ast in
  (* NOTE: the generic homomorphism routine m_env_transform_expr does not allow
     us to change the environment of the homomorphism as we descend or to block
     descending into a given subtree, so we can't use it here. *)
  let recurse = rename_variable old_name new_name in
  let tag = e_desc.tag in
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
  { tag; body = renamed_expr }

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
      raise @@ Jhupllib.Utils.Invariant_failure "list changed size"
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
