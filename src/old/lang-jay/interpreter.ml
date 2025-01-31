open Core
open Jay_ast

exception Evaluation_failure of string

let stdin_input_source () =
  let input =
    Out_channel.(flush stdout) ;
    Int.of_string In_channel.(input_line_exn stdin)
  in
  Int input

let rec substitute (x : ident) (subs : expr_desc) (og_expr : expr_desc) :
    expr_desc =
  let substitute_funsig (Funsig (f, xs, fe) as funsig) =
    if List.mem xs x ~equal:Ident.equal
    then funsig
    else Funsig (f, xs, substitute x subs fe)
  in
  match og_expr.body with
  | Var x' -> if Ident.equal x x' then subs else og_expr
  | Int _ | Bool _ | Input | Error _ -> og_expr
  | Function (xs, fe) ->
      if List.mem xs x ~equal:Ident.equal
      then og_expr
      else new_expr_desc @@ Function (xs, substitute x subs fe)
  | Appl (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Appl (e1', e2')
  | Let (x', e1, e2) ->
      if Ident.equal x x'
      then new_expr_desc @@ Let (x', substitute x subs e1, e2)
      else new_expr_desc @@ Let (x', substitute x subs e1, substitute x subs e2)
  | LetRecFun (funsigs, e) ->
      let funsigs' =
        List.map
          ~f:(fun (Funsig (f, _, _) as funsig) ->
            if Ident.equal x f then funsig else substitute_funsig funsig)
          funsigs
      in
      let fs = List.map ~f:(fun (Funsig (f, _, _)) -> f) funsigs in
      let e' =
        if List.mem fs x ~equal:Ident.equal then e else substitute x subs e
      in
      new_expr_desc @@ LetRecFun (funsigs', e')
  | LetFun ((Funsig (f, _, _) as funsig), e) ->
      let funsig' = substitute_funsig funsig in
      let e' = if Ident.equal f x then e else substitute x subs e in
      new_expr_desc @@ LetFun (funsig', e')
  | Plus (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Plus (e1', e2')
  | Minus (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Minus (e1', e2')
  | Times (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Times (e1', e2')
  | Divide (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Divide (e1', e2')
  | Modulus (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Modulus (e1', e2')
  | Equal (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Equal (e1', e2')
  | Neq (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Neq (e1', e2')
  | LessThan (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ LessThan (e1', e2')
  | Leq (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Leq (e1', e2')
  | GreaterThan (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ GreaterThan (e1', e2')
  | Geq (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Geq (e1', e2')
  | And (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ And (e1', e2')
  | Or (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ Or (e1', e2')
  | Not e1 ->
      let e1' = substitute x subs e1 in
      new_expr_desc @@ Not e1'
  | If (e1, e2, e3) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      let e3' = substitute x subs e3 in
      new_expr_desc @@ If (e1', e2', e3')
  | Record r ->
      let r' = Ident_map.map (fun ed -> substitute x subs ed) r in
      new_expr_desc @@ Record r'
  | RecordProj (r, lbl) ->
      let r' = substitute x subs r in
      new_expr_desc @@ RecordProj (r', lbl)
  | Match (e, pats) ->
      let shadow_in_pat x pat =
        match pat with
        | VarPat x' | VariantPat (_, x') -> Ident.equal x x'
        | LstDestructPat (x1, x2) -> Ident.equal x x1 || Ident.equal x x2
        | RecPat r | StrictRecPat r ->
            r
            |> Ident_map.exists (fun _ x'_opt ->
                   match x'_opt with
                   | Some x' -> Ident.equal x x'
                   | None -> false)
        | _ -> false
      in
      let e' = substitute x subs e in
      let pats' =
        List.map
          ~f:(fun (pat, pat_e) ->
            if shadow_in_pat x pat
            then (pat, pat_e)
            else (pat, substitute x subs pat_e))
          pats
      in
      new_expr_desc @@ Match (e', pats')
  | VariantExpr (v_lbl, e) ->
      let e' = substitute x subs e in
      new_expr_desc @@ VariantExpr (v_lbl, e')
  | List lst ->
      let lst' = List.map ~f:(substitute x subs) lst in
      new_expr_desc @@ List lst'
  | ListCons (e1, e2) ->
      let e1' = substitute x subs e1 in
      let e2' = substitute x subs e2 in
      new_expr_desc @@ ListCons (e1', e2')
  | Assert e1 ->
      let e1' = substitute x subs e1 in
      new_expr_desc @@ Assert e1'
  | Assume e1 ->
      let e1' = substitute x subs e1 in
      new_expr_desc @@ Assume e1'

let matches (e : expr_desc) (pat : pattern) : bool =
  match (e.body, pat) with
  | _, AnyPat -> true
  | _, VarPat _ -> true
  | Int _, IntPat -> true
  | Bool _, BoolPat -> true
  | Function _, FunPat -> true
  | Record r, RecPat pat_r ->
      let keys_r = Ident_map.key_list r in
      let keys_pat = Ident_map.key_list pat_r in
      List.for_all ~f:(fun x -> List.mem keys_pat x ~equal:Ident.equal) keys_r
  | Record r, StrictRecPat pat_r ->
      let keys_r = Ident_map.key_list r in
      let keys_pat = Ident_map.key_list pat_r in
      List.for_all ~f:(fun x -> List.mem keys_pat x ~equal:Ident.equal) keys_r
  | ( VariantExpr (Variant_label v_lbl, _),
      VariantPat (Variant_label expected_lbl, _) ) ->
      String.equal v_lbl expected_lbl
  | List [], EmptyLstPat -> true
  | List _, LstDestructPat _ -> true
  | _ -> false

let rec interp_jay ?(input_source = stdin_input_source) (e_desc : expr_desc) :
    expr_desc =
  let e = e_desc.body in
  match e with
  | Int _ | Bool _ | Function _ -> e_desc
  | Input -> new_expr_desc @@ input_source ()
  | Var x ->
      raise
      @@ Evaluation_failure
           ("Cannot find the variable " ^ Jay_ast_pp.show_ident x
          ^ " in the environment!")
  | Appl (e1, e2) -> (
      let e1 = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match e1.body with
      | Function (x :: _, fed) ->
          let res = substitute x e2' fed in
          interp_jay ~input_source res
      | _ -> raise @@ Evaluation_failure "Evaluation of non-function!")
  | Let (x, e1, e2) ->
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source (substitute x e1' e2) in
      e2'
  (* TODO: Might be buggy *)
  | LetRecFun (fun_sigs, e) ->
      let subs_fs =
        List.map
          ~f:(fun (Funsig (f, _, _)) ->
            let eta_x = Ident "eta_x" in
            let applf =
              new_expr_desc
              @@ Appl (new_expr_desc @@ Var f, new_expr_desc @@ Var eta_x)
            in
            let subst = new_expr_desc @@ LetRecFun (fun_sigs, applf) in
            let subst_f = new_expr_desc @@ Function ([ eta_x ], subst) in
            (f, subst_f))
          fun_sigs
      in
      let subs_fes =
        List.map
          ~f:(fun (Funsig (f, xs, fe)) ->
            let fe' =
              List.fold_left
                ~f:(fun acc (f, f_subst) -> substitute f f_subst acc)
                ~init:fe subs_fs
            in
            (f, new_expr_desc @@ Function (xs, fe')))
          fun_sigs
      in
      let e' =
        List.fold_left
          ~f:(fun acc (f, f_subst) -> substitute f f_subst acc)
          ~init:e subs_fes
      in
      interp_jay ~input_source e'
  | LetFun (Funsig (f, xs, fe), e) ->
      let subs_f = new_expr_desc @@ Function (xs, fe) in
      let e' = interp_jay ~input_source (substitute f subs_f e) in
      e'
  | Plus (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Int (n1 + n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Minus (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Int (n1 - n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Times (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Int (n1 * n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Divide (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Int (n1 / n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Modulus (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Int (n1 mod n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Equal (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (n1 = n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Neq (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (not (n1 = n2))
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | LessThan (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (n1 < n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Leq (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (n1 <= n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | GreaterThan (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (n1 > n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Geq (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Int n1, Int n2 -> new_expr_desc @@ Bool (n1 >= n2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | And (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Bool b1, Bool b2 -> new_expr_desc @@ Bool (b1 && b2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Or (e1, e2) -> (
      let e1' = interp_jay ~input_source e1 in
      let e2' = interp_jay ~input_source e2 in
      match (e1'.body, e2'.body) with
      | Bool b1, Bool b2 -> new_expr_desc @@ Bool (b1 || b2)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | Not e1 -> (
      let e1' = interp_jay ~input_source e1 in
      match e1'.body with
      | Bool b1 -> new_expr_desc @@ Bool (not b1)
      | _ ->
          raise
          @@ Evaluation_failure "Binary Operation: Operands should be integers!"
      )
  | If (e1, e2, e3) -> (
      let e1' = interp_jay ~input_source e1 in
      match e1'.body with
      | Bool true ->
          let e2' = interp_jay ~input_source e2 in
          e2'
      | Bool false ->
          let e3' = interp_jay ~input_source e3 in
          e3'
      | _ ->
          raise
          @@ Evaluation_failure
               "If Then Else: Condition should interp_jayuate to boolean!")
  | Record r ->
      let r' = Ident_map.map (fun ed -> interp_jay ~input_source ed) r in
      new_expr_desc @@ Record r'
  | RecordProj (e, Label lbl) -> (
      let e' = interp_jay ~input_source e in
      match e'.body with
      | Record r -> (
          let lbl_v_opt = Ident_map.find_opt (Ident lbl) r in
          match lbl_v_opt with
          | Some v -> v
          | None ->
              raise
              @@ Evaluation_failure
                   "Record Projection: Projection from non-existent field!")
      | _ ->
          raise
          @@ Evaluation_failure
               "Record Projection: Projecting from non-record value!")
  | Match (e, pats) -> (
      let resolve_match (e : expr_desc) (matched_pat : pattern)
          (matched_expr : expr_desc) : expr_desc =
        match (e.body, matched_pat) with
        | VariantExpr (_, v_expr), VariantPat (_, x) ->
            interp_jay ~input_source (substitute x v_expr matched_expr)
        | _, VarPat x -> interp_jay ~input_source (substitute x e matched_expr)
        | List lst_expr, LstDestructPat (hd, tl) ->
            let subs1 = substitute hd (List.hd_exn lst_expr) matched_expr in
            let subs2 =
              substitute tl (new_expr_desc @@ List (List.tl_exn lst_expr)) subs1
            in
            interp_jay ~input_source subs2
        | Record r, RecPat r_pat | Record r, StrictRecPat r_pat ->
            r_pat |> Ident_map.to_list
            |> List.filter_map ~f:(fun (lbl, x_opt) ->
                   match x_opt with
                   | None -> None
                   | Some x ->
                       let lookup = Ident_map.find lbl r in
                       Some (x, lookup))
            |> List.fold_left
                 ~f:(fun acc (x, subs_v) -> substitute x subs_v acc)
                 ~init:matched_expr
            |> interp_jay ~input_source
        (* TODO: Make the match more robust? *)
        | _ -> interp_jay ~input_source matched_expr
      in
      let e' = interp_jay ~input_source e in
      let matched = List.filter ~f:(fun (pat, _) -> matches e' pat) pats in
      match matched with
      | [] -> raise @@ Evaluation_failure "Pattern Match: No matching branch!"
      | (matched_pat, matched_expr) :: _ ->
          resolve_match e' matched_pat matched_expr)
  | VariantExpr (v_lbl, v_expr) ->
      let v_expr' = interp_jay ~input_source v_expr in
      new_expr_desc @@ VariantExpr (v_lbl, v_expr')
  | List exprs ->
      let exprs' = List.map ~f:(interp_jay ~input_source) exprs in
      new_expr_desc @@ List exprs'
  | ListCons (hd, tl) -> (
      let hd' = interp_jay ~input_source hd in
      let tl' = interp_jay ~input_source tl in
      match tl'.body with
      | List tl_lst -> new_expr_desc @@ List (hd' :: tl_lst)
      | _ ->
          raise
          @@ Evaluation_failure
               "List Cons: The tail should interp_jayuate to a list value!")
  | Assert e -> (
      let e' = interp_jay ~input_source e in
      match e'.body with
      | Bool true -> new_expr_desc @@ Bool true
      | _ ->
          raise
          @@ Evaluation_failure "Assert: Assertion interp_jayuates to false.")
  | Assume e -> (
      let e' = interp_jay ~input_source e in
      match e'.body with
      | Bool true -> new_expr_desc @@ Bool true
      | _ ->
          raise
          @@ Evaluation_failure "Assume: Assumption interp_jayuates to false.")
  | Error x ->
      raise
      @@ Evaluation_failure ("Error: Detected error at variable " ^ Ident.show x)
