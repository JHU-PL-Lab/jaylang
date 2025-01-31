open Core
open Jay_ast

let rec defined_vars_of_expr_desc (ed : expr_desc) : Ident_set.t =
  let e = ed.body in
  defined_vars_of_expr e

and defined_vars_of_expr (e : expr) : Ident_set.t =
  match e with
  | Int _ | Bool _ | Input -> Ident_set.empty
  | Var x | Error x -> Ident_set.singleton x
  | Function (params, ed) ->
      Ident_set.union (Ident_set.of_list params) (defined_vars_of_expr_desc ed)
  | Appl (ed1, ed2)
  | Plus (ed1, ed2)
  | Minus (ed1, ed2)
  | Times (ed1, ed2)
  | Divide (ed1, ed2)
  | Modulus (ed1, ed2)
  | Equal (ed1, ed2)
  | Neq (ed1, ed2)
  | LessThan (ed1, ed2)
  | Leq (ed1, ed2)
  | GreaterThan (ed1, ed2)
  | Geq (ed1, ed2)
  | And (ed1, ed2)
  | Or (ed1, ed2)
  | ListCons (ed1, ed2) ->
      let s1 = defined_vars_of_expr_desc ed1 in
      let s2 = defined_vars_of_expr_desc ed2 in
      Ident_set.union s1 s2
  | Not ed | RecordProj (ed, _) | VariantExpr (_, ed) | Assert ed | Assume ed ->
      defined_vars_of_expr_desc ed
  | If (ed1, ed2, ed3) ->
      let s1 = defined_vars_of_expr_desc ed1 in
      let s2 = defined_vars_of_expr_desc ed2 in
      let s3 = defined_vars_of_expr_desc ed3 in
      s1 |> Ident_set.union s2 |> Ident_set.union s3
  | Record r ->
      Ident_map.fold
        (fun _l ed acc -> Ident_set.union (defined_vars_of_expr_desc ed) acc)
        r Ident_set.empty
  | List eds ->
      List.fold
        ~f:(fun acc ed -> Ident_set.union (defined_vars_of_expr_desc ed) acc)
        ~init:Ident_set.empty eds
  | Match (med, pat_ed_lst) ->
      let s1 = defined_vars_of_expr_desc med in
      let s2 =
        List.fold
          ~f:(fun acc (_, ed) ->
            Ident_set.union (defined_vars_of_expr_desc ed) acc)
          ~init:Ident_set.empty pat_ed_lst
      in
      Ident_set.union s1 s2
  | Let (x, ed1, ed2) ->
      let s1 = defined_vars_of_expr_desc ed1 in
      let s2 = defined_vars_of_expr_desc ed2 in
      Ident_set.singleton x |> Ident_set.union s1 |> Ident_set.union s2
  | LetFun (fun_sig, ed) ->
      let s1 = defined_vars_of_funsig fun_sig in
      let s2 = defined_vars_of_expr_desc ed in
      Ident_set.union s1 s2
  | LetRecFun (fun_sigs, ed) ->
      let s1 =
        List.fold
          ~f:(fun acc fun_sig ->
            Ident_set.union (defined_vars_of_funsig fun_sig) acc)
          ~init:Ident_set.empty fun_sigs
      in
      let s2 = defined_vars_of_expr_desc ed in
      Ident_set.union s1 s2

and defined_vars_of_funsig (Funsig (f, params, ed) : funsig) : Ident_set.t =
  let s1 = Ident_set.of_list @@ (f :: params) in
  let s2 = defined_vars_of_expr_desc ed in
  Ident_set.union s1 s2