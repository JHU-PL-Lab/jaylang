open Jay_ast
module P = Pp_std

let ss = Fmt.any
let id : ident Fmt.t = Fmt.(using (fun (Ident s) -> s) string)
let id_ = id
let id_opt : ident option Fmt.t = Fmt.option id
let label = Fmt.(using (fun (Label lab) -> lab) string)
let id_set = Fmt.braces (P.set id Ident_set.iter)
let id_map pp_value = Fmt.braces (P.map id pp_value Ident_map.iter)

let id_map_sp pp_value =
  Fmt.braces Fmt.(P.map id_ pp_value Ident_map.iter ++ ss "_")

let id_list = P.list ~sep:Fmt.sp id
let variant_label oc (Variant_label label) = Fmt.pf oc "`%s" label

let pattern oc = function
  | AnyPat -> Fmt.string oc "any"
  | IntPat -> Fmt.string oc "int"
  | BoolPat -> Fmt.string oc "bool"
  | FunPat -> Fmt.string oc "fun"
  | RecPat record -> (id_map_sp id_opt) oc record
  | StrictRecPat record -> (id_map id_opt) oc record
  | VariantPat (lbl, var) -> Fmt.pf oc "%a %a" variant_label lbl id var
  | VarPat ident -> id oc ident
  | EmptyLstPat -> Fmt.string oc "[]"
  | LstDestructPat (x, xs) -> Fmt.pf oc "%a :: %a" id x id xs

let binop_s_of_expr = function
  | Appl _ -> " " (* FIXME: Outputs two spaces! *)
  | Plus _ -> " + "
  | Minus _ -> " - "
  | Times _ -> " * "
  | Divide _ -> " / "
  | Modulus _ -> " % "
  | Equal _ -> " == "
  | Neq _ -> " <> "
  | LessThan _ -> " < "
  | Leq _ -> " <= "
  | GreaterThan _ -> " > "
  | Geq _ -> " >= "
  | And _ -> " and "
  | Or _ -> " or "
  | ListCons _ -> " :: "
  | _ -> raise @@ Jhupllib.Utils.Invariant_failure "Not a binary operator!"

let rec funsig oc (Funsig (x, ident_list, e)) =
  Fmt.pf oc "%a@ %a =@ @[%a@]" id x id_list ident_list pp_expr_desc e

(* Note: For two operators of equal precedence, still wrap parens if the
   operators are right-associative, but not if they're left-associative. *)
and pattern_expr oc (pat, expr) =
  Fmt.pf oc "@[| %a ->@ @[<2>%a@]@]" pattern pat pp_expr_desc expr

and pp_binop oc (expr : expr) : unit =
  match expr with
  | Plus (e1, e2)
  | Minus (e1, e2)
  | Times (e1, e2)
  | Divide (e1, e2)
  | Modulus (e1, e2)
  | Equal (e1, e2)
  | Neq (e1, e2)
  | LessThan (e1, e2)
  | Leq (e1, e2)
  | GreaterThan (e1, e2)
  | Geq (e1, e2)
  | And (e1, e2)
  | Or (e1, e2)
  | ListCons (e1, e2)
  | Appl (e1, e2) ->
      let cl = expr_precedence_cmp e1.body expr < 0 in
      let cr = expr_precedence_cmp e2.body expr <= 0 in
      let pp_l = P.paren_if ~c:cl pp_expr_desc in
      let pp_r = P.paren_if ~c:cr pp_expr_desc in
      let pp_binop = Fmt.(using binop_s_of_expr string) in
      Fmt.pf oc "%a%a%a" pp_l e1 pp_binop expr pp_r e2
  | _ -> raise @@ Jhupllib.Utils.Invariant_failure "Not a binary operator!"

and pp_expr_desc oc = (Fmt.using (fun expr_d -> expr_d.body) pp_expr) oc
and pp_expr ?(top = false) oc = Fmt.box (pp_expr_raw ~top) oc

and pp_expr_raw ?(top = false) oc expr : unit =
  let _ = top in
  match expr with
  (* Values *)
  | Int n -> Fmt.int oc n
  | Bool b -> Fmt.bool oc b
  | Var x -> id oc x
  | Function (x_list, e) ->
      Fmt.pf oc "fun %a ->@ @[<2>%a@]" id_list x_list pp_expr_desc e
  | Input -> Fmt.string oc "input"
  | Record record -> id_map pp_expr_desc oc record
  | List e_list -> Fmt.brackets (P.list pp_expr_desc) oc e_list
  (* Operations *)
  | Appl _ -> pp_binop oc expr
  | Let (ident, e1, e2) ->
      (* if top
         then *)
      if not top then Fmt.pf oc "@," ;
      Fmt.pf oc "@[<hv 2>let %a =@ %a@ in@]" id ident pp_expr_desc e1 ;
      Fmt.pf oc "@ %a" pp_expr_desc e2 ;
      if not top then Fmt.pf oc "@," ;
      ()
      (* else
         Fmt.pf oc "@[<v 2>let %a = %a in@ %a@]" id ident pp_expr_desc e1
           pp_expr_desc e2 *)
  | LetRecFun (fsig_lst, e) ->
      let funsig_list = P.list ~sep:(ss " with ") funsig in
      Fmt.pf oc "let rec %a in@ @[%a@]" funsig_list fsig_lst pp_expr_desc e
  | LetFun (fsig, e) -> Fmt.pf oc "let %a in@ @[%a@]" funsig fsig pp_expr_desc e
  | If (cond, e1, e2) ->
      Fmt.pf oc "if@ %a@ then@ @[<2>%a@]@ else @[<2>%a@]" pp_expr_desc cond
        pp_expr_desc e1 pp_expr_desc e2
  | Match (e, pat_es) ->
      let pattern_expr_lst = P.list ~sep:(ss "") pattern_expr in
      Fmt.pf oc "match@ %a@ with@ @[%a@]@ end" pp_expr_desc e pattern_expr_lst
        pat_es
  (* Binary operations *)
  | Plus _ | Minus _ | Times _ | Divide _ | Modulus _ | Equal _ | Neq _
  | LessThan _ | Leq _ | GreaterThan _ | Geq _ | And _ | Or _ | ListCons _ ->
      pp_binop oc expr
  (* Unary operations *)
  | Not e ->
      let c = expr_precedence_cmp expr e.body >= 0 in
      Fmt.pf oc "not %a" (P.paren_if ~c pp_expr_desc) e
  | RecordProj (e, lbl) ->
      let c = expr_precedence_cmp expr e.body >= 0 in
      Fmt.pf oc "%a.%a" (P.paren_if ~c pp_expr_desc) e label lbl
  | VariantExpr (variant_lbl, e) ->
      let c = expr_precedence_cmp expr e.body >= 0 in
      Fmt.pf oc "%a %a" variant_label variant_lbl (P.paren_if ~c pp_expr_desc) e
  | Assert e ->
      let c = expr_precedence_cmp expr e.body >= 0 in
      Fmt.pf oc "assert %a" (P.paren_if ~c pp_expr_desc) e
  | Assume e ->
      let c = expr_precedence_cmp expr e.body >= 0 in
      Fmt.pf oc "assume %a" (P.paren_if ~c pp_expr_desc) e
  | Error x -> Fmt.pf oc "Error: %a" id x

and expr_desc_with_tag oc e : unit =
  Fmt.pf oc "{tag: %a, body: %a}" Fmt.int e.tag (pp_expr : expr Fmt.t) e.body

let jay_type oc = function
  | TopType -> Fmt.string oc "Any"
  | IntType -> Fmt.string oc "Integer"
  | BoolType -> Fmt.string oc "Boolean"
  | FunType -> Fmt.string oc "Function"
  | ListType -> Fmt.string oc "List"
  | RecType lbls -> Fmt.pf oc "Record %a" id_set lbls
  | VariantType lbl -> Fmt.pf oc "Variant %a" variant_label lbl

let show_ident = Fmt.to_to_string id
let show_expr = Fmt.to_to_string pp_expr
let show_expr_desc = Fmt.to_to_string pp_expr_desc
let show_pattern = Fmt.to_to_string pattern
let show_jay_type = Fmt.to_to_string jay_type
let print_expr e = Fmt.pr "@[%a@]" (pp_expr ~top:true) e
