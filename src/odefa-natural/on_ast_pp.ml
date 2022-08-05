open Batteries;;
open Jhupllib;;

open On_ast;;

(* TODO: Keep replacing " " with "@ " in format strings *)

let pp_label formatter (Label l) =
  Format.pp_print_string formatter l
;;

let pp_ident formatter (Ident s) =
  Format.pp_print_string formatter s
;;

let pp_ident_option formatter id_option =
  match id_option with
  | Some id -> pp_ident formatter id
  | None -> Format.pp_print_string formatter "(none)"
;;

let pp_ident_set formatter set =
  Pp_utils.pp_concat_sep_delim
    "{" "}" ","
    (fun formatter v -> pp_ident formatter v)
    formatter
    (Ident_set.enum set)
;;

let pp_ident_map pp_value formatter map =
  let open Format in
  Pp_utils.pp_concat_sep_delim
    "{" "}" ","
    (fun formatter (k,v) ->
       pp_ident formatter k;
       pp_print_string formatter " = ";
       pp_value formatter v)
    formatter
    (Ident_map.enum map)
;;

let pp_ident_map_sp pp_value formatter map =
  let open Format in
  Pp_utils.pp_concat_sep_delim
    "{" ",_}" ","
    (fun formatter (k,v) ->
       pp_ident formatter k;
       pp_print_string formatter " = ";
       pp_value formatter v)
    formatter
    (Ident_map.enum map)
;;

let pp_ident_list formatter list =
  Pp_utils.pp_concat_sep
    " "
    (fun formatter x -> pp_ident formatter x)
    formatter
    (List.enum list)
;;

let pp_variant_label formatter (Variant_label label) =
  Format.fprintf formatter "`%s" label
;;

let rec pp_funsig : Format.formatter -> funsig -> unit =
 fun formatter (Funsig (x, ident_list, e)) ->
  Format.fprintf formatter "%a@ %a =@ @[%a@]"
    pp_ident x pp_ident_list ident_list pp_expr_desc e

and pp_funsig_list : Format.formatter -> funsig list -> unit =
  fun formatter funsig_lst ->
  Pp_utils.pp_concat_sep
    " with "
    pp_funsig
    formatter
    (List.enum funsig_lst)

and pp_pattern formatter pattern =
  match pattern with
  | AnyPat -> Format.pp_print_string formatter "any"
  | IntPat -> Format.pp_print_string formatter "int"
  | BoolPat -> Format.pp_print_string formatter "bool"
  | FunPat -> Format.pp_print_string formatter "fun"
  | RecPat record ->
    Format.fprintf formatter "%a" (pp_ident_map_sp pp_ident_option) record
  | VariantPat (lbl, var) ->
    Format.fprintf formatter "%a %a" pp_variant_label lbl pp_ident var
  | VarPat ident -> Format.fprintf formatter "%a" pp_ident ident
  | EmptyLstPat -> Format.pp_print_string formatter "[]"
  | LstDestructPat (hd_var, tl_var) ->
    Format.fprintf formatter "%a :: %a"
      pp_ident hd_var pp_ident tl_var

(* Note: For two operators of equal precedence, still wrap parens if the
   operators are right-associative, but not if they're left-associative. *)

and pp_binop (formatter : Format.formatter) (expr : expr) : unit =
  let pp_expr_desc = 
    pp_expr_desc_without_tag
  in
  let pp_symb formatter expr =
    match expr with
    | Appl _ -> Format.pp_print_string formatter "" (* FIXME: Outputs two spaces! *)
    | Plus _ -> Format.pp_print_string formatter "+"
    | Minus _ -> Format.pp_print_string formatter "-"
    | Times _ -> Format.pp_print_string formatter "*"
    | Divide _ -> Format.pp_print_string formatter "/"
    | Modulus _ -> Format.pp_print_string formatter "%"
    | Equal _ -> Format.pp_print_string formatter "=="
    | Neq _ -> Format.pp_print_string formatter "<>"
    | LessThan _ -> Format.pp_print_string formatter "<"
    | Leq _ -> Format.pp_print_string formatter "<="
    | GreaterThan _ -> Format.pp_print_string formatter ">"
    | Geq _ -> Format.pp_print_string formatter ">="
    | And _ -> Format.pp_print_string formatter "and"
    | Or _ -> Format.pp_print_string formatter "or"
    | ListCons _ -> Format.pp_print_string formatter "::"
    | _ -> raise @@ Utils.Invariant_failure "Not a binary operator!"
  in
  match expr with
  | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Divide (e1, e2)
  | Modulus (e1, e2) | Equal (e1, e2) | Neq (e1, e2)
  | LessThan (e1, e2) | Leq (e1, e2) | GreaterThan (e1, e2) | Geq (e1, e2)
  | And (e1, e2) | Or (e1, e2) | ListCons (e1, e2) | Appl (e1, e2) ->
    let l_cmp = expr_precedence_cmp e1.body expr in
    let r_cmp = expr_precedence_cmp e2.body expr in
    if l_cmp < 0 && r_cmp <= 0 then 
      Format.fprintf formatter "(%a) %a (%a)" pp_expr_desc e1 pp_symb expr pp_expr_desc e2
    else if l_cmp >= 0 && r_cmp <= 0 then
      Format.fprintf formatter "%a %a (%a)" pp_expr_desc e1 pp_symb expr pp_expr_desc e2
    else if l_cmp < 0 && r_cmp > 0 then
      Format.fprintf formatter "(%a) %a %a" pp_expr_desc e1 pp_symb expr pp_expr_desc e2
    else if l_cmp >= 0 && r_cmp > 0 then
      Format.fprintf formatter "%a %a %a" pp_expr_desc e1 pp_symb expr pp_expr_desc e2
    else
      raise @@ Utils.Invariant_failure "Invalid precedence comparison!"
  | _ -> raise @@ Utils.Invariant_failure "Not a binary operator!"

and pp_expr_desc_with_tag (formatter : Format.formatter) (e : expr_desc) : unit = 
  Format.fprintf formatter "{tag: %a, body: %a}"
  Format.pp_print_int e.tag pp_expr e.body

and pp_expr_desc_without_tag 
  (formatter : Format.formatter) (e : expr_desc) : unit = 
  Format.fprintf formatter "%a"
  pp_expr e.body

and pp_expr 
  (formatter : Format.formatter) (expr : expr) : unit =
  let pp_expr_desc = 
    pp_expr_desc_without_tag
  in
  match expr with
  (* Values *)
  | Int n -> Format.pp_print_int formatter n
  | Bool b -> Format.pp_print_bool formatter b
  | Var x -> pp_ident formatter x
  | Function (x_list, e) ->
    Format.fprintf formatter "fun %a ->@ @[<2>%a@]"
      pp_ident_list x_list pp_expr_desc e
  | Input -> Format.pp_print_string formatter "input"
  | Record record ->
    pp_ident_map pp_expr_desc formatter record
  | List e_list ->
    Pp_utils.pp_concat_sep_delim
      "[" "]" ","
      (fun formatter e -> pp_expr_desc formatter e)
      formatter
      (List.enum e_list)
  (* Operations *)
  | Appl _ ->
    pp_binop formatter expr
  | Let (ident, e1, e2) -> 
    Format.fprintf formatter "let@ %a =@ %a@ in@ @[%a@]"
      pp_ident ident pp_expr_desc e1 pp_expr_desc e2
  | LetRecFun (funsig_lst, e) ->
    Format.fprintf formatter "let rec@ %a@ in@ @[%a@]"
      pp_funsig_list funsig_lst pp_expr_desc e
  | LetFun (funsig, e) ->
    Format.fprintf formatter "let@ %a@ in@ @[%a@]"
      pp_funsig funsig pp_expr_desc e
  | If (pred, e1, e2) ->
    Format.fprintf formatter "if@ %a@ then@ @[<2>%a@]@ else @[<2>%a@]"
    pp_expr_desc pred pp_expr_desc e1 pp_expr_desc e2
  | Match (e, pattern_expr_list) ->
    let pp_pattern_expr formatter (pattern, expr) =
      Format.fprintf formatter "@[| %a ->@ @[<2>%a@]@]"
        pp_pattern pattern pp_expr_desc expr
    in
    let pp_pattern_expr_lst formatter pat_expr_list =
      Pp_utils.pp_concat_sep
        ""
        pp_pattern_expr
        formatter
        (List.enum pat_expr_list)
    in
    Format.fprintf formatter "match@ %a@ with@ @[%a@]@ end"
    pp_expr_desc e pp_pattern_expr_lst pattern_expr_list
  (* Binary operations *)
  | Plus _ | Minus _ | Times _ | Divide _ | Modulus _
  | Equal _ | Neq _ | LessThan _ | Leq _ | GreaterThan _ | Geq _
  | And _ | Or _ | ListCons _ ->
    pp_binop formatter expr
  (* Unary operations *)
  | Not e ->
    if expr_precedence_cmp expr e.body < 0 then
      Format.fprintf formatter "not %a" pp_expr_desc e
    else
      Format.fprintf formatter "not (%a)" pp_expr_desc e
  | RecordProj (e, lbl) ->
    if expr_precedence_cmp e.body expr > 0 then
      Format.fprintf formatter "%a.%a" pp_expr_desc e pp_label lbl
    else
      Format.fprintf formatter "(%a).%a" pp_expr_desc e pp_label lbl
  | VariantExpr (variant_lbl, e) ->
    if expr_precedence_cmp expr e.body < 0 then
      Format.fprintf formatter "%a %a"
        pp_variant_label variant_lbl pp_expr_desc e
    else
      Format.fprintf formatter "%a (%a)"
        pp_variant_label variant_lbl pp_expr_desc e
  | Assert e ->
    if expr_precedence_cmp expr e.body < 0 then
      Format.fprintf formatter "assert %a" pp_expr_desc e
    else
      Format.fprintf formatter "assert (%a)" pp_expr_desc e
  | Assume e ->
    if expr_precedence_cmp expr e.body < 0 then
      Format.fprintf formatter "assume %a" pp_expr_desc e
    else
      Format.fprintf formatter "assume (%a)" pp_expr_desc e
  | Error x ->
    Format.fprintf formatter "%a" pp_ident x
;;

let show_ident = Pp_utils.pp_to_string pp_ident;;
let show_expr = Pp_utils.pp_to_string pp_expr;;
let show_pattern = Pp_utils.pp_to_string pp_pattern;;

let pp_on_type formatter (on_type : On_ast.type_sig) =
  match on_type with
  | TopType -> Format.pp_print_string formatter "Any"
  | IntType -> Format.pp_print_string formatter "Integer"
  | BoolType -> Format.pp_print_string formatter "Boolean"
  | FunType -> Format.pp_print_string formatter "Function"
  | ListType -> Format.pp_print_string formatter "List"
  | RecType lbls -> Format.fprintf formatter "Record %a" pp_ident_set lbls
  | VariantType lbl -> Format.fprintf formatter "Variant %a" pp_variant_label lbl
;;

let show_on_type = Pp_utils.pp_to_string pp_on_type;;