open Ast

module type Is_brief = sig
  val is_brief : bool
end

module Make (B : Is_brief) = struct
  let id = Fmt.(using (fun (Ident s) -> s) string)
  let var_ = Fmt.using (fun (Var (x, _)) -> x) id

  let id_map pp_v oc map =
    let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" id x pp_v v in

    Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry oc map

  let id_set oc set = Fmt.iter ~sep:(Fmt.any ", ") Ident_set.iter id oc set
  let id_list oc list = Fmt.iter ~sep:(Fmt.any ", ") List.iter id oc list

  let string_of_binop = function
    | Binary_operator_plus -> "+"
    | Binary_operator_minus -> "-"
    | Binary_operator_times -> "*"
    | Binary_operator_divide -> "/"
    | Binary_operator_modulus -> "%"
    | Binary_operator_less_than -> "<"
    | Binary_operator_less_than_or_equal_to -> "<="
    | Binary_operator_equal_to -> "=="
    | Binary_operator_not_equal_to -> "<>"
    | Binary_operator_and -> "and"
    | Binary_operator_or -> "or"

  let binop = Fmt.(using string_of_binop string)
  let record oc (Record_value els) = (Fmt.braces (id_map var_)) oc els
  let labels oc ids = (Fmt.braces id_list) oc ids

  let rec expr oc (Expr cls) =
    Fmt.(vbox @@ list ~sep:Fmt.semi (vbox ~indent:4 clause)) oc cls

  (* for tab usage, see https://www.mancoosi.org/~abate/ocaml-format-module.html *)
  and clause oc (Clause (x, b)) =
    Fmt.pf oc "%a" var_ x ;
    Fmt.pf oc " = %a" clause_body b

  and clause_body oc = function
    | Var_body x -> var_ oc x
    | Value_body v -> value oc v
    | Input_body -> Fmt.string oc "input"
    | Appl_body (x1, x2) -> Fmt.pf oc "%a %a" var_ x1 var_ x2
    | Conditional_body (x, e1, e2) ->
        if B.is_brief
        then Fmt.pf oc "%a ? (...) : (...)" (* "%a @[<4>? ...@]" *) var_ x
        else
          Fmt.pf oc "%a ? (@;    @[%a@]@;) : (@;    @[%a@]@;)" var_ x
            (Fmt.vbox ~indent:2 expr) e1 expr e2
    | Match_body (x, p) ->
        if B.is_brief
        then Fmt.pf oc "%a ~ ..." var_ x
        else Fmt.pf oc "%a ~ %a" var_ x pattern p
    | Projection_body (x, l) -> Fmt.pf oc "%a.%a" var_ x id l
    | Not_body x -> Fmt.pf oc "not %a" var_ x
    | Binary_operation_body (x1, op, x2) ->
        Fmt.pf oc "%a %a %a" var_ x1 binop op var_ x2
    | Abort_body -> Fmt.string oc "abort"
    | Assume_body x -> Fmt.pf oc "assume %a" var_ x
    | Assert_body x -> Fmt.pf oc "assert %a" var_ x

  and fun_ oc (Function_value (x, e)) =
    (* '('  '@ '   @[            @] ')'
        |   space   |             |  |
        |            - fbody-box  -  |
        |                            |
         ---------- wrapper  --------
    *)
    Fmt.pf oc "fun %a -> (@ @[<2>%a@])" var_ x expr e

  and value oc = function
    | Value_record r -> record oc r
    | Value_function f -> fun_ oc f
    | Value_int n -> Fmt.int oc n
    | Value_bool b -> Fmt.bool oc b

  and pattern oc = function
    | Fun_pattern -> Fmt.string oc "fun"
    | Int_pattern -> Fmt.string oc "int"
    | Bool_pattern -> Fmt.string oc "bool"
    | Any_pattern -> Fmt.string oc "any"
    | Rec_pattern els -> labels oc ((Ident_set.to_list @@ els) @ [ Ident "_" ])
    | Strict_rec_pattern els -> (Fmt.braces id_set) oc els

  let print_expr e = Fmt.pr "@[%a@]" expr e

  let type_sig oc type_sig =
    match type_sig with
    | Top_type -> Fmt.string oc "top"
    | Int_type -> Fmt.string oc "int"
    | Bool_type -> Fmt.string oc "bool"
    | Fun_type -> Fmt.string oc "fun"
    | Rec_type ids -> labels oc (Ident_set.to_list ids)
    (* pp_concat_sep_delim "{" "}" "," pp_ident formatter
       @@ Ident_set.enum labels *)
    (* | Untouched_type s -> Format.pp_print_string formatter @@ "'" ^ s *)
    | Bottom_type -> Format.pp_print_string oc "bottom"
  (* | Any_untouched_type -> Format.pp_print_string formatter "untouched" *)

  let show_id = Fmt.to_to_string id
  let show_var = Fmt.to_to_string var_
  let show_value = Fmt.to_to_string value
  let show_expr = Fmt.to_to_string expr
  let show_clause = Fmt.to_to_string clause
  let show_clause_body = Fmt.to_to_string clause_body
  let show_type_sig = Fmt.to_to_string type_sig
  let show_binary_operator = Fmt.to_to_string binop
end

module No_brief = Make (struct
  let is_brief = false
end)

include No_brief

module Brief = Make (struct
  let is_brief = true
end)
