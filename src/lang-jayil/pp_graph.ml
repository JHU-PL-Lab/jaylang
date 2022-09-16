open Ast

let id = Fmt.(using (fun (Ident s) -> s) string)
let var_ = Fmt.using (fun (Var (x, _)) -> x) id

let id_map pp_v oc map =
  let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" id x pp_v v in

  Fmt.iter_bindings ~sep:(Fmt.any "; ")
    (fun f map -> Ident_map.iter f map)
    pp_entry oc map

let id_set oc set =
  Fmt.iter ~sep:(Fmt.any ", ") (fun f set -> Ident_set.iter f set) id oc set

let string_of_binop = function
  | Binary_operator_plus -> "+"
  | Binary_operator_minus -> "-"
  | Binary_operator_times -> "*"
  | Binary_operator_divide -> "/"
  | Binary_operator_modulus -> "%"
  | Binary_operator_less_than -> (* "<" *) "&lt;"
  (* unicode ﹤ ≤ *)
  | Binary_operator_less_than_or_equal_to -> "≤"
  | Binary_operator_equal_to -> "=="
  | Binary_operator_not_equal_to -> "<>"
  | Binary_operator_and -> "and"
  | Binary_operator_or -> "or"

let binop = Fmt.(using string_of_binop string)
let record oc (Record_value els) = (Fmt.braces (id_map var_)) oc els

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
  | Appl_body (x1, x2) -> Fmt.pf oc "%a &nbsp;%a" var_ x1 var_ x2
  | Conditional_body (x, _e1, _e2) -> Fmt.pf oc "%a ? (...) : (...)" var_ x
  | Match_body (x, p) -> Fmt.pf oc "%a ~ %a" var_ x pattern p
  | Projection_body (x, l) -> Fmt.pf oc "%a.%a" var_ x id l
  | Not_body x -> Fmt.pf oc "not %a" var_ x
  | Binary_operation_body (x1, op, x2) ->
      Fmt.pf oc "%a %a %a" var_ x1 binop op var_ x2
  | Abort_body -> Fmt.string oc "abort"
  | Assume_body x -> Fmt.pf oc "assume %a" var_ x
  | Assert_body x -> Fmt.pf oc "assert %a" var_ x

(* unicode ↦ *)
and fun_ oc (Function_value (x, _e)) = Fmt.pf oc "fun %a ↦ ..." var_ x

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
  | Rec_pattern els -> (Fmt.braces id_set) oc els
