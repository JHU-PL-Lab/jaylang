open Batteries
open Jhupllib
open Ast
open Pp_utils

let pp_ident formatter (Ident s) = Format.pp_print_string formatter s
let show_ident = pp_to_string pp_ident

let pp_ident_map pp_value formatter map =
  let open Format in
  pp_concat_sep_delim "{" "}" ","
    (fun formatter (k, v) ->
      pp_ident formatter k ;
      pp_print_string formatter " => " ;
      pp_value formatter v)
    formatter
  @@ Ident_map.enum map

let pp_freshening_stack formatter (Freshening_stack ids) =
  ids
  |> List.iter (fun i ->
         Format.pp_print_string formatter "__" ;
         pp_ident formatter i)

let show_freshening_stack = pp_to_string pp_freshening_stack

let pp_var formatter (Var (i, mfs)) =
  pp_ident formatter i ;
  match mfs with
  | None -> ()
  | Some fs ->
      Format.pp_print_string formatter "__at__" ;
      pp_freshening_stack formatter fs

let show_var = pp_to_string pp_var

let pp_binary_operator formatter binop =
  let s =
    match binop with
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
  in
  Format.pp_print_string formatter s

let show_binary_operator = pp_to_string pp_binary_operator

let pp_record_value formatter (Record_value els) =
  let pp_element formatter (k, v) =
    Format.fprintf formatter "%a=%a" pp_ident k pp_var v
  in
  pp_concat_sep_delim "{" "}" "," pp_element formatter @@ Ident_map.enum els

let show_record_value = pp_to_string pp_record_value

let pp_variable_list formatter var_list =
  pp_concat_sep_delim "[" "]" "," pp_var formatter @@ List.enum var_list

let show_variable_list_value = pp_to_string pp_variable_list

let rec pp_function_value formatter (Function_value (x, e)) =
  Format.fprintf formatter "fun %a -> (@ @[<2>%a@])" pp_var x pp_expr e

and pp_value formatter v =
  match v with
  | Value_record r -> pp_record_value formatter r
  | Value_function f -> pp_function_value formatter f
  | Value_int n -> Format.pp_print_int formatter n
  | Value_bool b -> Format.pp_print_bool formatter b

and pp_clause_body formatter b =
  match b with
  | Var_body x -> pp_var formatter x
  | Value_body v -> pp_value formatter v
  | Input_body -> Format.pp_print_string formatter "input"
  | Appl_body (x1, x2) -> Format.fprintf formatter "%a %a" pp_var x1 pp_var x2
  | Conditional_body (x, e1, e2) ->
      Format.fprintf formatter "%a @[<4>? @[<2>(%a)@] : @[<2>(%a)@]@]" pp_var x
        pp_expr e1 pp_expr e2
  | Match_body (x, p) ->
      Format.fprintf formatter "%a ~ %a" pp_var x pp_pattern p
  | Projection_body (x, l) ->
      Format.fprintf formatter "%a.%a" pp_var x pp_ident l
  | Not_body x -> Format.fprintf formatter "not %a" pp_var x
  | Binary_operation_body (x1, op, x2) ->
      Format.fprintf formatter "%a %a %a" pp_var x1 pp_binary_operator op pp_var
        x2
  | Abort_body -> Format.pp_print_string formatter "abort"
  | Assume_body x -> Format.fprintf formatter "assume %a" pp_var x
  | Assert_body x -> Format.fprintf formatter "assert %a" pp_var x

and pp_clause formatter c =
  match c with
  | Clause (x, b) ->
      Format.fprintf formatter "%a = %a" pp_var x pp_clause_body b

and pp_expr formatter (Expr cls) =
  pp_concat_sep ";" pp_clause formatter @@ List.enum cls

and pp_pattern formatter p =
  match p with
  | Fun_pattern -> Format.pp_print_string formatter "fun"
  | Int_pattern -> Format.pp_print_string formatter "int"
  | Bool_pattern -> Format.pp_print_string formatter "bool"
  | Rec_pattern els ->
      let pp_element formatter idnt =
        Format.fprintf formatter "%a" pp_ident idnt
      in
      pp_concat_sep_delim "{" "}" ", " pp_element formatter
      @@ Ident_set.enum els
  | Strict_rec_pattern els ->
      let pp_element formatter idnt =
        Format.fprintf formatter "%a" pp_ident idnt
      in
      pp_concat_sep_delim "|{" "}|" ", " pp_element formatter
      @@ Ident_set.enum els
  | Any_pattern -> Format.pp_print_string formatter "any"

let show_value = pp_to_string pp_value
let show_clause_body = pp_to_string pp_clause_body
let show_clause = pp_to_string pp_clause
let show_brief_clause formatter (Clause (x, _)) = pp_var formatter x
let show_expr = pp_to_string pp_expr
let show_pattern = pp_to_string pp_pattern

let pp_type_sig formatter type_sig =
  match type_sig with
  | Top_type -> Format.pp_print_string formatter "top"
  | Int_type -> Format.pp_print_string formatter "int"
  | Bool_type -> Format.pp_print_string formatter "bool"
  | Fun_type -> Format.pp_print_string formatter "fun"
  | Rec_type labels ->
      pp_concat_sep_delim "{" "}" "," pp_ident formatter
      @@ Ident_set.enum labels
  (* | Untouched_type s -> Format.pp_print_string formatter @@ "'" ^ s *)
  | Bottom_type -> Format.pp_print_string formatter "bottom"
(* | Any_untouched_type -> Format.pp_print_string formatter "untouched" *)

let show_type_sig = pp_to_string pp_type_sig
