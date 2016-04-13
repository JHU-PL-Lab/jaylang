open Batteries;;

open Ast;;
open String_utils;;

let pp_ident (Ident s) = s;;

let pp_freshening_stack (Freshening_stack ids) =
  List.fold_left
    (fun acc -> fun i ->
       (* Since freshening stacks are stored in reverse, we reverse the string    *)
       (* here.                                                                   *)
       acc ^ "__" ^ pp_ident i) "" ids
;;

let pp_var (Var(i, mfs)) =
  match mfs with
  | None -> pp_ident i ^ "$"
  | Some fs -> pp_ident i ^ pp_freshening_stack fs
;;

let pp_binary_operator binop =
  match binop with
  | Binary_operator_plus -> "+"
  | Binary_operator_int_minus -> "-"
  | Binary_operator_int_less_than -> "<"
  | Binary_operator_int_less_than_or_equal_to -> "<="
  | Binary_operator_equal_to -> "=="
  | Binary_operator_bool_and -> "and"
  | Binary_operator_bool_or -> "or"
;;

let pp_unary_operator unop =
  match unop with
  | Unary_operator_bool_not -> "not"
;;

let pp_record_value (Record_value(els)) =
  let pp_element (k,v) =
      Printf.sprintf "%s=%s" (pp_ident k) (pp_var v)
  in
  concat_sep_delim "{" "}" ", " @@ Enum.map pp_element @@ Ident_map.enum els
;;

let pp_ref_value (Ref_value(x)) = "ref " ^ pp_var x;;

let rec pp_function_value (Function_value(x,e)) =
  pp_var x ^ " -> ( " ^ pp_expr e ^ " )"

and pp_value v =
  match v with
  | Value_record(r) -> pp_record_value r
  | Value_function(f) -> pp_function_value f
  | Value_ref(r) -> pp_ref_value r
  | Value_int(n) -> string_of_int n
  | Value_bool(b) -> string_of_bool b
  | Value_string(s) -> "\"" ^ s ^ "\""

and pp_clause_body b =
  match b with
  | Var_body(x) -> pp_var x
  | Value_body(v) -> pp_value v
  | Appl_body(x1,x2) -> pp_var x1 ^ " " ^ pp_var x2
  | Conditional_body(x,p,f1,f2) ->
    pp_var x ^ " ~ " ^ pp_pattern p ^ " ? " ^
    pp_function_value f1 ^ " : " ^ pp_function_value f2
  | Projection_body(x,i) -> pp_var x ^ "." ^ pp_ident i
  | Deref_body(x) -> "!" ^ pp_var x
  | Update_body(x1,x2) -> pp_var x1 ^ " <- " ^ pp_var x2
  | Binary_operation_body(x1,op,x2) ->
    Printf.sprintf "%s %s %s" (pp_var x1) (pp_binary_operator op) (pp_var x2)
  | Unary_operation_body(op,x1) ->
    Printf.sprintf "%s %s" (pp_unary_operator op) (pp_var x1)
  | Indexing_body(x1,x2) ->
    Printf.sprintf "%s[%s]" (pp_var x1) (pp_var x2)

and pp_clause c =
  match c with
  | Clause(x,b) ->
    pp_var x ^ " = " ^ pp_clause_body b

and pp_expr (Expr(cls)) =
  concat_sep "; " @@ Enum.map pp_clause @@ List.enum cls

and pp_pattern p =
  match p with
  | Record_pattern(els) ->
      let pp_element = pp_tuple pp_ident pp_pattern in
      concat_sep_delim "{" "}" ", " @@ Enum.map pp_element @@
      Ident_map.enum els
  | Fun_pattern -> "fun"
  | Ref_pattern -> "ref"
  | Int_pattern -> "int"
  | Bool_pattern(b) -> string_of_bool b
  | String_pattern -> "string"
;;

let brief_pp_clause (Clause(x,_)) = pp_var x;;

