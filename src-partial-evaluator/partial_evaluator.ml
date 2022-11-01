open Core
open Jayil
open Ast

let rec constant_folding (Expr clauses) =
  let new_clauses = List.map ~f:constant_folding_clause clauses in
  Expr new_clauses

and constant_folding_clause clause =
  let (Clause (Var (x, s), cbody)) = clause in
  match cbody with
  | Binary_operation_body (x1, Binary_operator_minus, x2)
    when Ast.Var.equal x1 x2 ->
      let new_body = Value_body (Value_int 0) in
      Clause (Var (x, s), new_body)
  | _ -> clause

let eval raw_source =
  let expr = Dj_common.File_utils.read_source raw_source in

  Fmt.pr "%a\n" Jayil.Pp.expr expr ;
  let new_expr = constant_folding expr in
  Fmt.pr "%a\n" Jayil.Pp.expr new_expr ;
  expr