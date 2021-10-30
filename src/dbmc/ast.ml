open Core

type id = Odefa_ast.Ast.ident = Ident of string

type bop = Plus | Minus

type var = Var of id

type value = Value

and pat = Pattern

and exp = Clause of var * clause_body | Block of exp list

and clause_body =
  | Val of value
  | Var_body of var
  | Input
  | App of var * var
  | Cond of var * exp * exp
  | Match of var * pat
  | Proj of var * id
  | Binop of var * bop * var

let of_odefa_var (Odefa_ast.Ast.Var (id, _)) = Var id

let rec of_odefa_ast expr =
  let open Odefa_ast in
  let (Ast.Expr chauses) = expr in
  let es = List.map chauses ~f:of_odefa_clause in
  Block es

and of_odefa_clause clause =
  let open Odefa_ast in
  let (Ast.Clause (Ast.Var (id, _), clause)) = clause in
  let clause_here =
    match clause with
    | Ast.Value_body _v -> Val Value
    | Ast.Var_body var -> Var_body (of_odefa_var var)
    | Ast.Input_body -> Input
    | _ -> Input
  in
  Clause (Var id, clause_here)
