open Core

module T = struct
  type t = Ident of string
  [@@deriving sexp, compare, equal]
end

include T
include Comparator.Make(T)

let show (Ident s) = s
let pp = Fmt.of_to_string show

let of_ast_id (x : Odefa_ast.Ast.Ident.t) : t = 
  match x with
  | Odefa_ast.Ast.Ident n -> Ident n

let to_ast_id = function
  | Ident n -> Odefa_ast.Ast.Ident n 