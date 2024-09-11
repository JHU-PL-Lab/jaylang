
type t =
  { clause_id   : Jayil.Ast.Ident_new.t
  ; input_value : int }
  [@@deriving sexp, compare]