open Odefa_ast
open Ast

(** [t] takes the [Id.t] and the call stack, to give the result *)
type t = Ident.t * (Ident.t * Ident.t list) -> int
let dummy0 _ = 0