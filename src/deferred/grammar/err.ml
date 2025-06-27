
open Lang.Ast

type t =
  | XTypeMismatch of string * Timestamp.t
  | XAbort of string * Timestamp.t
  | XDiverge of Timestamp.t
  | XUnboundVariable of Ident.t * Timestamp.t

let to_string : t -> string = function
  | XTypeMismatch (s, t) -> Format.sprintf "Type mismatch at time %s: %s" (Timestamp.to_string t) s
  | XAbort (s, t) -> Format.sprintf "Abort at time %s: %s" (Timestamp.to_string t) s
  | XDiverge t -> Format.sprintf "Diverge at time %s" (Timestamp.to_string t)
  | XUnboundVariable (id, t) -> Format.sprintf "Unbound variable at time %s: %s" (Timestamp.to_string t) (Ident.to_string id)

