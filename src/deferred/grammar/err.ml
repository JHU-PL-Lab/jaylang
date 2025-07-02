
open Lang.Ast
open Interp_common

type t =
  | XTypeMismatch of string * Callstack.t
  | XAbort of string * Callstack.t
  | XDiverge of Callstack.t
  | XUnboundVariable of Ident.t * Callstack.t

let to_string : t -> string = function
  | XTypeMismatch (s, t) -> Format.sprintf "Type mismatch at time %s: %s" (Callstack.to_string t) s
  | XAbort (s, t) -> Format.sprintf "Abort at time %s: %s" (Callstack.to_string t) s
  | XDiverge t -> Format.sprintf "Diverge at time %s" (Callstack.to_string t)
  | XUnboundVariable (id, t) -> Format.sprintf "Unbound variable at time %s: %s" (Callstack.to_string t) (Ident.to_string id)
