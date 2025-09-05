
(*
  This used to contain the time. Now we just have no payload because
  we lean on the concolic interpreter and extract the concrete value,
  but the concolic interpreter doesn't return the time of the error.
*)
type t = unit Interp_common.Errors.Runtime.t

let to_string : t -> string = function
  | `XType_mismatch { msg ; body = () } -> Format.sprintf "Type mismatch: %s" msg
  | `XAbort { msg ; body = () }         -> Format.sprintf "Abort: %s" msg
  | `XVanish ()                         -> "Vanish"
  | `XUnbound_variable (id, ())         -> Format.sprintf "Unbound variable: %s" (Lang.Ast.Ident.to_string id)
  | `XReach_max_step ()                 -> "Reached max step"
