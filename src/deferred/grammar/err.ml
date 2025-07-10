
open Interp_common

type t = Timestamp.t Errors.Runtime.t

let to_string : t -> string = function
  | `XType_mismatch { msg ; body = time } -> Format.sprintf "Type mismatch at time %s: %s" (Timestamp.to_string time) msg
  | `XAbort { msg ; body = time } -> Format.sprintf "Abort at time %s: %s" (Timestamp.to_string time) msg
  | `XVanish time -> Format.sprintf "Vanish at time %s" (Timestamp.to_string time)
  | `XUnbound_variable (id, time) -> Format.sprintf "Unbound variable at time %s: %s" (Timestamp.to_string time) (Lang.Ast.Ident.to_string id)
  | `XReach_max_step time -> Format.sprintf "Reached max step at time %s" (Timestamp.to_string time)
