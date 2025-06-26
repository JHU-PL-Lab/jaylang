
open Lang.Ast

type t =
  | XTypeMismatch of string * Timestamp.t
  | XAbort of string * Timestamp.t
  | XDiverge of Timestamp.t
  | XUnboundVariable of Ident.t * Timestamp.t