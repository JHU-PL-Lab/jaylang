
open Lang.Ast

exception UnboundVariable of Ident.t

module rec V : sig
  type t =
    | VInt of int * int Expression.t 
    | VBool of bool * bool Expression.t
    | VFunClosure of { param : Ident.t ; body : closure }
    | VVariant of { label : VariantLabel.t ; payload : t }
    | VRecord of t RecordLabel.Map.t
    | VFrozen of closure
    | VId

  and closure = { expr : Embedded.t ; env : Env.t }
end

and Env : sig
  type t
  val empty : t
  val add : t -> Ident.t -> V.t -> t
  val fetch : t -> Ident.t -> V.t
end

include module type of V with type closure = V.closure and type t = V.t
