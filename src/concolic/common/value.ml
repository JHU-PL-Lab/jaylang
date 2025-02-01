
open Core
open Lang
open Ast

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
end = struct
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
end = struct
  type t = V.t Ident.Map.t

  let empty : t = Ident.Map.empty

  let add (env : t) (id : Ident.t) (v : V.t) : t =
    Map.set env ~key:id ~data:v

  let fetch (env : t) (id : Ident.t) : V.t =
    match Map.find env id with
    | Some t -> t
    | None -> raise @@ UnboundVariable id
end

include V
