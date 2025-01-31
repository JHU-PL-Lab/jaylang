
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

  and closure = { expr : Embedded.t ; snap : Env.snapshot }
end = struct
  type t =
    | VInt of int * int Expression.t 
    | VBool of bool * bool Expression.t
    | VFunClosure of { param : Ident.t ; body : closure }
    | VVariant of { label : VariantLabel.t ; payload : t }
    | VRecord of t RecordLabel.Map.t
    | VFrozen of closure
    | VId

  and closure = { expr : Embedded.t ; snap : Env.snapshot }
end

and Env : sig
  type snapshot
  type t

  val create : unit -> t

  val restore : t -> snapshot -> unit

  val capture : t -> snapshot

  val add : t -> Ident.t -> V.t -> unit

  val fetch : t -> Ident.t -> V.t
end = struct
  module H = Hashtbl.Make (Ident)

  type snapshot = Store.snapshot

  type t = 
    { tbl : V.t Store.Ref.t H.t
    ; store : Store.t }

  let restore (env : t) (snap : snapshot) : unit =
    Store.restore env.store snap

  let capture (env : t) : snapshot = 
    Store.capture env.store

  let create () : t =
    { tbl = H.create ()
    ; store = Store.create () }

  let add (env : t) (id : Ident.t) (v : V.t) : unit =
    match Hashtbl.find env.tbl id with
    | None -> Hashtbl.set env.tbl ~key:id ~data:(Store.Ref.make env.store v)
    | Some r -> Store.Ref.set env.store r v

  let fetch (env : t) (id : Ident.t) : V.t =
    match Hashtbl.find env.tbl id with
    | Some r -> Store.Ref.get env.store r
    | None -> raise @@ UnboundVariable id
end

include V
