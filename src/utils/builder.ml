
open Core

module type S = sig
  type a
  type t
  val empty : t
  val cons : a -> t -> t
  val combine : t -> t -> t
end

module Unit_builder : S with type a = unit and type t = unit = struct
  type a = unit
  type t = unit
  let empty = ()
  let cons () () = ()
  let combine () () = ()
end

module Dlist_builder (A : T) : S with type a = A.t and type t = A.t Dlist.t = struct
  type a = A.t 
  include Dlist
  type t = a Dlist.t
end

module List_builder (A : T) : S with type a = A.t and type t = A.t list = struct
  type a = A.t 
  type t = a list
  let empty = []
  let cons a ls = a :: ls
  let combine = List.append
end


(*
  Note that BUILDER gives us MONOID:

  module type MONOID = sig
    type t 
    include S with type a := t and type t := t
  end

  So if we just want a monoidal log, then that will work. Hence, I require
  logs in this repo to work over builders, and any monoid is sufficient.
*)
module Of_monoid (M : Types.MONOID) : S with type a = M.t and type t = M.t = struct
  type a = M.t
  type t = M.t
  let empty = M.neutral
  let cons = M.combine
  let combine = M.combine
end
