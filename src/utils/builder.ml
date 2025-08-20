
open Core

module type S = sig
  type a
  type t
  val empty : t
  val cons : a -> t -> t
end

(*
  Note that BUILDER gives us MONOID:

  module type MONOID = sig
    type t 
    include S with type a := t and type t := t
  end

  So if we just want a monoidal log, then that will work.
*)

module Unit_builder : S with type a = unit and type t = unit = struct
  type a = unit
  type t = unit
  let empty = ()
  let cons () () = ()
end

module Make_list_builder (Elt : T) : S with type a = Elt.t and type t = Elt.t list = struct
  type a = Elt.t
  type t = a list
  let empty = []
  let cons a l = a :: l
end
