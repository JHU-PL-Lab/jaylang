
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

module Transformer (M : sig
  type 'a m 
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end) (B : S) = struct
  type 'a m = B.t -> ('a * B.t) M.m

  let return a = fun b -> M.return (a, b)

  let bind x f =
    fun b ->
      M.bind (x b) (fun (a, b) ->
        f a b
      )

  let upper (m : 'a M.m) : 'a m =
    fun b ->
      M.bind m (fun a ->
        M.return (a, b)
      )

  let observe : B.t m =
    fun b ->
      M.return (b, b)

  let modify_log (f : B.t -> B.t) : unit m =
    fun b ->
      M.return ((), f b)

  let log (a : B.a) : unit m =
    modify_log (B.cons a)
end

module Monad (B : S) = Transformer (struct
  type 'a m = 'a 
  let[@inline always] return a = a
  let[@inline always] bind x f = f x
end) (B)
