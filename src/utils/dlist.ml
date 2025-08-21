
(*
  Difference lists
*)

open Core

type 'a t = 'a list -> 'a list

let empty : 'a t = fun xs -> xs

let neutral = empty

let cons : 'a -> 'a t -> 'a t = fun a dlist ->
  fun xs -> a :: dlist xs

let singleton : 'a -> 'a t = fun a ->
  fun xs -> a :: xs

let append : 'a t -> 'a t -> 'a t = fun dlist1 dlist2 ->
  fun xs -> dlist2 (dlist1 xs)

let combine = append

let of_list ls = fun xs -> ls @ xs

module Specialize (X : T) = struct
  type a = X.t
  type nonrec t = a t
  let neutral = neutral
  let empty = empty
  let cons = cons
  let append = append
  let combine = combine
end

module Log (X : T) = struct
  type tape = X.t list
  type 'a m = 'a * X.t t

  let bind m f =
    let a, dlist = m in
    let b, dlist' = f a in
    b, append dlist dlist'

  let return a = a, empty

  let log (a : X.t) : unit m =
    (), singleton a

  (* expensive because it finally constructs the list *)
  let run (m : 'a m) : 'a * tape =
    let a, dlist = m in
    a, dlist []

  (* same here *)
  let listen (m : 'a m) : ('a * tape) m =
    return @@ run m
end
