
open Core

(*
 I should pass around instances of random number generators. 
 And also have a functor to pre-seed a module with such a generator.
*)

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let reset () : unit = 
  Random.init seed

let int : int -> int = 
  Random.int

let bool : unit -> bool =
  Random.bool

let int_incl : int -> int -> int = 
  Random.int_incl

let any_pos_int () : int = 
  Random.int Int.max_value