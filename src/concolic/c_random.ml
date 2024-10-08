
open Core

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let reset () : unit = 
  Random.init seed

let int : int -> int = 
  Random.int

let int_incl : int -> int -> int = 
  Random.int_incl

let any_pos_int () : int = 
  Random.int Int.max_value
