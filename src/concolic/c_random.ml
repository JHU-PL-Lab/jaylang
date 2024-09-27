
open Core

let seed =
  String.fold "jhu-pl-lab" ~init:0 ~f:(fun acc c -> Char.to_int c + acc)

let init () = 
  Random.init seed

let int = 
  Random.int

let int_incl = 
  Random.int_incl

let any_pos_int () = 
  Random.int Int.max_value
