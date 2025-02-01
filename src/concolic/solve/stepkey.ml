
open Core

include Utils.Separate.Make_with_compare (Int)

let to_string (type a) (x : a t) : string =
  match x with
  | I i -> Format.sprintf "i_stepkey_$%d" i
  | B i -> Format.sprintf "b_stepkey_$%d" i

let uniq_id (type a) (x : a t) : int = extract x
