
open Core

module T = struct
  type _ t =
    | True_direction : bool t
    | False_direction : bool t
    | Case_int : int -> int t
    | Case_default : { not_in : int list } -> int t

  let compare (type p) (cmp : p -> p -> int) (a : p t) (b : p t) : int =
    match a, b with
    (* bool *)
    | True_direction, False_direction -> -1
    | True_direction, True_direction -> 0
    | False_direction, False_direction -> 0
    | False_direction, True_direction -> 1
    (* int *)
    | Case_int _, Case_default _ -> -1
    | Case_int i1, Case_int i2 -> cmp i1 i2
    | Case_default ls1, Case_default ls2 -> List.compare cmp ls1.not_in ls2.not_in
    | Case_default _, Case_int _ -> 1
end

include T

let of_bool (b : bool) : bool t =
  if b
  then True_direction
  else False_direction

let of_int (i : int) : int t =
  Case_int i

module Packed = Utils.Pack.Make (T)

let pack (type a) (dir : a t) : Packed.t =
  match dir with
  | (True_direction | False_direction) as d -> B d
  | (Case_int _ | Case_default _) as d -> I d
