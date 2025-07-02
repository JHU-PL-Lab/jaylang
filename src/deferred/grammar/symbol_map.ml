
open Core

type t = Value.whnf Stack_map.t

let empty : t = Stack_map.empty

(*
  Cuts off all symbols at least as big as [t].
*)
let cut (Value.VSymbol t : Value.symb) (m : t) : t =
  Tuple3.get1
  @@ Stack_map.split t m