
open Core

type t = Value.whnf Time_map.t

let empty : t = Time_map.empty

(*
  Cuts off all symbols at least as big as [t].
*)
let cut (Value.VSymbol t : Value.symb) (m : t) : t =
  Tuple3.get1
  @@ Time_map.split t m

let close_value (v : Value.t) (m : t) : Value.Without_symbols.t =
  Value.subst v ~f:(fun t -> Time_map.find t m)
