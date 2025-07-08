
open Core

type t = Value.whnf Timestamp.Map.t

let empty : t = Timestamp.Map.empty

(*
  Cuts off all symbols at least as big as [t].
*)
let cut (Value.VSymbol t : Value.symb) (m : t) : t =
  Tuple3.get1
  @@ Timestamp.Map.split t m

let close_value (v : Value.t) (m : t) : Value.Without_symbols.t =
  Value.subst v ~f:(fun t -> Timestamp.Map.find t m)
