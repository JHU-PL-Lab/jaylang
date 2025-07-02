
open Core

type t = Value.closure Stack_map.t

let empty : t = Stack_map.empty

(* May want to raise an exception, just to check invariants, if the symbol is duplicate *)
let push (Value.VSymbol t : Value.symb) (work : Value.closure) (m : t) : t =
  Stack_map.add t work m

let pop (Value.VSymbol t : Value.symb) (m : t) : (Value.closure * t) option =
  Option.map (Stack_map.find_opt t m) ~f:(fun closure ->
    closure, Stack_map.remove t m
  )

(*
  Cuts off all symbols at least as big as [t].
*)
let cut (Value.VSymbol t : Value.symb) (m : t) : t =
  Tuple3.get1
  @@ Stack_map.split t m