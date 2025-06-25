
open Core
(* open Lang.Ast *)

(*
  In the future, I think many of these should have their own
  file and be abstract behind an mli.
*)

module Input_feeder = struct
  (* TODO: work with pick_b too *)
  type t = Timestamp.t -> int

  let zero : t = fun _ -> 0
end

module Symbol_map = struct
  type t = Value.whnf Timestamp.Map.t

  let empty : t = Timestamp.Map.empty

  (*
    Cuts off all symbols at least as big as [t].
  *)
  let cut (Value.VSymbol t : Value.symb) (m : t) : t =
    Tuple3.get1
    @@ Timestamp.Map.split t m
end

(* TODO: the grammar needs to note that v in M is whnf *)
module Pending = struct
  type t = Value.closure Timestamp.Map.t

  let empty : t = Timestamp.Map.empty

  (* May want to raise an exception, just to check invariants, if the symbol is duplicate *)
  let push (Value.VSymbol t : Value.symb) (work : Value.closure) (m : t) : t =
    Timestamp.Map.add t work m

  let pop (Value.VSymbol t : Value.symb) (m : t) : (Value.closure * t) option =
    Option.map (Timestamp.Map.find_opt t m) ~f:(fun closure ->
      closure, Timestamp.Map.remove t m
    )

  (*
    Cuts off all symbols at least as big as [t].
  *)
  let cut (Value.VSymbol t : Value.symb) (m : t) : t =
    Tuple3.get1
    @@ Timestamp.Map.split t m
end
