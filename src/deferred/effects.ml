
open Core

(* monad to handle all of the effects *)

module Env = struct
  (* stuff to read from *)
  type t =
    { time   : Timestamp.t
    ; feeder : Grammar.Input_feeder.t
    ; env    : Value.env
    }
end

(*
  For some reason I don't know, Core.Map likes to take linear time to compute
  the size of a map. Who cares.

  Baby, on the other hand, offers an O(log n) cut operation, as opposed to
  O(m + log n) where m is the size of the resulting map, offered from Core.

  This is why we use Baby in the maps in Grammar.
*)
module State = struct
  type t =
    { symbol_env : Grammar.Symbol_map.t
    ; pending_proofs : Grammar.Pending.t } 

  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)
end

(* I don't think errors will be this simple because we need to be ready to continue on younger proofs *)
(* The error needs to produce new state *)
type 'a m = {
  run : 'r. reject:(Env.t -> 'r) -> accept:('a -> Env.t -> State.t -> 'r) -> Env.t -> State.t -> 'r
}

let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  { run = fun ~reject ~accept e s ->
    x.run e s ~reject ~accept:(fun a e s ->
      (f a).run ~accept ~reject e s
    )
  }

let return (a : 'a) : 'a m =
  { run = fun ~reject:_ ~accept e s -> accept a e s }


(*
  I think I'll need some way to check if we have an error.
  This tells me that errors probably shouldn't be values. And also I would
  like to case on error, but the only way I know how to do that here is to run it.
  Maybe that will be okay. I'll just need to chug along and see what happens.
*)
