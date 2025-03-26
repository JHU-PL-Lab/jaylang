(**
  Module [State_read_result].

  This is a common monad that supports state and is transformed
  by reader and result. It is also a continuation monad.

  The several interpreters in this repo use this monad. It is written
  in this way (without the abstraction of actual monad transformers)
  because performance is a concern. However, it's not enough of a concern
  to warrant using mutation (and risk incorrectness) instead.

  While the functions exposed here are sufficient to use the monad, the
  type is not abstract because performance can be critical, and there
  are some cases where compiler inlining is not enough; instead, we will
  use the structure of the type to write specialized functions.
*)

open Core

(*
  General state monad that supports error but is significantly more efficient
  than using the Result monad inside of a normal state monad.

  Credit for this idea goes to the writer of the following post:
    https://discuss.ocaml.org/t/can-a-state-monad-be-optimized-out-with-flambda/9841/5?u=brandon

  I have further added in the reader monad for a non-propagating element to the state, since I
  know the typical use of this is in an interpreter. This is nice because we could otherwise be
  passing around the environment in a "reader" style while letting the monad handle other threading.
  Instead, it feels best to centralize the core effects and prevent the proliferation of inconsistent
  "mini monad" implementations scattered throughout the code.
*)
module Make (State : T) (Read : T) (Err : T) = struct
  type 'a m = {
    run : 'r. reject:(Err.t -> 'r) -> accept:('a -> State.t -> 'r) -> State.t -> Read.t -> 'r
  } 

  let[@inline always][@specialise] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    { run =
      fun ~reject ~accept s r ->
        x.run s r ~reject ~accept:(fun x s ->
          (f x).run ~reject ~accept s r
        )
    }

  let[@inline always][@specialise] return (a : 'a) : 'a m =
    { run = fun ~reject:_ ~accept s _ -> accept a s }

  let read : (State.t * Read.t) m =
    { run = fun ~reject:_ ~accept s r -> accept (s, r) s }

  let[@inline always][@specialise] modify (f : State.t -> State.t) : unit m =
    { run =
      fun ~reject:_ ~accept s _ ->
        accept () (f s)
    }

  let[@inline always][@specialise] fail (e : Err.t) : 'a m =
    { run = fun ~reject ~accept:_ _ _ -> reject e }

  let[@inline always][@specialise] local (f : Read.t -> Read.t) (x : 'a m) : 'a m =
    { run = fun ~reject ~accept s r -> x.run ~reject ~accept s (f r) }

  let run (x : 'a m) (init_state : State.t) (init_read : Read.t) : ('a * State.t, Err.t) result =
    x.run ~reject:(fun e -> Error e) ~accept:(fun a s -> Ok (a, s)) init_state init_read
end
