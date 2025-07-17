
(* Concolic main *)

(*
  TODO: 
    - Need concolic effects to push branches and such.
    - This will require composing the monads, probably 
      manually as a first pass.
    - I'll just steal the entire concolic monad and paste
      it in with the deferred monad. Then add the calls
      I need to track concolic stuff. And also handle expressions
      accordingly. But I think it will be very easy.


  Consts for concolic interp will need to allow a parametrized
  input feeder. Input feeders are already parametrized, so that
  won't be a big step, but it will be a mild annoyance.
*)
