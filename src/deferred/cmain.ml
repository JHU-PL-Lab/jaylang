
(* Concolic main *)

module V = Value.Make (Concolic.Value.Concolic_value)

(*
  TODO: 
    - Need concolic effects to push branches and such.
    - This will require composing the monads, probably 
      manually as a first pass.
    - I'll just steal the entire concolic monad and paste
      it in with the deferred monad. Then add the calls
      I need to track concolic stuff. And also handle expressions
      accordingly. But I think it will be very easy.
*)
