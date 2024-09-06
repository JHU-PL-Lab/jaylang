
module Cresult =
  struct
    type 'a t =
      | Ok of 'a
      | Found_abort
      | Type_mismatch
      | Found_failed_assume
      | Reach_max_step
  end

(* TODO: hide this type *)
type 'a m = Session.Symbolic.t -> Session.Symbolic.t * 'a Cresult.t

let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  fun s ->
    match x s with
    | ss, Ok a -> f a ss
    (* must match on all remaining cases to keep 'a and 'b different *)
    | ss, Found_abort -> ss, Found_abort
    | ss, Type_mismatch -> ss, Type_mismatch
    | ss, Found_failed_assume -> ss, Found_failed_assume
    | ss, Reach_max_step -> ss, Reach_max_step

let return (a : 'a) : 'a m =
  fun s -> s, Ok a

let read : Session.Symbolic.t m =
  fun s ->
    s, Ok s

let write (s : Session.Symbolic.t) : unit m =
  fun _ ->
    s, Ok ()
  
let modify (f : Session.Symbolic.t -> Session.Symbolic.t) : unit m =
  fun s ->
    f s, Ok ()

let run (x : 'a m) (s : Session.Symbolic.t) : Session.Symbolic.t * 'a Cresult.t =
  x s

let reach_max_step =
  fun s -> s, Cresult.Reach_max_step

let found_abort =
  fun s -> s, Cresult.Found_abort

let failed_assume =
  fun s -> s, Cresult.Found_failed_assume

let type_mismatch =
  fun s -> s, Cresult.Type_mismatch
