
module Cresult =
  struct
    type 'a t =
      | Ok of 'a
      | Found_abort
      | Type_mismatch
      | Found_failed_assume
      | Reach_max_step

    let pp (x : 'a t) (pp : 'a -> string) : string =
      match x with
      | Ok a -> pp a
      | Found_abort -> "Found abort in interpretation"
      | Type_mismatch -> "Type mismatch in interpretation"
      | Reach_max_step -> "Reach max steps during interpretation"
      | Found_failed_assume -> "Found failed assume or assert"
  end

(* I probably want to hide this type *)
type 'a m = Session.Symbolic.t -> Session.Symbolic.t * 'a Cresult.t

let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  fun s ->
    let ss, r = x s in
    match r with
    | Ok a -> f a ss
    (* must match on all remaining cases to keep 'a and 'b different. Type checking complains if we capture and reuse *)
    | Found_abort -> ss, Found_abort
    | Type_mismatch -> ss, Type_mismatch
    | Found_failed_assume -> ss, Found_failed_assume
    | Reach_max_step -> ss, Reach_max_step

let return (a : 'a) : 'a m =
  fun s -> s, Ok a

let read : Session.Symbolic.t m =
  fun s ->
    s, Ok s
  
let modify (f : Session.Symbolic.t -> Session.Symbolic.t) : unit m =
  fun s ->
    f s, Ok ()

let run (x : 'a m) (s : Session.Symbolic.t) : Session.Symbolic.t * 'a Cresult.t =
  x s

(* This is a little dangerous because it sends all to Ok, but I use it safely *)
let show (x : 'a m) (pp : 'a -> string) : string m =
  fun s ->
    match x s with
    | ss, res -> ss, Ok (Cresult.pp res pp)

let reach_max_step =
  let%bind () = modify Session.Symbolic.reach_max_step in
  fun s -> s, Cresult.Reach_max_step

let found_abort =
  let%bind () = modify Session.Symbolic.found_abort in
  fun s -> s, Cresult.Found_abort

let failed_assume =
  let%bind () = modify Session.Symbolic.fail_assume in
  fun s -> s, Cresult.Found_failed_assume

let type_mismatch =
  let%bind () = modify Session.Symbolic.found_type_mismatch in
  fun s -> s, Cresult.Type_mismatch
