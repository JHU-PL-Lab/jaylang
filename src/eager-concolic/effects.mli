
open Concolic_common

(*
  ------------
  MONAD BASICS 
  ------------
*)

include Utils.Types.MONAD

(*
  -------------
  STATE AND ENV
  -------------
*)

val fetch : Lang.Ast.Ident.t -> Value.t m
(** [fetch id] is the value in the environment to which [id] maps, or a monadic error
    if [id] is unbound. *)

val local : (Value.Env.t -> Value.Env.t) -> 'a m -> 'a m
(** [local f m] runs [m] under the local environment transformed by [f]. *)

val read_env : Value.Env.t m
(** [read_env] is the environment. *)

(*
  --------------
  INTERPRETATION
  --------------
*)

val with_incr_depth : 'a m -> 'a m
(** [with_incr_depth m] runs [m] where the depth of determinism wrappings is locally increased. *)

val with_escaped_det : 'a m -> 'a m
(** [with_escaped_depth m] runs [m] where nondeterminism is allowed. *)

val assert_nondeterminism : unit m
(** [assert_nondterminism] is a monadic error if the current environment does not allow nondeterminism.
    Otherwise, nothing happens. *)

val abort : string -> 'a m
(** [abort msg] aborts with a monadic error with the given reason [msg]. *)

val type_mismatch : string -> 'a m
(** [type_mismatch msg] ends interpretation in a monadic error with the given reason [msg]. *)

val vanish : 'a m
(** [vanish] is a safe monadic exit. *)

val step : Interp_common.Step.t m
(** [step] is the current step count. *)

val incr_step : max_step:Interp_common.Step.t -> unit m
(** [incr_step ~max_step] updates the state and quits if [max_step] has been exceeded. *)

val push_branch : Interp_common.Step.t Direction.t -> unit m
(** [push_branch dir] pushes the [dir] onto the stateful path. *)

val get_input : (Interp_common.Step.t -> 'a Interp_common.Key.Stepkey.t) -> Interp_common.Step.t Interp_common.Input_feeder.t -> Value.t m
(** [get_input make_key feeder] is the value from the [feeder] associated with the key
    made from the current step count. *)

val run : 'a m -> Status.Eval.t * Interp_common.Step.t Path.t
(** [run x] is the result of the monad run on an empty initial state and environment. *)
