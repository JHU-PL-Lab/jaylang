(**
  File: semantics.mli
  Purpose: encapsulate concolic semantics into a monad

  Detailed description:
    The concolic semantics need state, error, environment, and logging.
    These are most efficiently composed into a CPS monad, as well.
    The benefit of the monad is that it contains all of the semantics
    specific to concolic evaluation.
    The downside is that performance is worse than using built-in effects
    and state from OCaml. The tradeoff for now is worth it: we have good
    separation of logic and follow the documented semantics closely.

    While it negatively affects inlining, we avoid passing around constants
    by initializing a functor with some constants. Behavior that is
    independent of these constants is extracted and statically intialized
    for better inlining.

    Currently, we use lists for logging. It is very likely to be more
    performant to include the logging in the state.
*)

module M : sig
  (*
    ------------
    MONAD BASICS 
    ------------
  *)
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m


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

  val step : Interp_common.Step.t m
  (** [step] is the current step count. *)
end

module Consts : sig
  type t =
    { target       : Target.t
    ; options      : Options.t
    ; input_feeder : Input_feeder.t } 
end

module type S = sig
  type 'a m = 'a M.m 

  val vanish : 'a m
  (** [vanish] is a safe monadic exit. *)

  val incr_step : unit m
  (** [incr_step] updates the state and quits if max step has been exceeded. *)

  val hit_branch : bool Direction.t -> bool Formula.t -> unit m
  (** [hit_branch dir e] takes the given [dir] down a branch whose condition has expression [e]. *)

  val hit_case : int Direction.t -> int Formula.t -> other_cases:int list -> unit m
  (** [hit_case dir e] takes the given [dir] down a case whose condition has expression [e],
      and the integer cases *not* taken are in [other_cases]. *)

  val get_input : (Interp_common.Step.t -> 'a Input_feeder.Key.t) -> Value.t m
  (** [get_input make_key] is an input value from the input feeder using [make_key] to make
      a key out of the current step and determine the value. If ['a] is [int], then this
      is a [VInt] value, and if ['a] is [bool], then this is a [VBool] value. *)

  val run : 'a m -> Status.Eval.t * Target.t list
  (** [run m] is the result of running [m] with empty initial state and environment,
      where the result is the interpretation evaluation status and the targets acquired
      through the concolic logic that should be pushed to a queue of targets. *)
end

module Initialize (_ : sig val c : Consts.t end) : S
