(**
  File: solve.mli
  Purpose: create a Z3 solver context

  Detailed description:
    Z3 solver contexts have state, and we sometimes want to isolate
    this state. A new state can be initiliazed via the generative
    functor [Make ()], or the default global state can be used with
    [Default].

    This module isolates all instances of Z3 state, and any other module
    that appears to use Z3 expressions is independent of state.

  Dependencies:
    Expression -- expressions can be converted to Z3 expressions in this
                  module's context
    Input_feeder -- the Z3 model is used to generate inputs
*)

module type S = sig
  include Z3_api.S

  val solve : bool Expression.t list -> [ `Sat of Input_feeder.t | `Unsat ]
  (** [solve exprs] does not handle solver timeout. An exception is thrown in that
      case, but it practically does not happen in the concolic system. *)
end

(* This functor is generative because it makes a new Z3 context *)
module Make () : S

(* Use this to use the global default Z3 context. Beware that this is not thread-safe. *)
module Default : S