(**
  File: options.mli
  Purpose: contain options for concolic evaluation

  Detailed description:
    The concolic evaluator can run with many options, such as
    how long to run until timeout, how deep of a bound to put
    on the path tree, etc. This module contains those options
    and provides defaults whenever they are not given.

    There is a functional record and record of ref cells for
    ease of argument parsing, as well as for passing in to a
    functor as a singleton module to initialize a sort of "state".

    The is also an optional argument [Arrow] module to work with
    these parameters as optional arguments, filling in the default
    whenever not given.

    Note: sometimes the [Arrow] module is overused, and it makes it
      appear as if a behavior can be affected by *all* of the arguments.
      This is often not the case, and it is used for ease to pass in
      just two or three of them. This is not documented well when done.
*)
module T : sig
  type t =
    { global_timeout_sec : float
    ; global_max_step    : int
    ; max_tree_depth     : int
    ; random             : bool
    ; n_depth_increments : int
    ; in_parallel        : bool }
    [@@deriving sexp]
end

type t = T.t

val default : t

val cmd_arg_term : t Cmdliner.Term.t
(** [cmd_arg_term] is a [Cmdliner] term that parses command line arguments into a [T.t]. *)

module type V = sig
  val r : t
  (** [r] is a record containing values to fill in for each optional arg. *)
end

(* `Arrow` for optional arguments on functions *)
module Arrow : sig
  type ('a, 'b) t =
    ?global_timeout_sec    : float
    -> ?global_max_step    : int
    -> ?max_tree_depth     : int
    -> ?random             : bool
    -> ?n_depth_increments : int
    -> ?in_parallel        : bool
    -> 'a
    -> 'b
  (** [t] is an arrow *)

  include Preface.Specs.ARROW with type ('a, 'b) t := ('a, 'b) t

  val appl : ('b, 'c) t -> T.t -> 'b -> 'c
  (** [run x r] applies the values from [r] to the arguments of [x] *)

  val make : (T.t -> 'b -> 'c) -> ('b, 'c) t
  (** [make f] accepts optional arguments and applies them in the default record to [f]. *)
end