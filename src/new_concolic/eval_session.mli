
type t

(*
  --------
  CREATION
  --------
*)

val empty : t

val make : Target.t -> Input_feeder.t -> t

val with_options : (t, t) Options.Fun.a

val get_max_step : t -> int

(*
  -----------
  EXPRESSIONS
  -----------
*)

val get_input : 'a Stepkey.t -> t -> t * Value.t

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : bool Direction.t -> bool Expression.t -> t -> t

val hit_case : int Direction.t -> int Expression.t -> other_cases:int list -> t -> t

val diverge : t -> t

val abort : t -> t

val type_mismatch : t -> t

val reach_max_step : t -> t

(*
  -----------
  BETWEEN-RUN
  -----------
*)

val finish : t -> Status.Eval.t
