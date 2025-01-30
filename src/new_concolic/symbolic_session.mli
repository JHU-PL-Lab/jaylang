
(*
  * Perform binop (that is, DON'T perform binop if we've exceeded depth)
  * Perform not (..)
  * Create const exprs
  * Create key exprs while retrieving input
  * Hit branch
    * for bool branch, just take bool value
    * for int branch, take int value and other cases and default
  * Hold stem (related to branch)
  * Track the pruning state
*)


module Status : sig
  type t =
    | Found_abort of (Input_feeder.Input.t list [@compare.ignore])
    | Type_mismatch of (Input_feeder.Input.t list [@compare.ignore])
    | Finished_interpretation of { pruned : bool ; reached_max_step : bool ; stem : Stem.t }
    (* [@@deriving compare, sexp] *)
end

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

val get_input : 'a Concolic_key.t -> t -> t * Value.t

(* val get_bool_input : Concolic_key.t -> t -> t * Value.t *)

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

val finish : t -> Status.t
