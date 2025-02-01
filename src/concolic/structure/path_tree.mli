
module Make (_ : Target_queue.S) : sig
  type t

  val of_options : (unit, t) Options.Arrow.t
  (** [of_options ()] knows no path or constraints. *)

  val add_stem : t -> Stem.t -> t
  (** [add_stem tree stem] is a new path tree where the [stem] has been placed onto the [tree]. *)

  val pop_sat_target : t -> (t * Target.t * Input_feeder.t) option Lwt.t
  (** [pop_sat_target tree] is a new tree, target, and input feeder to hit that target, or
      is none if there are not satisfiable targets left in the [tree]. *)
end
