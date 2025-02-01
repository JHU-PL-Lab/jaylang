
type t

val empty : t
(** [empty] knows no paths or constraints. *)

val of_options : (unit, t) Options.Arrow.t

val add_stem : t -> Stem.t -> t
(** [add_stem tree stem] is a new path tree where the [stem] has been placed onto the [tree]. *)

val pop_sat_target : ?kind:Target_queue.Pop_kind.t -> t -> (t * Target.t * Input_feeder.t) option Lwt.t
(** [pop_sat_target ?kind tree] is a new tree, target, and input feeder to hit that target, or
    is none if there are not satisfiable targets left in the [tree]. *)
