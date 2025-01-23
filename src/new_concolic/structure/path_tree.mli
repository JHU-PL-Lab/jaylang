
type t

val empty : t

val of_options : (unit, t) Options.Fun.a

val add_stem : t -> Stem.t -> t

val pop_sat_target : ?kind:Target_queue.Pop_kind.t -> t -> (t * Target.t * Input_feeder.t) option Lwt.t
