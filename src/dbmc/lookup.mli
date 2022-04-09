exception Found_solution of Riddler.result_info

val lookup_top :
  config:Global_config.t ->
  state:Global_state.t ->
  unit Scheduler.job Pairing_heap.t ->
  unit Lwt.t
(** [lookup_top] performs the lookup. Usually one lookup steps consists of

    - process clause
    - handle graph node
    - book-keep global search state
    - create sub-lookups and push into the scheduler

    [state.node_map] is once used to keep track of [Lookup_key.t] created. *)
