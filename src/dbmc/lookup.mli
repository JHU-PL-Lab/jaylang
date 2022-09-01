val run_ddse :
  config:Global_config.t ->
  state:Global_state.t ->
  (Lookup_key.t, unit) Scheduler.job Pairing_heap.t ->
  unit Lwt.t

val run_dbmc :
  config:Global_config.t ->
  state:Global_state.t ->
  (Lookup_key.t, unit) Scheduler.job Pairing_heap.t ->
  unit Lwt.t
(** [run_dbmc] performs the lookup. Usually one lookup steps consists of

    - process clause
    - handle graph node
    - book-keep global search state
    - create sub-lookups and push into the scheduler

    [state.key_map] is once used to keep track of [Lookup_key.t] created. *)
