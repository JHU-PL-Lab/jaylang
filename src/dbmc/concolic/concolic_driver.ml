open Core

(* Run concolic tester: *)
let test_program_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic.With_options.t =
  fun
  ?global_timeout_sec
  ?solver_timeout_sec
  ?quit_on_first_abort
  ?global_max_step
  source
  ->
  let program = Dj_common.File_utils.read_source source in
  Concolic.eval program ?global_timeout_sec ?solver_timeout_sec ?global_max_step ?quit_on_first_abort
  (* match timeout_sec with
  | None -> let _ = Concolic.eval program in ()
  | Some s -> Format.printf "Calling concolic eval with timer\n"; let _ = Concolic.eval ~global_timeout_sec:s program in () *)

let test_program_strict_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic.With_options.t =
  fun
  ?global_timeout_sec
  ?solver_timeout_sec
  ?quit_on_first_abort
  ?global_max_step
  source
  ->
  let program = Dj_common.File_utils.read_source source in
  Concolic_strict.eval program ?global_timeout_sec ?solver_timeout_sec ?global_max_step ?quit_on_first_abort