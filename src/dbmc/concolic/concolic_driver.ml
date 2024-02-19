open Core

(* Run concolic tester: *)
let test_program_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic_options.With_options.t =
  Concolic_options.With_options.map
    Concolic.eval
    ~f:(fun eval -> fun source -> eval (Dj_common.File_utils.read_source source))

let test_program_strict_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic_options.With_options.t =
  Concolic_options.With_options.map
    Concolic_strict.eval
    ~f:(fun eval -> fun source -> eval (Dj_common.File_utils.read_source source))