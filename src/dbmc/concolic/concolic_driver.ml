open Core

(* Run concolic tester: *)
let test_program_loose_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic_options.F.t =
  Concolic_options.F.compose
    Concolic_loose.eval
    ~f:Dj_common.File_utils.read_source

let test_program_concolic : (string -> Branch_tracker.Status_store.Without_payload.t) Concolic_options.F.t =
  Concolic_options.F.compose
    Concolic.eval
    ~f:Dj_common.File_utils.read_source


  (* Toy example of propogating a change in optional args *)
  (* Concolic_options.F.make (fun r -> fun source ->
    Concolic_options.F.appl Concolic.eval { r with global_timeout_sec = 1.0 } (Dj_common.File_utils.read_source source)
    ) *)