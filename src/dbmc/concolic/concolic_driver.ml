open Core

(* Run concolic tester: *)
let test_program_concolic ?timeout_sec source =
  let program = Dj_common.File_utils.read_source source in
  match timeout_sec with
  | None -> let _ = Concolic.eval program in ()
  | Some s -> let _ = Concolic.eval_timeout program s in ()