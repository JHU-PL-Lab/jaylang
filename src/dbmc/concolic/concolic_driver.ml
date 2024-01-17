open Core

(* Run concolic tester: *)
let rec test_program_concolic source counter =
  if counter <= 0 then Format.printf "Timeout limit reached. Program will not terminate...\n" else 
  let program = Dj_common.File_utils.read_source source in 
  try
    let _ = Concolic.eval program in ()
  with
  | Concolic_exceptions.Reach_max_step(_, _, _) -> Format.printf "Reach max step... re-evaluating:\n"; test_program_concolic source (counter - 1)
  | ex -> raise ex 