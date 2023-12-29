(*
  See src-test/dbmc/test_concolic.ml
*)

open Core

(* This logic is out of date because reach max step should be getting bubbled *)
(* Run concolic tester: *)
(* let rec test_program_concolic source counter =
  if counter <= 0 then Format.printf "Timeout limit reached. Program will not terminate...\n" else 
  let program = Dj_common.File_utils.read_source source in 
  try Concolic.concolic_eval program with
  | Concolic_exceptions.Reach_max_step(_, _, _) -> Format.printf "Reach max step... re-evaluating:\n"; test_program_concolic source (counter - 1)
  | ex -> raise ex  *)