(*
  See src-test/dbmc/test_concolic.ml
*)

open Core

(* Run concolic tester: *)
let rec test_program_concolic source counter =
  if counter <= 0 then Format.printf "Timeout limit reached. Program will not terminate...\n" else 
  let program = Dj_common.File_utils.read_source source in 
  try Concolic.concolic_eval program with
  | Concolic_exceptions.All_Branches_Hit -> Format.printf "All branches hit.\n"
  | Concolic_exceptions.Unreachable_Branch(b) -> Format.printf "Unreachable branch: %s\n" (Branch.Ast_branch.to_string b)
  | Concolic_exceptions.Unsatisfiable_Branch(b) -> Format.printf "Unsatisfiable branch: %s\n" (Branch.Ast_branch.to_string b)
  | Concolic_exceptions.Reach_max_step(_, _) -> Format.printf "Reach max step... re-evaluating:\n"; test_program_concolic source (counter - 1)
  | ex -> raise ex 