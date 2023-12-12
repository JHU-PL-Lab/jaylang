open Dj_common (* expose Id, Concrete_stack *)

(* type t =
  (* Interpreter types *)
  | Found_target of { x : Id.t ; stk : Concrete_stack.t ; v : Dvalue.t } (* Different from target branch *)
  | Found_abort of Dvalue.t
  | Terminate of Dvalue.t
  | Reach_max_step of Id.t * Concrete_stack.t
  | Run_the_same_stack_twice of Id.t * Concrete_stack.t
  | Run_into_wrong_stack of Id.t * Concrete_stack.t
  (* Concolic types *)
  | All_branches_hit
  | Unreachable_branch of Ast_branch.t
  | Unsatisfiable_branch of Ast_branch.t
  | Missed_target_branch *)

(* Interpreter exceptions *)
exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : Dvalue.t }
exception Found_abort of Dvalue.t
exception Terminate of Dvalue.t
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

(* Concolic exceptions *)
exception All_Branches_Hit (* Exhausted the AST, so done with concolic evaluation *)
exception Unreachable_Branch of Ast_branch.t (* Can't enter either side of branch at all *)
exception Unsatisfiable_Branch of Ast_branch.t (* Branch direction cannot be solved for *)
exception Missed_Target_Branch (* Previous solve didn't properly let us hit the target branch *)