open Dj_common (* expose Id, Concrete_stack *)

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