open Dj_common (* expose Id, Concrete_stack *)

(* type t =
  (* Interpreter types *)
  | Found_target of { x : Id.t ; stk : Concrete_stack.t ; v : Dvalue.t } (* Different from target branch *)
  | Found_abort of Dvalue.t
  | Terminate of Dvalue.t
  | Reach_max_step of Id.t * Concrete_stack.t
  | Run_the_same_stack_twice of Id.t * Concrete_stack.t
  | Run_into_wrong_stack of Id.t * Concrete_stack.t *)

(* Interpreter exceptions *)
(* T.t gets passed up my exceptions that affect control flow of interpretation *)
module Make (T : sig type t end) =
  struct
    exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : Dvalue.t }
    exception Found_abort of Dvalue.t * T.t
    exception Found_failed_assume of T.t
    exception Found_failed_assert of T.t
    exception Terminate of Dvalue.t
    exception Reach_max_step of Id.t * Concrete_stack.t * T.t
    exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
    exception Run_into_wrong_stack of Id.t * Concrete_stack.t
  end