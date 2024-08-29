open Dj_common (* expose Id, Concrete_stack *)

(* Interpreter exceptions *)
(* T.t gets passed up my exceptions that affect control flow of interpretation *)
module Make (T : sig type t end) =
  struct
    exception Found_abort of Dvalue.t * T.t
    exception Type_mismatch of T.t
    exception Found_failed_assume of T.t
    exception Found_failed_assert of T.t
    exception Reach_max_step of Id.t * T.t
  end