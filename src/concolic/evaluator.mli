(**
  File: evaluator.mli
  Purpose: concolically evaluate the program repeatedly

  Detailed description:
    This runs the interpreter repeatedly and only stops when at a terminal
    status--timeout was hit, a program error was found, or all paths were
    exhausted.
*)

val global_runtime : float Utils.Safe_cell.t
(** [global_runtime] is a cell containing the total global time spent
    on interpretation. *)

val global_solvetime : float Utils.Safe_cell.t
(** [global_solvetime] is a cell containing the total global time spent
    on solving between interpretations. This inludes any management of
    symbolic constraints; it is not strictly the time spent using Z3 to solve. *)

type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t -> 
  max_step:Interp_common.Step.t -> Concolic_common.Status.Eval.t * 'k Concolic_common.Path.t

(* One single run of the concolic evaluator, sans loop *)
val eager_eval : Interp_common.Step.t eval

module Make : functor (K : Smt.Symbol.KEY) (_ : Target_queue.Make(K).S)
  (_ : Smt.Formula.SOLVABLE) (P : Pause.S) (_ : Concolic_common.Options.V) -> sig
  val eval : Lang.Ast.Embedded.t -> K.t eval -> Concolic_common.Status.Terminal.t P.t
  (** [eval pgm] is the result of concolic evaluation on [pgm]. *)
end

val lwt_eval : (Lang.Ast.Embedded.t, Concolic_common.Status.Terminal.t Lwt.t) Concolic_common.Options.Arrow.t
(** [lwt_eval pgm] is the result of concolic evaluation on [pgm] using the default
    global [Solve.S] module and eager evaluation. *)
