
open Common
open Interp_common

val eager_eval : Lang.Ast.Embedded.t -> Step.t Input_feeder.t -> max_step:Step.t -> Status.Eval.t * Step.t Path.t
(** [eager_eval] is eager concolic evaluation with standard eager semantics. *)
