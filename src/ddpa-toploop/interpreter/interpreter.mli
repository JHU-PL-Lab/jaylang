(**
   Defines an interpreter for Odefa.
*)

open Jhupllib;;
open Odefa_ast;;

open Ast;;
open Pp_utils;;

module Environment = Var_hashtbl;;

type evaluation_environment = value Environment.t;;

val pp_evaluation_environment : evaluation_environment pretty_printer;;
val show_evaluation_environment : evaluation_environment -> string;;

exception Evaluation_failure of string;;

(**
   Evaluates the provided expression.  If evaluation becomes stuck, an
   Evaluation_failure is raised.  On successful evaluation, the last evaluated
   variable is returned with the evaluation store.

   The default input source is standard input, but this may be overridden.
   Input sources are functions which take a variable and produce an integer.

   A callback may be provided which is invoked on each clause immediately before
   that clause is evaluated.  The variables are freshened in such clauses,
   meaning that each clause will only be presented once per evaluation.
*)
val eval :
  ?input_source:(var -> value) ->
  ?clause_callback:(clause -> unit) ->
  expr ->
  var * evaluation_environment;;
