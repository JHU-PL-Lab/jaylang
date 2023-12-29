open Core
open Dj_common
open Jayil.Ast

module Mode :
  sig
    type t =
      | Plain
      | With_target_x of Id.t
      | With_full_target of Id.t * Concrete_stack.t

    module Debug :
      sig
        type t =
          | No_debug
          | Debug_clause of (Id.t -> Concrete_stack.t -> value -> unit)
      end
  end

module G : (* = Graph.Imperative.Digraph.ConcreteBidirectional (Id_with_stack) *) (* hide until can find type *)
  sig
    type t
  end

(**
  The eval session tracks everything needed by the interpreter to evaluate the expression.
  Nothing here is used for any part of the concolic logic. This is all to correctly evaluate
  the expression.
*)
module Eval :
  sig
    (* NOTE: this type is mutable *)
    type t =
      { (* mode *)
        input_feeder    : Input_feeder.t
      ; mode            : Mode.t
      ; (* tuning *)
        step            : int ref
      ; max_step        : int option
      ; (* book-keeping*)
        alias_graph     : G.t
      ; (* debug *)
        is_debug        : bool (* TODO: get rid of this *) (* can use Mode.Debug.t instead *)
      ; debug_mode      : Mode.Debug.t
      ; val_def_map     : (Id_with_stack.t, clause_body * Dvalue.t) Hashtbl.t
      ; term_detail_map : (Lookup_key.t, Lookup_detail.t) Hashtbl.t
      ; block_map       : Cfg.block Jayil.Ast.Ident_map.t
      ; rstk_picked     : (Rstack.t, bool) Hashtbl.t
      ; lookup_alert    : Lookup_key.t Hash_set.t } 

    val create_default : unit -> t
    (** [create_default ()] is an arbitrary session with no intentional input feeder and empty graphs. *)

    (* val create : ?max_step:int -> ?debug_mode:Mode.Debug.t -> Global_state.t -> Global_config.t -> Mode.t -> Input_feeder.t -> t *)
    val create : Input_feeder.t -> int -> t
    (** [create input_feeder global_max_step] *)

    val add_alias : Id_with_stack.t -> Id_with_stack.t -> t -> unit
    (** [add_alias x y session] sets the alias graph in the given [session] to say that
        [x] is now an alias of [y]. [x] is defined *after* [y] and points to [y]. *)

    val add_val_def_mapping : Id_with_stack.t -> (clause_body * Dvalue.t) -> t -> unit
    (** [add_val_def_mapping x (body, v) session] sets the identifier [x] to hold the value [v]
        that is evaluated from the clause body [body] in the given [session]. *)
  end

(**
  The concolic session tracks a single concolic evaluation. It gathers formulas from start to finish
  of one evaluation of the program.   
*)
module Concolic :
  sig
    (* TODO: instead of using outcomes, and just set some bools to true. *)
    module Outcome :
      sig
        type t =
          | Hit_target (* The session's target was hit. *)
          | Found_abort (* The session ended in an abort. *)
          | Reach_max_step (* The session ended by reaching the max number of steps. *)
      end

    module Outcome_set :
      sig
         type t
         (** [t] holds multiple outcomes from the program. e.g. the program can hit the target and later find an abort. *)
      end

    type t =
      { branch_solver : Branch_solver.t
      ; cur_target    : Branch.Runtime.t option
      ; new_targets   : Branch.Runtime.t list
      ; outcomes      : Outcome_set.t
      ; hit_branches  : (Branch.Runtime.t * Branch.Status.t) list
      ; inputs        : (Ident.t * Dvalue.t) list }

    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formula session expr] adds the [expr] under the current parent of the [session]. *)

    val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
    (** [add_key_eq_val session k v] sets [k = v] in the [session]. This is a special case of [add_formula]. *)

    val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_alias session x y] sets [x = y] in the [session]. This is a special case of [add_formula]. *)

    val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_binop session x op y z] sets [z = y op z] in the [session]. This is a special case of [add_formula]. *)

    val found_abort : t -> t
    (** [found_abort session] adds the info that an abort was found under the current parent. *)

    val reach_max_step : t -> t
    (** [reach_max_step session] adds the info that the max step was reached under the current parent. *)

    val enter_branch : t -> Branch.Runtime.t -> t
    (** [enter_branch session branch] sets the new parent as [branch] and hits the branch. *)

    val exit_branch : t -> t
    (** [exit_branch session ret_key] uses the final key [ret_key] in the branch to exit and return
        to previous parent. Also cleans up formulas in the solver. *)

    val add_input : t -> Ident.t -> Dvalue.t -> t
    (** [add_input session x v] adds the fact that [v] was fed to variable [x] as an input. *)
  end

module Solver_map :
  sig
    type t 
    (** [t] maps targets to solvers that can be used to solve for that target. *)

    (* TODO: the formulas never depend on inputs, so maybe I can just keep all formulas together. *)
  end

(*
  TODO: keep persistent formulas as a result of max step and aborts because we might not want a branch
    to be unsatisfiable, but rather mark it as "unsatisfiable because of earlier abort" or "unreachable
    because of max step".
    We can do this by solving for the target without any persistent formulas added, and then carefully add
    them to determine *why* we can't solve for a target.
    This also helps us decide when to stop trying to hit a target that keeps resulting in max step.
*)
type t = 
  { branch_store        : Branch.Status_store.t
  ; persistent_formulas : Branch_solver.Formula_set.t
  ; target_stack        : Branch.Runtime.t list
  ; solver_map          : Solver_map.t
  ; global_max_step     : int
  ; run_num             : int }
  (** [t] tracks information between runs of the concolic evaluator and helps create new Eval and Concolic
      sessions for future runs. *)

val default : t
(** [default] is a session to be used to make the first run of the concolic evaluator. *)

val load_branches : t -> Jayil.Ast.expr -> t
(** [load_branches session expr] has the AST branches loaded from [expr] into [session]. *)

val next : t -> [ `Done of t | `Next of t * Concolic.t * Eval.t ]
(** [next session] is [`Done session'] when there is no satisfiable or unhit target left in [session'],
    or it is a new session with a concolic session and eval session to try to hit the top target. *)

val finish : t -> t
(** [finish session] is [session] with the finished branch store (i.e. unhit branches set as unreachable). *)

val print : t -> unit
(** [print session] prints the branch store. *)

val accum_concolic : t -> Concolic.t -> t
(** [accum_concolic session concolic_session] is a new session that merges the results of [concolic_session] into
    [session]. The new session is then ready to be called with [next] to begin another run. *)