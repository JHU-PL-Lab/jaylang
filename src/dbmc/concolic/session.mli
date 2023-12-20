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

module Concolic :
  sig
    module Outcome :
      sig
        type t =
          | Hit_target
          | Found_abort
          | Reach_max_step
      end

    module Outcome_set :
      sig
         type t
      end

    type t =
      { branch_solver : Branch_solver.t
      ; cur_parent    : Branch_solver.Parent.t
      ; parent_stack  : Branch_solver.Parent.t list (* previous parents to revert back to when exiting branches *)
      ; cur_target    : Branch.Runtime.t option
      ; new_targets   : Branch.Runtime.t list
      ; outcomes      : Outcome_set.t (* Note: it's possible to hit the target and reach abort later, so we need multiple outcomes *)
      ; hit_branches  : (Branch.Runtime.t * Branch.Status.t) list
      ; inputs        : (Ident.t * Dvalue.t) list }

    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formula session expr] adds the [expr] under the current parent of the [session]. *)

    val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
    (** [add_key_eq_val session k v] sets [k = v] in the [session]. This is a special case of [add_formula]. *)

    val add_siblings : t -> Lookup_key.t -> siblings:Lookup_key.t list -> t
    (** [add_siblings session key siblings] adds all dependencies of [siblings] to the [key] so that the
        [key] also depends on them.
        
        NOTE: I think this is not needed anymore. *)

    val found_abort : t -> t
    (** [found_abort session] adds the info that an abort was found under the current parent. *)

    val enter_branch : t -> Branch.Runtime.t -> t
    (** [enter_branch session branch] sets the new parent as [branch] and hits the branch. *)

    val exit_branch : t -> Lookup_key.t -> t
    (** [exit_branch session ret_key] uses the final key [ret_key] in the branch to exit and return
        to previous parent. Also cleans up formulas in the solver. *)

    val add_input : t -> Ident.t -> Dvalue.t -> t
    (** [add_input session x v] adds the fact that [v] was fed to variable [x] as an input. *)
  end

module Target_stack :
  sig
    type t (* = (Branch_solver.t * Branch.Runtime.t) list *)
  end

type t = 
  { branch_store        : Branch.Status_store.t
  ; persistent_formulas : Branch_solver.Formula_set.t
  ; target_stack        : Target_stack.t
  ; global_max_step     : int
  ; run_num             : int }

val create_default : unit -> t
(** [create_default ()] is a session to be used for to make the first run of the concolic evaluator. *)

val load_branches : t -> Jayil.Ast.expr -> t
(** [load_branches session expr] has the AST branches loaded from [expr] into [session]. *)

val next : t -> [ `Done of t | `Next of t * Concolic.t * Eval.t ]
(** [next session] is [`Done session'] when there is no satisfiable or unhit target left in [session'],
    or it is a new session with a concolic session and eval session to try to hit the top target. *)

(* val is_finished : t -> bool *)
(** [is_finished session] is true if there are no satisfiable or unhit targets left. *)

val finish : t -> t
(** [finish session] is [session] with the finished branch store (i.e. unhit branches set as unreachable). *)

val print : t -> unit
(** [print session] prints the branch store. *)

val accum_concolic : t -> Concolic.t -> t

(* val hit_branch : ?new_status:Branch.Status.t -> t -> Branch.Runtime.t -> t *)
(** [hit_branch ~new_status session ast_branch] has the given [ast_branch] set with the [new_status].
    This may add to the persistent formulas if the status is abort. *)

(* val add_targets : t -> Branch.Runtime.t list -> Branch_solver.t -> t *)
(** [add_targets session target_list branch_solver] adds the [target_list] to the worklist to be later solved
    for using the given [branch_solver]. *)
