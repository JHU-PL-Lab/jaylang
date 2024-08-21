open Core
open From_dbmc
open Dj_common
open Jayil.Ast

(**
  The concrete session tracks everything needed by the interpreter to evaluate the expression.
  Nothing here is used for any part of the concolic logic. This is all to correctly evaluate
  the expression.
*)
module Concrete :
  sig
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

    module G : (* = Graph.Imperative.Digraph.ConcreteBidirectional (Id_with_stack) *)
      sig
        type t
      end

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
        is_debug        : bool
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

module Symbolic = Symbolic_session
(** [Symbolic] is alias for [Symbolic_session]. *)

type t
(** [t] holds program info between interpretations and helps generate concrete and symbolic sessions
    for the next run. *)

val empty : t
(** [empty] is a default empty session *)

val with_options : (t -> t) Options.Fun.t
(** [with_options t] is [t] that has all relevant info loaded in from the optional arguments. *)

val of_expr : Jayil.Ast.expr -> t
(** [of_expr expr] is [empty] that knows of all branches in the [expr]. *)

val accum_symbolic : t -> Symbolic.t -> t
(** [accum_symbolic t sym] finishes the sybolic session [sym] and accumulates results into [t]. *)

val next : t -> [ `Done of (Branch_info.t * bool) | `Next of (t * Symbolic.t * Concrete.t) ]
(** [next t] is [`Done (branch_info, tree_has_been_pruned)] if the concolic evaluation is done,
    or is [`Next (session, symbolic, concrete)] if the interpreter is to be run again with [symbolic]
    and [concrete] sessions. *)

val branch_info : t -> Branch_info.t
(** [branch_info t] is the branch info in [t]. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)