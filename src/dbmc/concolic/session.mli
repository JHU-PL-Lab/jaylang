open Core
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

    module G : (* = Graph.Imperative.Digraph.ConcreteBidirectional (Id_with_stack) *) (* hide until can find type *)
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

module Symbolic :
  sig
    type t
    (** [t] tracks symbolic representations of the program during interpretation. *)

    val empty : t
    (** [empty] is a default symbolic session. *)

    val with_options : (t -> t) Concolic_options.Fun.t
    (** [with_options t] is [t] configured with the optional arguments. *)

    (*
      -----------
      EXPRESSIONS
      -----------
    *)

    val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
    (** [add_key_eq_val t k v] adds the formula that [k] has value [v] in [t]. *)

    val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in [t]. *)

    val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_binop t x op left right] adds the formula that [x = left op right] in [t]. *)

    val add_input : t -> Lookup_key.t -> Dvalue.t -> t
    (** [add_input t x v] is [t] that knows input [x = v] was given. *)

    val add_not : t -> Lookup_key.t -> Lookup_key.t -> t
    (** [add_not t x y] adds [x = not y] to [t]. *)

    val add_match : t -> Lookup_key.t -> Lookup_key.t -> Jayil.Ast.pattern -> t
    (** [add_match t x y pat] adds [x = y ~ pat] to [t]. *)

    (*
      -----------------
      CONTROL FLOW ETC.
      -----------------
    *)

    val hit_branch : t -> Branch.Runtime.t -> t
    (** [hit_branch t branch] is [t] that knows [branch] has been hit during interpretation. *)

    val fail_assume : t -> Lookup_key.t -> t
    (** [fail_assume t key] tells [t] that the variable in [key] was false when it was assumed to be true. *)

    val found_abort : t -> t
    (** [found_abort t] tells [t] that an abort was found in interpretation. *)

    val reach_max_step : t -> t
    (** [reach_max_step t] tells [t] that the max interpretation step was hit, and interpretation stopped. *)
  end

type t

val empty : t
(** [empty] is a default path tracker with no target and empty tree and stack. *)

val with_options : (t -> t) Concolic_options.Fun.t

val of_expr : Jayil.Ast.expr -> t
(** [of_expr expr] is [empty] that knows of all branches in the [expr]. *)

val accum_symbolic : t -> Symbolic.t -> t
(** [accum_symbolic t sym] finishes the sybolic session [sym] and accumulates results into [t]. *)

val next : t -> [ `Done of Branch_info.t | `Next of (t * Symbolic.t * Concrete.t) ]
(** [next t] is [`Done branch_info] if the concolic evaluation is done, or is [`Next (session, symbolic, concrete)] if
    the interpreter is to be run again with [symbolic] and [concrete] sessions. *)

val branch_info : t -> Branch_info.t
(** [branch_info t] is the branch info in [t]. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)