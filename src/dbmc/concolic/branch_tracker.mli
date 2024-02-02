module Input :
  sig
    type t = (Lookup_key.t * int) list
    (* [t] is the input for an entire run. A run can have multiple inputs *)
  end

module rec Status :
  sig
    module T :
      sig
        type t =
          | Hit of Input.t list (* can be hit on multiple runs *)
          | Unhit
          | Unsatisfiable
          | Found_abort of Input.t list
          | Reach_max_step of int (* counter for how many times it has reached max step *)
          | Missed of Status_store.Without_payload.t (* what were the statuses as of the solve for the miss *)
          | Unreachable_because_abort
          | Unreachable_because_max_step
          | Unknown of Status_store.Without_payload.t (* similar to missed *)
          | Unreachable
      end

    include module type of T
    val to_string : t -> string

    module Without_payload :
      sig
        type t =
          | Hit
          | Unhit
          | Unsatisfiable
          | Found_abort
          | Reach_max_step
          | Missed                       (* Not used in final report *)
          | Unreachable_because_abort    (* Unused *)
          | Unreachable_because_max_step (* Unused *)
          | Unknown
          | Unreachable                  (* Unused *)

        val to_string : t -> string
        val t_of_with_payload : T.t -> t
        val with_payload_of_t : t -> T.t
        
      end
  end

(*
  A `Status_store` tracks how AST branches are hit. It maps branch identifiers to their status.
*)
and Status_store :
  sig
    module type S = 
      sig
        type status_t 
        type t [@@deriving sexp, compare]
        (** [t] is a map from a branch identifier to the status of the branch. So it tells
            us whether the true and false direction of each branch have been hit. *)

        val empty : t
        (** [empty] has no information on any branches *)

        val of_expr : Jayil.Ast.expr -> t
        (** [of_expr expr] has all branches unhit that exist in the given [expr]. *)

        val print : t -> unit
        (** [print store] prints the statuses of all branches in the [store] to stdout. *)

        val add_branch_id : t -> Jayil.Ast.ident -> t
        (** [add_branch_id store id] is a new store where the identifier [id] has been added to
            the branch store [store], and both directions of the new branch are unhit. *)

        (* val get_unhit_branch : t -> Branch.t option *)
        (** [get_unhit_branch store] is some branch that is unhit. *)

        val set_branch_status : new_status:status_t -> t -> Branch.t -> t
        (** [set_branch_status status store branch] is a new store where the given [branch] now has the
            [status]. All other branches are unaffected. *)

        val is_hit : t -> Branch.t -> bool
        (** [is_hit store branch] is true if and only if the status of [branch.branch_ident] in 
            the [store] has [branch.direction] as [Hit]. *)

        val get_status : t -> Branch.t -> status_t
        (** [get_status store branch] is the status of the given [branch]. *)

        val find_branches : Jayil.Ast.expr -> t -> t
        (** [find_branches e store] is a new store where all the branches in the given expression [expr]
            have been added as unhit branches to the given [store]. *)

        val finish : t -> int -> bool -> t
        (** [finish store allowed_max_step has_quit] is a new store where all unhit branches are now marked as unsatisfiable. *)

        val contains : t -> status_t -> bool
      end

    module Without_payload : S with type status_t := Status.Without_payload.t
    include S with type status_t := Status.t
    val without_payload : t -> Without_payload.t
  end

module Runtime :
  sig
    type t
    (** [t] tracks branches as they are hit during a run of the interpreter. *)

    val empty : t
    (** [empty] is a runtime branch tracker in global state with no info. *)

    val with_target : Branch.t -> t
    (** [with_target target] is [empty] that knows the given [target]. *)

    val hit_branch : t -> Branch.t -> t
    (** [hit_branch t branch] marks [branch] as hit in [t] and enters the branch. *)

    val exit_branch : t -> t
    (** [exit_branch t] exits the current branch in [t]. *)

    val found_abort : t -> t
    (** [found_abort t] tells [t] that abort was found in the current branch. *)
    
    val reach_max_step : t -> t
    (** [reach_max_step t] tells [t] that max step was reached in the current branch. *)
  end

type t
(** [t] tracks AST branches with statuses and collects results from the runtime tracker. *)

val empty : t
(** [empty] is a branch tracker with no info and default values. *)

val of_expr : Jayil.Ast.expr -> t
(** [of_expr expr] is [empty] with all branches in [expr] loaded as unhit. *)

val collect_runtime : t -> Runtime.t -> Input.t -> t
(** [collect runtime t runtime i] is [t] with all hit branches (and/or abort/max-step results)
    from [runtime], knowing the input [i] was used to make the runtime. *)

val set_unsatisfiable : t -> Branch.t -> t
(** [set_unsatisfiable t branch] is [t] with the [branch] having status unsatisfiable. *)

val set_status : t -> Branch.t -> Status.t -> t
(** [set_status t branch status] is [t] with the [branch] having the given [status]. *)

val next_target : t -> Branch.t option * t
(** [next_target t] is an optional target and a new branch tracker after popping off the target.
    The result depends on which targets have been found by collecting runtime trackers and the
    already known status inside [t]. *)

val get_aborts : t -> Branch.t list
(** [get_aborts t] is all branches known to contain abort in [t]. *)

val get_max_steps : t -> Branch.t list
(** [get_max_steps t] is all branches known to have hit max step too many times in [t]. *)

val finish : t -> bool -> t
(** [finish t has_quit] sets statuses in [t] to unreachable or hit accordingly, such that results are ready
    to be nicely printed. [has_quit] is true iff some concolic evaluation quit via control flow. *)

val print : t -> unit
(** [print t] prints the branch statuses in [t]. *)

val status_store : t -> Status_store.t
(** [status_store t] gets the status_store from [t]. *)

val set_unknown : t -> Branch.t -> t
(** [set_unknown t b] sets [b] to have unknown status in [t]. *)

val set_missed : t -> Branch.t -> t
(** [set_missed t b] sets [b] to have been missed as a target in [t]. *)