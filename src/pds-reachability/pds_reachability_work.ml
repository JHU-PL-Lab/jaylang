open Pds_reachability_basis;;
open Pds_reachability_types;;

(** This module specifies the type used to describe work in a PDS reachability
    analysis as well as the interface for managing a pending work collection. *)
module type Work_type =
sig
  (** The basis module for the PDS reachability analysis. *)
  module B : Basis;;

  (** The types module for the PDS reachability analysis. *)
  module T : Types
    with type state = B.state
     and type stack_element = B.stack_element;;

  (** The type of a work unit. *)
  type work =
    | Expand_node of T.node
    | Introduce_edge of T.edge
    | Introduce_untargeted_dynamic_pop of
        T.node * T.untargeted_dynamic_pop_action
  ;;

  (** An equality test for work unit. *)
  val equal_work : work -> work -> bool

  (** A comparator for work units. *)
  val compare_work : work -> work -> int

  (** A pretty-printer for work units. *)
  val pp_work : Format.formatter -> work -> unit

  (** A conversion from work units to strings. *)
  val show_work : work -> string
end;;

module Make
    (B : Basis)
    (T : Types with type state = B.state
                and type stack_element = B.stack_element)
  : Work_type with module B = B
               and module T = T
=
struct
  module B = B;;
  module T = T;;
  type t_node = T.node;;
  type t_edge = T.edge;;
  let equal_t_node = T.equal_node;;
  let equal_t_edge = T.equal_edge;;
  let compare_t_node = T.compare_node;;
  let compare_t_edge = T.compare_edge;;
  let pp_t_node = T.pp_node;;
  let pp_t_edge = T.pp_edge;;
  type work =
    | Expand_node of t_node
    | Introduce_edge of t_edge
    | Introduce_untargeted_dynamic_pop of
        T.node * T.untargeted_dynamic_pop_action
    [@@deriving eq, ord, show]
  ;;
end;;
