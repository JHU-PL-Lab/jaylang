(** A module defining the interface of a context stack. *)

open Cba_graph;;

module type Context_stack =
sig
  type t
  val compare : t -> t -> int
  val empty : t
  val push : abstract_clause -> t -> t
  val pop : t -> t (* TODO: Shouldn't this be able to signal failure? *)
  val is_top : abstract_clause -> t -> bool
  val pretty : t -> string
  val pretty_abbrv : t -> string
end;;