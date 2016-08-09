(** A module defining the interface of a context stack. *)

open Ddpa_graph;;

module type Context_stack =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val push : abstract_clause -> t -> t
  val pop : t -> t (* TODO: Shouldn't this be able to signal failure? *)
  val is_top : abstract_clause -> t -> bool
  val pp : Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.json
  val show : t -> string
  val name : string
end;;
