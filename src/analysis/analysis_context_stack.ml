(** A module defining the interface of a context stack. *)

open Batteries;;

open Ast;;

module type Context_stack =
sig
  type t
  val compare : t -> t -> int
  val empty : t
  val push : clause -> t -> t
  val pop : t -> t (* TODO: Shouldn't this be able to signal failure? *)
  val is_top : clause -> t -> bool
  val pretty : t -> string
  (* TODO: hopefully eliminate this function in favor of a symbolic
           representation? *)
  val enumerate : expr -> t Enum.t
end;;