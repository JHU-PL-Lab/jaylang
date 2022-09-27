open Batteries;;

(** The abstract type of UIDs in the system. *)
type uid

(** A function which returns a fresh UID. *)
val next_uid : unit -> uid

(** A function to compare UIDs. *)
val compare_uid : uid -> uid -> int

(** A function to check UIDs for equality. *)
val equal_uid : uid -> uid -> bool

(** A pretty-printer for UIDs. *)
val pp_uid : Format.formatter -> uid -> unit

(** A function to convert a UID to a string. *)
val show_uid : uid -> string

(** An ordering module for UIDs. *)
module Uid_ord : Interfaces.OrderedType with type t = uid

(** A dictionary data structure for UIDs. *)
module Uid_map : BatMap.S with type key = uid
