(**
   This module contains resources used by the Menhir generated parser which
   are then manipulated by the parser facade. It is not to be used
   directly.
*)

open Source_origin;;
open Uid;;

(** Resets the contents of the global source origin mapping. *)
val reset : unit -> unit

(** Puts a new entry into the global source origin mapping. *)
val put : uid -> file_region -> unit

(** Retrieves all source origin mappings as an independent dictionary. *)
val get_all : unit -> file_region Uid_map.t
