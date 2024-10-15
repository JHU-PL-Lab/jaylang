(**
   This module contains resources used by the Menhir generated parser which
   are then manipulated by the parser facade. It is not to be used
   directly.
*)

open Source_origin;;
open Uid;;

(** Resets the contents of the global file region mapping. *)
val reset : unit -> unit

(** Puts a new entry into the global file region mapping. *)
val put : uid -> file_region -> unit

(** Retrieves all file region mappings as an independent dictionary. *)
val get_all : unit -> file_region Uid_map.t

(** A routine the parser can use to store position information about the AST as
    it is parsed.  This function accepts the start and end location of a
    Menhir production.  It stores these locations in the global file region
    mapping under a fresh UID and then returns the UID it used. *)
val next_uid : Lexing.position -> Lexing.position -> uid
