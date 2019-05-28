(** This module contains public type definitions for the symbolic monad.  This
    prevents duplication of these types between interface and implementation. *)

open Sat_types;;

(** A type representing the metadata tracked about each non-deterministic
    computation. *)
type metadata = {
  md_steps : int;
};;

(** A type representing the result of a non-deterministic computation (when it
    reaches its conclusion. *)
type 'a result = {
  rs_value : 'a;
  rs_metadata : metadata;
  rs_formulae : Formula_set.t;
};;

(** A type representing the state of a computation. *)
type 'a computation_state =
  | Computation_finished of 'a result
  | Computation_suspended
;;
