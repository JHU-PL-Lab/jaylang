open Jhupllib;;

(**
   A type for modules which carry a value type along with a few common
   operations on that type.
*)
module type Decorated_type =
(* TODO: consider -- do we want to just have this in jhupllib? *)
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Pp_utils.pretty_printer
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
end;;
