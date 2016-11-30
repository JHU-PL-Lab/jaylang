(** A "context stack" which performs no actions. *)

open Jhupllib;;

open Ddpa_context_stack;;
open Pp_utils;;

module Stack : Context_stack =
struct
  type t = unit;;
  let equal () () = true;;
  let compare () () = 0;;
  let empty = ();;
  let push _ () = ();;
  let pop () = ();;
  let is_top _ () = true;;
  let pp formatter () = Format.pp_print_string formatter "-";;
  let show = pp_to_string pp;;
  let to_yojson _ = `List [];;
  let name = "0ddpa";;
end;;
