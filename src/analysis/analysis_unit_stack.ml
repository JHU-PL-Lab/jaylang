(** A "context stack" which performs no actions. *)

open Analysis_context_stack;;

module Stack : Context_stack =
struct
  type t = unit;;
  let compare () () = 0;;
  let empty = ();;
  let push _ () = ();;
  let pop () = ();;
  let is_top _ () = true;;
  let pretty () = "-";;
end;;