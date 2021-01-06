open Core
module T = struct
  type t = (Id.t * Id.t) list
  [@@deriving sexp, compare, equal, show {with_path = false}]
end

include T
include Comparator.Make(T)

(* type show_t = (Id.t * Id.t) list 
   [@@deriving show] *)