open Batteries;;

let _next_uid = ref 0;;

type uid = Uid of int
  [@@deriving eq, ord, show]
;;

let next_uid () =
  let n = !_next_uid in
  _next_uid := n+1;
  Uid n
;;

module Uid_ord : Interfaces.OrderedType with type t = uid =
struct
  type t = uid
  let compare = compare_uid
end;;

module Uid_map = Map.Make(Uid_ord);;
