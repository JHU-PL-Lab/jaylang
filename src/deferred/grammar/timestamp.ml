
open Core

module T = struct
  type t =
    | Timestamp of int list [@@unboxed]
end

include T

let empty = Timestamp [ 1 ]

let push (Timestamp ls) = Timestamp (1 :: ls)

let increment = function
  | Timestamp [] -> failwith "disallowed empty timestamp"
  | Timestamp (hd :: tl) -> Timestamp (hd + 1 :: tl)

let compare (Timestamp a) (Timestamp b) =
  List.compare Int.compare (List.rev a) (List.rev b)

let equal a b = compare a b = 0

let to_string : t -> string = function
  | Timestamp ls ->
    List.fold_left ls ~init:"" ~f:(fun acc i ->
      acc ^ "." ^ Int.to_string i
      )

module Map = Baby.W.Map.Make (struct
  type t = T.t
  let compare = compare
end)
