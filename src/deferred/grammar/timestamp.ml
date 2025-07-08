
open Core

module Time = struct
  type t =
    | Time of int [@@unboxed]
    [@@deriving compare, equal]

  let one = Time 1
  let plus_one (Time t) = Time (t + 1)

  let to_string (Time t) = Int.to_string t
end

(* I'm thinking that some run length encoding could help, but because
  we are incrementing instead of pushing whole values, it's not easy to recognize. *)
module T = struct
  type t =
    | Timestamp of Time.t list [@@unboxed]
end

include T

let empty = Timestamp [ Time.one ]

let push (Timestamp ls) = Timestamp (Time.one :: ls)

let increment = function
  | Timestamp [] -> failwith "disallowed empty timestamp"
  | Timestamp (hd :: tl) -> Timestamp (Time.plus_one hd :: tl)

let compare (Timestamp a) (Timestamp b) =
  List.compare Time.compare (List.rev a) (List.rev b)

let equal a b = compare a b = 0

let to_string : t -> string = function
  | Timestamp ls ->
    List.fold_left ls ~init:"" ~f:(fun acc t ->
      acc ^ "." ^ Time.to_string t
      )

module Map = Baby.W.Map.Make (struct
  type t = T.t
  let compare = compare
end)
