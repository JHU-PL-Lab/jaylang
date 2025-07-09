
open Core

module Time = struct
  type t =
    | Time of int [@@unboxed]
    [@@deriving compare, equal, sexp]

  let one = Time 1
  let plus_one (Time t) = Time (t + 1)

  let to_string (Time t) = Int.to_string t
end

module T = struct
  type t =
    | Timestamp of Time.t list [@@unboxed]
    [@@deriving equal, sexp]
    (* for efficiency and library support, we use list instead of nonempty list *)
end

include T

let empty = Timestamp [ Time.one ]

let push (Timestamp ls) = Timestamp (Time.one :: ls)

let increment = function
  | Timestamp [] -> failwith "disallowed empty timestamp"
  | Timestamp (hd :: tl) -> Timestamp (Time.plus_one hd :: tl)

let compare (Timestamp a) (Timestamp b) =
  List.compare Time.compare (List.rev a) (List.rev b)

let to_string : t -> string = function
  | Timestamp ls ->
    List.fold_left ls ~init:"" ~f:(fun acc t ->
      acc ^ "." ^ Time.to_string t
    )

include Comparator.Make (struct
  include T
  let compare = compare
end)
