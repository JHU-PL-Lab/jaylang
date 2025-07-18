open Core

module type S = sig
  type t
  val initial : t
  val push : t -> t
  val increment : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string

  type comparator_witness
  val comparator : (t, comparator_witness) Comparator.t
end

module Simple : S = struct
  module T = struct
    type t =
      | Timestamp of int list [@@unboxed]
    [@@deriving equal, sexp]
  end

  include T
  let initial = Timestamp [1]
  let push (Timestamp xs) = Timestamp (1::xs)
  let increment = function
    | Timestamp (x::xs) -> Timestamp ((x+1)::xs)
    | Timestamp [] -> failwith "Invariant broken: empty timestamp"
  let compare (Timestamp xs1) (Timestamp xs2) =
    List.compare compare (List.rev xs1) (List.rev xs2)
  let to_string (Timestamp xs) =
    List.fold_left (List.rev xs) ~init:"" ~f:(fun acc t ->
        acc ^ "." ^ string_of_int t
      )

  include Comparator.Make (struct
      include T
      let compare = compare
    end)
end

(* Select a default implementation *)
include Simple