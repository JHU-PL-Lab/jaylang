
open Core

module M = Map.Make (Branch)

type t = int M.t [@@deriving compare]

let empty = M.empty

let get_count (m : t) (b : Branch.t) : int =
  match Map.find m b with
  | Some i -> i
  | None -> 1

let hit_branch (m : t) (b : Branch.t) : t =
  Map.update m b ~f:(function Some i -> i + 1 | None -> 1)

let merge (m1 : t) (m2 : t) : t =
  Map.merge m1 m2 ~f:(fun ~key:_ ->
    function
    | `Both (a, b) -> Some (a + b)
    | `Left a | `Right a -> Some a
    )
