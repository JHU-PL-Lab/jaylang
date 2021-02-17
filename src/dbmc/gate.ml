open Core

type t

type node =
  | Pending
  | Mismatch of Id.t
  | Done of Id.t
  | Pass of Id.t * node ref 
  | And of Id.t * node ref list
  | GuardedChoice of Id.t * node ref list * node ref list
  | Choice of Id.t * node ref list
[@@deriving show {with_path = false}]

let for_all_refs f tree_refs = 
  List.for_all tree_refs ~f:(fun c ->
      f !c
    )

let exists_refs f tree_refs = 
  List.exists tree_refs ~f:(fun c ->
      f !c
    )

let rec check_valid_tree = function
  | Pending -> false
  | Mismatch _ -> false
  | Done _ -> true
  | Pass (_, child) -> check_valid_tree !child
  | And (_, childs) ->  for_all_refs check_valid_tree childs
  | GuardedChoice (_, guards, choices) ->
    (for_all_refs check_valid_tree guards)
    && (exists_refs check_valid_tree choices)
  | Choice (_, choices) ->
    exists_refs check_valid_tree choices

let sum f childs = List.sum (module Int) childs ~f:(fun child -> f !child)

let rec size = function
  | Pending -> 1
  | Mismatch _ -> 1
  | Done _ -> 1
  | Pass (_, child) -> 1 + size !child
  | And (_, childs) -> 1 + sum size childs
  | GuardedChoice (_, guards, choices) ->
    1 
    + sum size guards
    + sum size choices
  | Choice (_, choices) ->
    1 
    + sum size choices
