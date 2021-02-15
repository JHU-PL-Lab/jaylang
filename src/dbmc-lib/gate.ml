open Core

type t

type node =
  | Pending
  | Mismatch
  | Done
  | Pass of node ref 
  | And of node ref list
  | GuardedChoice of node ref list * node ref list
  | Choice of node ref list
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
  | Mismatch -> false
  | Done -> true
  | Pass child -> check_valid_tree !child
  | And (childs) ->  for_all_refs check_valid_tree childs
  | GuardedChoice (guards, choices) ->
    (for_all_refs check_valid_tree guards)
    && (exists_refs check_valid_tree choices)
  | Choice choices ->
    exists_refs check_valid_tree choices

