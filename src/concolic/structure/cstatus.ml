
type t =
  | In_progress
  | In_progress_pruned
  | Found_abort of (Jil_input.t list [@compare.ignore])
  | Type_mismatch of (Jil_input.t list [@compare.ignore])
  | Exhausted
  | Exhausted_pruned_tree
  [@@deriving compare, sexp]

let update_for_pruned (cstatus : t) : t =
  match cstatus with
  | In_progress -> In_progress_pruned
  | Exhausted -> Exhausted_pruned_tree
  | _ -> cstatus