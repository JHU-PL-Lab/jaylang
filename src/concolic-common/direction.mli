
type int_direction =
  | Case_int of int
  | Case_default

type 'k t =
  | Bool_direction of bool * (bool, 'k) Smt.Formula.t
  | Int_direction of { dir : int_direction ; not_in : int list ; expr : (int, 'k) Smt.Formula.t }
(** ['k t] is a branch direction taken during concolic evaluation, with the symbolic formula
    for the branch condition keyed by ['k]. *)

val to_expression : 'k t -> (bool, 'k) Smt.Formula.t
(** [to_expression dir] is a symbolic formula for the concolic condition to take that direction. *)

val negations : 'k t -> (bool, 'k) Smt.Formula.t list
(** [negations dir] is all formulas for directions that are adjacent to [dir] but are not [dir]. *)
