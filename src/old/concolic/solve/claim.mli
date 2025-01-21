

type t =
  | Int_equality of Concolic_key.t * int Expression.t
  | Bool_equality of Concolic_key.t * bool Expression.t

val get_formulas : t list -> Expression.Cache.t -> bool C_sudu.Gexpr.t list
(** [get_formulas ls cache] uses the [cache] to make the equalities given by the claims
    in [ls], where the keys are equal to both the given expression and the expression in
    cache. *)