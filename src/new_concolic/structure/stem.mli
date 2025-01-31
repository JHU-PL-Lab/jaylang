type t =
  | Root
  | Beginning_from of Target.t
  | Bool_branch of { claim : bool Claim.t; tail : t }
  | Int_branch of { claim : int Claim.t ; other_cases : int Claim.t list ; tail : t }

val empty : t
(** [empty] is [Root] *)

val of_target : Target.t -> t
(** [of_target target] is a stem beginning from [target]. *)

val push_branch : t -> bool Direction.t -> bool Expression.t -> t
(** [push_branch stem direction expression] has branched off of [stem] with the claim
    that the [expression] takes the [direction]. *)

val push_case : t -> int Direction.t -> int Expression.t -> int list -> t
(** [push_branch stem direction expression other_int_cases] has branched off of [stem] with the claim
    that the [expression] takes the [direction], where all of the [other_int_cases] are given so that
    the default direction can be computed if needed. *)

val to_rev_path : t -> Path.Reverse.t
(** [to_rev_path stem] is a reverse path representing the [stem], including its beginning target, if any. *)
