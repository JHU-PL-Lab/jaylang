
module Input : sig
  type t =
    | Int of int
    | Bool of bool
    [@@deriving compare, sexp]
end

type t =
  { get_int : Concolic_key.t -> int
  ; get_bool : Concolic_key.t -> bool }
(** [t] gets an input to go for the clause with the provided key. *)

val zero : t
(** [zero] is 0 or false. *)

val default : t
(** [default] is random in -10, 10 inclusive or a random bool. *)

val from_model : Z3.Model.model -> t
(** [from_model model] is a feeder that satisfies the given [model]. *)
