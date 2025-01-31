
val reset : unit -> unit
(** [reset ()] initializes the pseudo-random number generator with a pre-selected seed.
    The same seed is used every time this function is called, and thus it resets the generator. *)

val int : int -> int
(** [int i] is a random positive integer less than [i]. *)

val bool : unit -> bool
(** [bool ()] is a random bool. *)

val int_incl : int -> int -> int
(** [int_incl a b] is an integer between [a] and [b] inclusive, or exception
    if [a > b]. *)

val any_pos_int : unit -> int
(** [any_pos_int ()] is a positive integer less than 2^63. *)