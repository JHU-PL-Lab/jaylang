
type _ t =
  | Int_key : int -> int t
  | Bool_key : int -> bool t

val compare : 'a t -> 'a t -> int

val equal : 'a t -> 'a t -> bool

val to_string : 'a t -> string

val uniq_id : 'a t -> int
