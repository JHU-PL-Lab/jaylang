
module T :
  sig
    type t [@@deriving hash, compare, equal, sexp]
  end

type t = T.t [@@deriving hash, compare, equal, sexp]

val create : Jayil.Ast.ident -> Fun_depth.t -> t
(** [create id n] is the key for clause ident [id] that is at function depth [n]. *)

val to_string : t -> string

val x : t -> Jayil.Ast.ident

val n : t -> Fun_depth.t
