
(* module T :
  sig
    type t [@@deriving hash, compare, equal, sexp]
  end *)

type t [@@deriving hash, compare, equal, sexp]

(* val create : Jayil.Ast.ident -> Fun_depth.t -> t *)
(** [create id n] is the key for clause ident [id] that is at function depth [n]. *)

val create : Jayil.Ast.Ident_new.t -> int -> t
(** [create id step_count] is the key for the clause [id] created at the interpreter [step_count] *)

val to_string : t -> string

val x : t -> int

val id : t -> Jayil.Ast.Ident_new.t

(* val n : t -> Fun_depth.t *)
