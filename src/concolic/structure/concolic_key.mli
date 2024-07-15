
module T :
  sig
    type t [@@deriving hash, compare, equal, sexp]
  end

type t = T.t [@@deriving hash, compare, equal, sexp]

val generate : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t

val to_string : t -> string

val x : t -> Jayil.Ast.ident

val d : t -> int

module Lazy :
  sig
    type t = unit -> T.t
    val to_key : t -> T.t
    val make : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t
  end
