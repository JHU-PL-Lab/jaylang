
module Lazy :
  sig
    type t = unit -> Lookup_key.t
    val to_key : t -> Lookup_key.t
    (* val generate_lookup_key : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> Lookup_key.t *)
    val make : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t
  end

module T :
  sig
    type t [@@deriving hash, compare, equal, sexp]
  end

type t = T.t [@@deriving hash, compare, equal, sexp]

val generate : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t

val to_string : t -> string

val x : t -> Jayil.Ast.ident

module Lazy2 :
  sig
    type t = unit -> T.t
    val to_key : t -> T.t
    val make : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t
  end
