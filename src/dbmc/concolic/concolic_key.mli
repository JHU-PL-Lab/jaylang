
module Lazy :
  sig
    type t = unit -> Lookup_key.t
    val to_key : t -> Lookup_key.t
    (* val generate_lookup_key : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> Lookup_key.t *)
    val make : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t
  end

module T :
  sig
    type t [@@deriving hash, compare, equal]
  end

type t = T.t [@@deriving hash, compare, equal]

val generate : Jayil.Ast.ident -> int -> t

val to_string : t -> string

module Lazy2 :
  sig
    type t = unit -> T.t
    val to_key : t -> T.t
    val make : Jayil.Ast.ident -> int -> t
  end
