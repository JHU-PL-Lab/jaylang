open Core

module Lazy =
  struct
    type t = unit -> Lookup_key.t    

    let to_key (x : t) : Lookup_key.t =
      x ()

    let generate_lookup_key (x : Jayil.Ast.ident) (stk : Dj_common.Concrete_stack.t) : Lookup_key.t =
      Lookup_key.without_block x
      @@ Rstack.from_concrete stk
    
    let make (x : Jayil.Ast.ident) (stk : Dj_common.Concrete_stack.t) : t =
      fun () -> generate_lookup_key x stk

  end

module T =
  struct
    type t =
      { x : Jayil.Ast.Ident_new.t
      ; n : Int.t } (* n is depth in tree *)
      [@@deriving hash, compare, equal]
  end

include T

let generate (x : Jayil.Ast.ident) (n : int) : t =
  { x ; n }

let to_string ({ x ; n } : t) : string = 
  Format.sprintf "%s_$%s$" (Dj_common.Id.show x) (Int.to_string n)

module Lazy2 =
  struct
    type t = unit -> T.t    

    let to_key (x : t) : T.t =
      x ()

    let make (x : Jayil.Ast.ident) (n : int) : t =
      fun () -> generate x n

  end