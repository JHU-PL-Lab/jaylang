

open Core

type t =
  { id : Jayil.Ast.Ident_new.t
  ; x  : int }
  [@@deriving sexp]

let compare (a : t) (b : t) : int =
  Int.compare a.x b.x

let equal (a : t) (b : t) : bool =
  Int.(a.x = b.x)

let create (id : Jayil.Ast.Ident_new.t) (x : int) : t =
  { id ; x }

let to_string ({ id = Ident s ; x } : t) : string =
  Format.sprintf "%s_$%d" s x

let id ({ id ; _ } : t) : Jayil.Ast.Ident_new.t =
  id

let x ({ x ; _ } : t) : int =
  x