

open Core

type t =
  { id       : Jayil.Ast.Ident_new.t
  ; x        : int 
  ; is_const : bool }
  [@@deriving sexp]

let compare (a : t) (b : t) : int =
  Int.compare a.x b.x

let equal (a : t) (b : t) : bool =
  Int.(a.x = b.x)

let create (id : Jayil.Ast.Ident_new.t) (x : int) (is_const : bool) : t =
  { id ; x ; is_const }

let set_not_const (x : t) : t =
  { x with is_const = false }

let to_string ({ id = Ident s ; x ; is_const } : t) : string =
  Format.sprintf "%s_$%d(is_const:%b)" s x is_const

let id ({ id ; _ } : t) : Jayil.Ast.Ident_new.t =
  id

let x ({ x ; _ } : t) : int =
  x

let is_const ({ is_const ; _ } : t) : bool =
  is_const

let with_dependencies (x : t) (deps : t list) : t =
  if List.for_all deps ~f:is_const
  then x
  else { x with is_const = false }