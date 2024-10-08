
open Core

type t =
  { clause_name : Jayil.Ast.Ident_new.t
  ; uniq_id     : int }
  [@@deriving sexp]

let compare (a : t) (b : t) : int =
  Int.compare a.uniq_id b.uniq_id

let equal (a : t) (b : t) : bool =
  Int.(a.uniq_id = b.uniq_id)

let create clause_name uniq_id =
  { clause_name ; uniq_id }

let to_string ({ clause_name = Ident s ; uniq_id } : t) : string =
  Format.sprintf "%s_$%d" s uniq_id

let clause_name ({ clause_name ; _ } : t) : Jayil.Ast.Ident_new.t =
  clause_name

let uniq_id ({ uniq_id ; _ } : t) : int =
  uniq_id