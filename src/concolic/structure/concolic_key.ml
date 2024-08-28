open Core

module T =
  struct
    type t =
      { x : Jayil.Ast.Ident_new.t
      ; n : Int.t } (* n is number of functions entered in path to variable *)
      [@@deriving hash, compare, equal, sexp]
  end

include T

let generate (x : Jayil.Ast.ident) (fun_depth : int) : t =
  { x ; n = fun_depth }

let to_string ({ x ; n } : t) : string = 
  Format.sprintf "%s_$%s$" (Dj_common.Id.show x) (Int.to_string n)

let x ({ x ; _ } : t) : Jayil.Ast.ident =
  x

module Lazy =
  struct
    type t = unit -> T.t    

    let to_key (x : t) : T.t =
      x ()

    let make (x : Jayil.Ast.ident) (fun_depth : int) : t =
      fun () -> generate x fun_depth
  end