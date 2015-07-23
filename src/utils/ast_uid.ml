open Batteries;;

type ast_uid = Ast_uid of int;;

let next_uid_cell = ref 1;;

let next_uid () =
  let x = !next_uid_cell in
  next_uid_cell := x + 1;
  Ast_uid(x)
;;

let int_of_uid (Ast_uid n) = n;;

module Ast_uid_hash =
struct
  type t = ast_uid
  let equal (Ast_uid i) (Ast_uid j) = i=j
  let hash (Ast_uid i) = i
end;;

module Ast_uid_hashtbl = Hashtbl.Make(Ast_uid_hash)