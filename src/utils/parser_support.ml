open Ast_uid;;
open Source_origin;;

let ast_position_hash : file_region Ast_uid_hashtbl.t ref
  = ref (Ast_uid_hashtbl.create(10))
;;

let reset_ast_position_hash () =
  ast_position_hash := Ast_uid_hashtbl.create(10)
;;

let get_ast_position_hash () =
  !ast_position_hash
;;