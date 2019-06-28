open Batteries;;
(* open Jhupllib;; *)

open Odefa_ast;;

(** This creates a fresh name counter so we can easily get fresh names during
    the A-normalization process. *)
let _fresh_name_counter = ref 0;;

(** This function generates a fresh name with the provided prefix. *)
let fresh_name (prefix : string) : string =
  let n = !_fresh_name_counter + 1 in
  _fresh_name_counter := n;
  prefix ^ "_" ^ string_of_int n
;;

let ast_var_from_string (name : string) : Ast.var =
  let new_ident = Ast.Ident (fresh_name name) in
  Ast.Var(new_ident, None)
;;
