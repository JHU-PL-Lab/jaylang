open Batteries;;
open Jhupllib;;
open Odefa_ast;;

(** This module contains utilities for picking fresh names.
    Plucked from Compiler labs. Will change if necessary. *)

(** This creates a fresh name counter so we can easily get fresh names during
    the A-normalization process. *)
let _fresh_name_counter = ref 0;;

(** This function generates a fresh name with the provided prefix. *)
let fresh_name (prefix : string) : string =
  let n = !_fresh_name_counter + 1 in
  _fresh_name_counter := n;
  prefix ^ "_" ^ string_of_int n
;;


let flatten_expr (e : On_ast.expr) : (Odefa_ast.Ast.clause list * Odefa_ast.Ast.var) =
  raise @@ Utils.Not_yet_implemented "translator"

;;

let translate (e : On_ast.expr) : Odefa_ast.Ast.expr =
  let (c_list, _) = flatten_expr e in
  Ast.Expr(c_list)
;;
