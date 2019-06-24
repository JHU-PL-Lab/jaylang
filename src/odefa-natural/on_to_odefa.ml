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


let translator (e : On_ast.expr) : (Odefa_ast.Ast.expr * Odefa_ast.Ast.var) =
  match e with
  | Var (id) ->
    raise (Failure "")
  | _ -> raise (Failure "")
    (* TODO: add record projection (RecProj) !!!*)


;;
