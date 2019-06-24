open Batteries;;
open Jhupllib;;
open Odefa_natural;;
open Odefa;;
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


let translator (e : On_ast.expr) : (expr * var) =
  match e with
  | Var (id) ->
    raise NotImplementedError
  | Function (id, e) ->
    raise NotImplementedError
  | Appl (e1, e2) ->
    raise NotImplementedError
  | Let (varname, e1, e2) ->
    raise NotImplementedError
  | LetRec (ident, ident_list, expr, expr) ->
raise NotImplementedError

  | Plus (e1, e2) ->
    raise NotImplementedError
  | Minus (e1, e2) ->
    raise NotImplementedError
  | Equal (e1, e2) ->
    raise NotImplementedError
  | And (e1, e2) ->
    raise NotImplementedError
  | Or (e1, e2) ->
    raise NotImplementedError
  | Not (e) ->
    raise NotImplementedError
  | If (e1, e2, e3) ->
raise NotImplementedError
  | Int (n) ->
raise NotImplementedError
  | Bool (b) ->
raise NotImplementedError
  | Record (rlist) ->
raise NotImplementedError
    (* TODO: add record projection (RecProj) !!!*)


;;
