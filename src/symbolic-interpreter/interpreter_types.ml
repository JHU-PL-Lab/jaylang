(** This module defines basic data types used by the symbolic interpreter. *)

open Batteries;;
open Jhupllib;;
open Odefa_ast;;

open Ast;;
open Relative_stack;;

(** An enumeration of special symbols used throughout the interpreter. *)
type special_symbol =
  | SSymTrue
[@@deriving eq, ord, show, to_yojson]
;;

(** The type of a symbol in the symbolic interpreter.  This is essentially the
    type of a variable using a stack-costack pair rather than a freshening
    stack. *)
type symbol =
  | Symbol of Ident.t * relative_stack
  | SpecialSymbol of special_symbol
[@@deriving eq, ord, show, to_yojson]
;;

module Symbol = struct
  type t = symbol [@@deriving eq, ord, show, to_yojson];;
end;;

module Symbol_map = struct
  module M = Map.Make(Symbol);;
  include M;;
  include Pp_utils.Map_pp(M)(Symbol);;
  include Yojson_utils.Map_to_yojson(M)(Symbol);;
end;;
