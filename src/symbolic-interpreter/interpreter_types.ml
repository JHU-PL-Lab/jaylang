(** This module defines basic data types used by the symbolic interpreter. *)

open Odefa_ast;;

open Ast;;
open Relative_stack;;

(** The type of a symbol in the symbolic interpreter.  This is essentially the
    type of a variable using a stack-costack pair rather than a freshening
    stack. *)
type symbol =
  | Symbol of Ident.t * relative_stack
[@@deriving eq, ord, show, to_yojson]
;;

module Symbol = struct
  type t = symbol [@@deriving eq, ord, show, to_yojson];;
end;;
