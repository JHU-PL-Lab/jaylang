(** This module defines basic data types used by the symbolic interpreter. *)

open Batteries;;
open Jhupllib;;
open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Pp_utils;;
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
[@@deriving eq, ord, to_yojson]
;;

let pp_symbol : symbol pretty_printer =
  fun formatter symbol ->
  match symbol with
  | Symbol(x,relstack) ->
    pp_ident formatter x;
    pp_relative_stack formatter relstack;
  | SpecialSymbol ss ->
    begin
      match ss with
      | SSymTrue -> Format.pp_print_string formatter "#true#"
    end
;;
let show_symbol = pp_to_string pp_symbol;;

module Symbol = struct
  type t = symbol [@@deriving eq, ord, show, to_yojson];;
end;;

module Symbol_set = struct
  module M = Set.Make(Symbol);;
  include M;;
  include Pp_utils.Set_pp(M)(Symbol);;
  include Yojson_utils.Set_to_yojson(M)(Symbol);;
end;;

module Symbol_map = struct
  module M = Map.Make(Symbol);;
  include M;;
  include Pp_utils.Map_pp(M)(Symbol);;
  include Yojson_utils.Map_to_yojson(M)(Symbol);;
end;;
