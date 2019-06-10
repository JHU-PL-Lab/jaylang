(** This module contains data types for the SAT solving process used by
    symbolic interpretation. *)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Interpreter_types;;
open Pp_utils;;

(** The type of right-hand sides of formulae generated during symbolic
    interpretation. *)
type formula_expression =
  | Formula_expression_binop of symbol * binary_operator * symbol
  | Formula_expression_alias of symbol
  | Formula_expression_value of value
[@@deriving eq, ord, to_yojson]
;;

let pp_formula_expression : formula_expression pretty_printer =
  fun formatter e ->
  match e with
  | Formula_expression_binop (x,op,y) ->
    pp_symbol formatter x;
    Format.pp_print_char formatter ' ';
    pp_binary_operator formatter op;
    Format.pp_print_char formatter ' ';
    pp_symbol formatter y;
  | Formula_expression_alias x ->
    pp_symbol formatter x;
  | Formula_expression_value v ->
    pp_value formatter v;
;;

(** The type of formulae which are generated during symbolic interpretation. *)
type formula =
  | Formula of symbol * formula_expression
[@@deriving eq, ord, to_yojson]
;;

let pp_formula : formula pretty_printer =
  fun formatter formula ->
  let Formula(x,e) = formula in
  pp_symbol formatter x;
  Format.pp_print_string formatter " = ";
  pp_formula_expression formatter e;
;;

module Formula_expression = struct
  type t = formula_expression [@@deriving eq, ord, show, to_yojson];;
end;;

module Formula = struct
  type t = formula [@@deriving eq, ord, show, to_yojson];;
end;;

module Formula_set = struct
  module S = Set.Make(Formula);;
  include S;;
  include Pp_utils.Set_pp(S)(Formula);;
  include Yojson_utils.Set_to_yojson(S)(Formula);;
end;;
