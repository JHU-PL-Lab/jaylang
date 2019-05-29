(** This module contains data types for the SAT solving process used by
    symbolic interpretation. *)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Interpreter_types;;

(** The type of right-hand sides of formulae generated during symbolic
    interpretation. *)
type formula_expression =
  | Formula_expression_binop of symbol * binary_operator * symbol
  | Formula_expression_alias of symbol
  | Formula_expression_value of value
[@@deriving eq, ord, show, to_yojson]
;;

(** The type of formulae which are generated during symbolic interpretation. *)
type formula =
  | Formula of symbol * formula_expression
[@@deriving eq, ord, show, to_yojson]
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

module Symbol_map = struct
  module Impl = Map.Make(Ident)
  include Impl
  include Pp_utils.Map_pp(Impl)(Ident)
  include Yojson_utils.Map_to_yojson(Impl)(Ident)
end
