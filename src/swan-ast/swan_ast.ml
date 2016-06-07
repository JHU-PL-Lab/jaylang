(**
   Contains data type definitions for the AST of the *nested* toy language.
*)

open Ast;;
open Ast_pp;;
open Pp_utils;;

(** Expressions in the nested language. *)
type expr =
  | Record_expr of expr Ident_map.t
        [@printer
          fun formatter map ->
            Format.fprintf formatter "Swan_ast.Record_expr(\"%a\")"
              (pp_map pp_ident pp_expr Ident_map.enum) map
        ]
  | Function_expr of function_value
  | Int_expr of int
  | Bool_expr of bool
  | String_expr of string
  | Ref_expr of expr
  | Var_expr of var
  | Appl_expr of expr * expr
  | Conditional_expr of expr * pattern * function_value * function_value
  | If_expr of expr * expr * expr
  | Deref_expr of expr
  | Update_expr of expr * expr
  | Binary_operation_expr of expr * binary_operator * expr
  | Unary_operation_expr of unary_operator * expr
  | Indexing_expr of expr * expr
  | Let_expr of var * expr * expr
  | Projection_expr of expr * ident
  | Match_expr of expr * match_pair list
  [@@deriving eq, ord, show]

(** Function values in the nested language. *)
and function_value =
  | Function of var * expr
  [@@deriving eq, ord, show]


(** Patterns in the nested language. *)
and pattern =
  | Record_pattern of pattern Ident_map.t
        [@printer
          fun formatter map ->
            Format.fprintf formatter "Swan_ast.Record_pattern(\"%a\")"
              (pp_map pp_ident pp_pattern Ident_map.enum) map
        ]
  | Fun_pattern
  | Ref_pattern
  | Int_pattern
  | Bool_pattern of bool
  | String_pattern
  [@@deriving eq, ord, show]

and match_pair =
  | Match_pair of pattern * expr
  [@@deriving eq, ord, show]

;;
