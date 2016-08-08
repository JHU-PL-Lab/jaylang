(**
   Contains data type definitions for the AST of the Swam language.
*)

open Core_ast;;
open Uid;;

type variant = Variant of string [@@deriving eq, ord];;

(** Expressions in the nested language. *)
type expr =
  | Record_expr of uid * expr Ident_map.t
        (* [@printer
          fun formatter map ->
            Format.fprintf formatter "Swan_ast.Record_expr(\"%a\")"
              (pp_map pp_ident pp_expr Ident_map.enum) map
        ] *)
  | List_expr of uid * expr list
  | Sequencing_expr of uid * expr * expr
  | Cons_expr of uid * expr * expr
  | Variant_expr of uid * variant * expr list
  | Function_expr of uid * function_value
  | Int_expr of uid * int
  | Bool_expr of uid * bool
  | String_expr of uid * string
  | Ref_expr of uid * expr
  | Var_expr of uid * swan_var
  | Appl_expr of uid * expr * expr
  | Conditional_expr of uid * expr * pattern * function_value * function_value
  | If_expr of uid * expr * expr * expr
  | Deref_expr of uid * expr
  | Update_expr of uid * expr * expr
  | Binary_operation_expr of uid * expr * binary_operator * expr
  | Unary_operation_expr of uid * unary_operator * expr
  | Indexing_expr of uid * expr * expr
  | Let_pattern_expr of uid * pattern * expr * expr
  | Let_function_expr of uid * swan_var * pattern list * expr * expr
  | Projection_expr of uid * expr * ident
  | Match_expr of uid * expr * match_pair list
  | Invariant_failure_expr of uid * expr
  [@@deriving eq, ord]

(** Function values in the nested language. *)
and function_value =
  | Function_with_multiple_arguments of uid * pattern list * expr
  [@@deriving eq, ord]


(** Patterns in the nested language. *)
and pattern =
  | Record_pattern of uid * pattern Ident_map.t
        (* [@printer
          fun formatter map ->
            Format.fprintf formatter "Swan_ast.Record_pattern(\"%a\")"
              (pp_map pp_ident pp_pattern Ident_map.enum) map
        ] *)
  | List_pattern of uid * pattern list
  | Cons_pattern of uid * pattern * pattern
  | Variant_pattern of uid * variant * pattern list
  | Fun_pattern of uid
  | Ref_pattern of uid
  | Int_pattern of uid
  | Bool_pattern of uid * bool
  | String_pattern of uid
  | Any_pattern of uid
  | Var_pattern of uid * swan_var
  [@@deriving eq, ord]

and match_pair =
  | Match_pair of uid * pattern * expr
  [@@deriving eq, ord]

and swan_var =
  | Swan_var of uid * ident
  [@@deriving eq, ord]

;;
