(**
   Contains data type definitions for the toy language AST.
*)

open Batteries;;

(** A data type for identifiers in the toy language. *)
type ident = Ident of string [@@deriving eq, ord, show, to_yojson];;

module Ident_hash =
struct
  type t = ident
  let equal = equal_ident
  let hash = Hashtbl.hash
end
;;

module Ident_hashtbl = Hashtbl.Make(Ident_hash);;

module Ident_order =
struct
  type t = ident
  let compare = compare_ident
end
;;

module Ident_set = struct
  include Set.Make(Ident_order);;
  let pp = Pp_utils.pp_set pp_ident enum;;
  let to_yojson = Yojson_utils.set_to_yojson ident_to_yojson enum;;
end;;

module Ident_map = struct
  include Map.Make(Ident_order);;
  let pp pp_value = Pp_utils.pp_map pp_ident pp_value enum;;
  let to_yojson value_to_yojson =
    Yojson_utils.map_to_yojson ident_to_yojson value_to_yojson enum
  ;;
end;;

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack =
  | Freshening_stack of ident list
  [@@deriving eq, ord, to_yojson]
;;

(** Variables in the AST. *)
type var =
  | Var of ident * freshening_stack option
  [@@deriving eq, ord, to_yojson]
;;

module Var_order =
struct
  type t = var
  let compare = compare_var
end;;

module Var_set = Set.Make(Var_order);;

module Var_map = Map.Make(Var_order);;

let var_map_union m1 m2 =
  Var_map.fold Var_map.add m2 m1
;;

module Var_hashtbl = Hashtbl.Make(
  struct
    type t = var
    let equal = equal_var
    let hash = Hashtbl.hash
  end
  );;

type binary_operator =
  | Binary_operator_plus
  | Binary_operator_int_minus
  | Binary_operator_int_less_than
  | Binary_operator_int_less_than_or_equal_to
  | Binary_operator_equal_to
  | Binary_operator_bool_and
  | Binary_operator_bool_or
  [@@deriving eq, ord]
;;

let binary_operator_to_yojson = function
  | Binary_operator_plus -> `String "+"
  | Binary_operator_int_minus -> `String "-"
  | Binary_operator_int_less_than -> `String "<"
  | Binary_operator_int_less_than_or_equal_to -> `String "<="
  | Binary_operator_equal_to -> `String "=="
  | Binary_operator_bool_and -> `String "and"
  | Binary_operator_bool_or -> `String "or"
;;

type unary_operator =
  | Unary_operator_bool_not
  [@@deriving eq, ord]
;;

let unary_operator_to_yojson = function
  | Unary_operator_bool_not ->  `String "not"
;;

(** A type to express record values. *)
type record_value =
  | Record_value of var Ident_map.t
  [@@deriving eq, ord, to_yojson]

(** A type to express function values. *)
and function_value =
  | Function_value of var * expr
  [@@deriving eq, ord, to_yojson]

(** A type to express reference values. *)
and ref_value =
  | Ref_value of var
  [@@deriving eq, ord, to_yojson]

(** A type to represent values. *)
and value =
  | Value_record of record_value
  | Value_function of function_value
  | Value_ref of ref_value
  | Value_int of int
  | Value_bool of bool
  | Value_string of string
  [@@deriving eq, ord, to_yojson]

(** A type to represent the bodies of clauses. *)
and clause_body =
  | Value_body of value
  | Var_body of var
  | Appl_body of var * var
  | Conditional_body of var * pattern * function_value * function_value
  | Projection_body of var * ident
  | Deref_body of var
  | Update_body of var * var
  | Binary_operation_body of var * binary_operator * var
  | Unary_operation_body of unary_operator * var
  | Indexing_body of var * var
  [@@deriving eq, ord, to_yojson]

(** A type to represent clauses. *)
and clause =
  | Clause of var * clause_body
  [@@deriving eq, ord, to_yojson]

(** A type to represent expressions. *)
and expr = Expr of clause list [@@deriving eq, ord, to_yojson]

(** A type representing conditional patterns. *)
and pattern =
  | Record_pattern of pattern Ident_map.t
  | Fun_pattern
  | Ref_pattern
  | Int_pattern
  | Bool_pattern of bool
  | String_pattern
  | Any_pattern
  [@@deriving eq, ord, yojson]
;;

module Value_order =
struct
  type t = value
  let compare = compare_value
end;;
