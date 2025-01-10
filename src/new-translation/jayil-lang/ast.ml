
open Batteries
open Jhupllib

module Ident = struct
  open Core
  type t = Ident of string
  [@@unboxed][@@deriving eq, ord, show, sexp, compare, hash, to_yojson]
end

module Ident_set = struct
  module S = Set.Make (Ident)
  include S
  include Pp_utils.Set_pp (S) (Ident)
  include Yojson_utils.Set_to_yojson (S) (Ident)
end

module Ident_map = struct
  module M = Map.Make (Ident)
  include M
  include Pp_utils.Map_pp (M) (Ident)
  include Yojson_utils.Map_to_yojson (M) (Ident)

  let key_list map = keys map |> List.of_enum
end

module Var = struct
  type t = Var of Ident.t
  [@@unboxed][@@deriving eq, ord, show, sexp, compare, hash, to_yojson]
end

module Var_map = Map.Make (Var)

module Binary_operator = struct
  type t = 
    | Binary_operator_plus
    | Binary_operator_minus
    | Binary_operator_times
    | Binary_operator_divide
    | Binary_operator_modulus
    | Binary_operator_less_than
    | Binary_operator_less_than_or_equal_to
    | Binary_operator_equal_to
    | Binary_operator_not_equal_to
    | Binary_operator_and
    | Binary_operator_or
    [@@deriving eq, ord]

  let to_yojson binop =
    let s = 
      match binop with
      | Binary_operator_plus                  -> "+"
      | Binary_operator_minus                 -> "-"
      | Binary_operator_times                 -> "*"
      | Binary_operator_divide                -> "/"
      | Binary_operator_modulus               -> "%"
      | Binary_operator_less_than             -> "<"
      | Binary_operator_less_than_or_equal_to -> "<="
      | Binary_operator_equal_to              -> "=="
      | Binary_operator_not_equal_to          -> "<>"
      | Binary_operator_and                   -> "and"
      | Binary_operator_or                    -> "or"
    in
    `String s
end

(** A type to express record values. *)
type record_value = Record_value of value Ident_map.t
[@@unboxed][@@deriving eq, ord, to_yojson]

(** A type to express function values. *)
and function_value = Function_value of { param : Ident.t ; body : expr }
[@@deriving eq, ord, to_yojson]

(** A type to represent values. *)
and value =
  | Value_record of record_value
  | Value_function of function_value
  | Value_int of int
  | Value_bool of bool
[@@deriving eq, ord, to_yojson]

(** A type to represent the bodies of clauses. *)
and clause_body =
  | Value_body of value
  | Var_body of Var.t
  | Input_body
  | Appl_body of { f_var : Var.t ; arg_var : Var.t }
  | Conditional_body of { cond_var : Var.t ; true_body : expr ; false_body : expr }
  | Match_body of Ident.t * pattern
  | Projection_body of { record_var : Var.t ; label_id :  Ident.t }
  | Not_body of Ident.t
  | Binary_operation_body of { left_var : Var.t ; binop : Binary_operator.t ; right_var : Ident.t }
  | Abort_body
  | Diverge_body
[@@deriving eq, ord, to_yojson]

(** A type to represent clauses. *)
and clause = Clause of (Ident.t * clause_body)
[@@unboxed][@@deriving eq, ord, to_yojson]

(** A type to represent expressions. *)
and expr = Expr of clause list
[@@unboxed][@@deriving eq, ord, to_yojson]

(** A type representing conditional patterns. *)
and pattern =
  | Fun_pattern
  | Int_pattern
  | Bool_pattern
  | Rec_pattern of Ident_set.t
  | Strict_rec_pattern of Ident_set.t
  | Any_pattern
[@@deriving eq, ord, to_yojson]