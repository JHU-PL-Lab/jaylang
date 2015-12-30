(**
   Contains data type definitions for the toy language AST.
*)

open Batteries;;

(** A module for hashtables keyed by UIDs. *)
module Ast_uid_hashtbl = Ast_uid.Ast_uid_hashtbl;;

(** A data type for identifiers in the toy language. *)
type ident = Ident of string [@@deriving eq, ord];;

let pp_ident (Ident i) = i;;

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

module Ident_set = Set.Make(Ident_order);;

module Ident_map = Map.Make(Ident_order);;

let pp_ident_map pp_value map =
  Ident_map.enum map
  |> Enum.map (fun (k,v) ->
                Printf.sprintf "%s => %s" (pp_ident k) (pp_value v))
  |> String_utils.concat_sep_delim "{\n    " "\n}" ",\n    "
;;

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack = Freshening_stack of ident list [@@deriving eq, ord];;

(** Variables in the AST. *)
type var = Var of ident * freshening_stack option [@@deriving eq, ord];;

module Var_order =
struct
  type t = var
  let compare = compare_var
end;;

module Var_set = Set.Make(Var_order);;

module Var_map = Map.Make(Var_order);;

module Var_hashtbl = Hashtbl.Make(
  struct
    type t = var
    let equal = equal_var
    let hash = Hashtbl.hash
  end
  );;

(** A type to express record values. *)
type record_value = Record_value of var Ident_map.t [@@deriving eq, ord]

(** A type to express function values. *)
and function_value = Function_value of var * expr [@@deriving eq, ord]

(** A type to express reference values. *)
and ref_value = Ref_value of var [@@deriving eq, ord]

(** A type to represent values. *)
and value =
  | Value_record of record_value
  | Value_function of function_value
  | Value_ref of ref_value
  [@@deriving eq, ord]

(** A type to represent the bodies of clauses. *)
and clause_body =
  | Value_body of value
  | Var_body of var
  | Appl_body of var * var
  | Conditional_body of var * pattern * function_value * function_value
  | Projection_body of var * ident
  | Deref_body of var
  | Update_body of var * var
  [@@deriving eq, ord]

(** A type to represent clauses. *)
and clause =
  | Clause of var * clause_body
  [@@deriving eq, ord]

(** A type to represent expressions. *)
and expr = Expr of clause list [@@deriving eq, ord]

(** A type representing conditional patterns. *)
and pattern =
  | Record_pattern of pattern Ident_map.t
  [@@deriving eq, ord]
;;

module Value_order =
struct
  type t = value
  let compare = compare_value
end;;
