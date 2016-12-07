open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Pp_utils;;

module Pattern_ord =
struct
  type t = pattern
  let compare = compare_pattern
end;;

module Pattern_set =
struct
  include Set.Make(Pattern_ord);;
  let pp = Pp_utils.pp_set pp_pattern enum;;
  let show = Pp_utils.pp_to_string pp;;
  let to_yojson = Yojson_utils.set_to_yojson pattern_to_yojson enum;;
end;;

(** A type to express abstract values. *)
type abstract_value =
  | Abs_value_record of record_value
  | Abs_value_function of abstract_function_value
  | Abs_value_ref of ref_value
  | Abs_value_int
  | Abs_value_bool of bool
  | Abs_value_string
[@@deriving eq, ord, to_yojson]

and abstract_function_value =
    | Abs_function_value of var * abstract_expr
[@@deriving eq, ord, to_yojson]

(** A type to represent the bodies of abstract clauses. *)
and abstract_clause_body =
    | Abs_value_body of abstract_value
  | Abs_var_body of var
  | Abs_appl_body of var * var
  | Abs_conditional_body of
      var * pattern * abstract_function_value * abstract_function_value
  | Abs_projection_body of var * ident
  | Abs_deref_body of var
  | Abs_update_body of var * var
  | Abs_binary_operation_body of var * binary_operator * var
  | Abs_unary_operation_body of unary_operator * var
  | Abs_indexing_body of var * var
[@@deriving eq, ord, to_yojson]

(** A type to represent abstract clauses. *)
and abstract_clause =
    | Abs_clause of var * abstract_clause_body
[@@deriving eq, ord, to_yojson]

(** A type to represent abstract expressions. *)
and abstract_expr =
    | Abs_expr of abstract_clause list
[@@deriving eq, ord, to_yojson]
;;

let rec pp_abstract_function_value formatter (Abs_function_value(x,e)) =
  Format.fprintf formatter "%a -> (@ %a)" pp_var x pp_abstract_expr e

and pp_abstract_value formatter v =
  match v with
  | Abs_value_record r -> pp_record_value formatter r
  | Abs_value_function f -> pp_abstract_function_value formatter f
  | Abs_value_ref r -> pp_ref_value formatter r
  | Abs_value_int -> Format.pp_print_string formatter "int"
  | Abs_value_bool b ->
    Format.pp_print_string formatter @@ if b then "true" else "false"
  | Abs_value_string -> Format.pp_print_string formatter "string"

and pp_abstract_clause_body formatter b =
  match b with
  | Abs_var_body(x) -> pp_var formatter x
  | Abs_value_body(v) -> pp_abstract_value formatter v
  | Abs_appl_body(x1,x2) -> Format.fprintf formatter "%a %a" pp_var x1 pp_var x2
  | Abs_conditional_body(x,p,f1,f2) ->
    Format.fprintf formatter
      "%a ~ %a@[<4> ? @[<2>%a@] : @[<2>%a@]@]"
      pp_var x
      pp_pattern p
      pp_abstract_function_value f1
      pp_abstract_function_value f2
  | Abs_projection_body(x,i) ->
    Format.fprintf formatter "%a.%a" pp_var x pp_ident i
  | Abs_deref_body(x) -> Format.fprintf formatter "!%a" pp_var x
  | Abs_update_body(x1,x2) ->
    Format.fprintf formatter "%a <- %a" pp_var x1 pp_var x2
  | Abs_binary_operation_body(x1,op,x2) ->
    Format.fprintf formatter "%a %a %a"
      pp_var x1 pp_binary_operator op pp_var x2
  | Abs_unary_operation_body(op,x1) ->
    Format.fprintf formatter "%a %a"
      pp_unary_operator op pp_var x1
  | Abs_indexing_body(x1,x2) ->
    Format.fprintf formatter "%a[%a]" pp_var x1 pp_var x2

and pp_abstract_clause formatter (Abs_clause(x,b)) =
  Format.fprintf formatter "%a = @[<hv 2>%a@]"
    pp_var x pp_abstract_clause_body b

and pp_abstract_expr formatter (Abs_expr(cls)) =
  pp_concat_sep ";" pp_abstract_clause formatter @@ List.enum cls
;;

let show_abstract_clause = pp_to_string pp_abstract_clause;;

let var_of_abstract_clause (Abs_clause(x,_)) = x;;
let pp_var_of_abstract_clause formatter acl =
  pp_var formatter (var_of_abstract_clause acl)
;;

let is_abstract_clause_immediate (Abs_clause(_,b)) =
  match b with
  | Abs_var_body _ | Abs_value_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _ | Abs_unary_operation_body _
  | Abs_indexing_body _ -> true
  | Abs_appl_body _ | Abs_conditional_body _ -> false
;;

module Abs_value =
struct
  type t = abstract_value
  let equal = equal_abstract_value
  let compare = compare_abstract_value
  let pp = pp_abstract_value
  let to_yojson = abstract_value_to_yojson
end;;

module Abs_value_set =
struct
  module Impl = Set.Make(Abs_value);;
  include Impl;;
  include Pp_utils.Set_pp(Impl)(Abs_value);;
  include Yojson_utils.Set_to_yojson(Impl)(Abs_value);;
end;;

let pp_abs_value_set formatter s =
  pp_concat_sep_delim "{" "}" ", " pp_abstract_value formatter @@
  Abs_value_set.enum s
;;

type abs_filtered_value =
  | Abs_filtered_value of abstract_value * Pattern_set.t * Pattern_set.t
[@@deriving eq, ord, show, to_yojson]
;;
let _ = show_abs_filtered_value;;

let pp_abs_filtered_value formatter (Abs_filtered_value(v,patsp,patsn)) =
  if Pattern_set.is_empty patsp && Pattern_set.is_empty patsn
  then pp_abstract_value formatter v
  else
    Format.fprintf formatter "%a:(+%a,-%a)"
      pp_abstract_value v Pattern_set.pp patsp Pattern_set.pp patsn
;;
let show_abs_filtered_value = pp_to_string pp_abs_filtered_value;;

module Abs_filtered_value =
struct
  type t = abs_filtered_value
  let compare = compare_abs_filtered_value
  let pp = pp_abs_filtered_value
  let to_yojson = abs_filtered_value_to_yojson
end;;

module Abs_filtered_value_set =
struct
  module Impl = Set.Make(Abs_filtered_value);;
  include Impl;;
  include Pp_utils.Set_pp(Impl)(Abs_filtered_value);;
  include Yojson_utils.Set_to_yojson(Impl)(Abs_filtered_value);;
end;;

module Abs_clause =
struct
  type t = abstract_clause
  let compare = compare_abstract_clause
  let pp = pp_abstract_clause
  let to_yojson = abstract_clause_to_yojson
end;;

module Abs_clause_set =
struct
  module Impl = Set.Make(Abs_clause);;
  include Impl;;
  include Pp_utils.Set_pp(Impl)(Abs_clause);;
  include Yojson_utils.Set_to_yojson(Impl)(Abs_clause);;
end;;

type annotated_clause =
  | Unannotated_clause of abstract_clause
  | Enter_clause of var * var * abstract_clause
  | Exit_clause of var * var * abstract_clause
  | Start_clause of var
  (** This variable is the return variable of the block that this clause
      starts. *)
  | End_clause of var
  (** This variable is the return variable of the block that this clause
      ends. *)
[@@deriving ord, eq, show, to_yojson]
;;

let is_annotated_clause_immediate acl =
  match acl with
  | Unannotated_clause(cl) -> is_abstract_clause_immediate cl
  | Enter_clause _ | Exit_clause _ | Start_clause _ | End_clause _ -> true
;;

module Annotated_clause =
struct
  type t = annotated_clause
  let compare = compare_annotated_clause
  let pp = pp_annotated_clause
  let to_yojson = annotated_clause_to_yojson
end;;

module Annotated_clause_set =
struct
  module Impl = Set.Make(Annotated_clause);;
  include Impl;;
  include Pp_utils.Set_pp(Impl)(Annotated_clause);;
  include Yojson_utils.Set_to_yojson(Impl)(Annotated_clause);;
end;;

module Annotated_clause_map =
struct
  module Impl = Map.Make(Annotated_clause);;
  include Impl;;
  include Pp_utils.Map_pp(Impl)(Annotated_clause);;
  include Yojson_utils.Map_to_yojson(Impl)(Annotated_clause);;
end;;

let pp_annotated_clause_set =
  Pp_utils.pp_set pp_annotated_clause Annotated_clause_set.enum
;;
