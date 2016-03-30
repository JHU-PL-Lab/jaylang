(**
  A module defining data structures and basic operations to form a DDPA graph.
*)

open Batteries;;

open Ast;;
open Ast_pp;;

module Pattern_ord =
struct
  type t = pattern
  let compare = compare_pattern
end;;

module Pattern_set = Set.Make(Pattern_ord);;

let pp_pattern_set pats =
  String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pp_pattern @@
    Pattern_set.enum pats
;;

(** A type to express abstract values. *)
type abstract_value =
  | Abs_value_record of record_value
  | Abs_value_function of abstract_function_value
  | Abs_value_ref of ref_value
  | Abs_value_int
  [@@deriving eq, ord]

and abstract_function_value =
  | Abs_function_value of var * abstract_expr
  [@@deriving eq, ord]

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
  [@@deriving eq, ord]

(** A type to represent abstract clauses. *)
and abstract_clause =
  | Abs_clause of var * abstract_clause_body
  [@@deriving eq, ord]

(** A type to represent abstract expressions. *)
and abstract_expr = Abs_expr of abstract_clause list [@@deriving eq, ord]
;;

let rec pp_abstract_function_value (Abs_function_value(x,e)) =
  Printf.sprintf "%s -> ( %s )" (pp_var x) (pp_abstract_expr e)

and pp_abstract_value v =
  match v with
  | Abs_value_record r -> pp_record_value r
  | Abs_value_function f -> pp_abstract_function_value f
  | Abs_value_ref r -> pp_ref_value r
  | Abs_value_int -> "int"

and pp_abstract_clause_body b =
  match b with
  | Abs_var_body(x) -> pp_var x
  | Abs_value_body(v) -> pp_abstract_value v
  | Abs_appl_body(x1,x2) -> pp_var x1 ^ " " ^ pp_var x2
  | Abs_conditional_body(x,p,f1,f2) ->
    pp_var x ^ " ~ " ^ pp_pattern p ^ " ? " ^
    pp_abstract_function_value f1 ^ " : " ^ pp_abstract_function_value f2
  | Abs_projection_body(x,i) -> pp_var x ^ "." ^ pp_ident i
  | Abs_deref_body(x) -> "!" ^ pp_var x
  | Abs_update_body(x1,x2) -> pp_var x1 ^ " <- " ^ pp_var x2
  | Abs_binary_operation_body(x1,op,x2) ->
    Printf.sprintf "%s %s %s" (pp_var x1) (pp_binary_operator op) (pp_var x2)

and pp_abstract_clause (Abs_clause(x,b)) =
  Printf.sprintf "%s = %s" (pp_var x) (pp_abstract_clause_body b)
  
and pp_abstract_expr (Abs_expr(cls)) =
  String_utils.concat_sep "; " @@ Enum.map pp_abstract_clause @@ List.enum cls
;;

let ppa_abstract_clause (Abs_clause(x,_)) = pp_var x;;

let is_abstract_clause_immediate (Abs_clause(_,b)) =
  match b with
  | Abs_var_body _ | Abs_value_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _ -> true
  | Abs_appl_body _ | Abs_conditional_body _ -> false
;;

module Abs_value_ord =
struct
  type t = abstract_value
  let compare = compare_abstract_value
end;;

module Abs_value_set = Set.Make(Abs_value_ord);;

let pp_abs_value_set s =
  String_utils.concat_sep_delim "{" "}" ", " @@
    Enum.map pp_abstract_value @@ Abs_value_set.enum s
;;

type abs_filtered_value =
  | Abs_filtered_value of abstract_value * Pattern_set.t * Pattern_set.t
  [@@deriving ord]
;;

module Abs_filtered_value_ord =
struct
  type t = abs_filtered_value
  let compare = compare_abs_filtered_value
end;;

module Abs_filtered_value_set = Set.Make(Abs_filtered_value_ord);;

let pp_abs_filtered_value (Abs_filtered_value(v,patsp,patsn)) =
  if Pattern_set.is_empty patsp && Pattern_set.is_empty patsn
  then pp_abstract_value v
  else
    Printf.sprintf "%s:(+%s,-%s)"
      (pp_abstract_value v) (pp_pattern_set patsp) (pp_pattern_set patsn)
;;

let pp_abs_filtered_value_set s =
  String_utils.concat_sep_delim "{" "}" ", " @@
    Enum.map pp_abs_filtered_value @@ Abs_filtered_value_set.enum s
;;

module Abs_clause_ord =
struct
  type t = abstract_clause
  let compare = compare_abstract_clause
end;;

module Abs_clause_set = Set.Make(Abs_clause_ord);;

type annotated_clause =
  | Unannotated_clause of abstract_clause
  | Enter_clause of var * var * abstract_clause
  | Exit_clause of var * var * abstract_clause
  | Start_clause
  | End_clause
  [@@deriving ord, eq]
;;

let pp_annotated_clause acl =
  match acl with
  | Unannotated_clause(cl) -> pp_abstract_clause cl
  | Enter_clause(x,x',cl) -> Printf.sprintf "%s=%s@%s+"
      (pp_var x) (pp_var x') (pp_abstract_clause cl)
  | Exit_clause(x,x',cl) -> Printf.sprintf "%s=%s@%s+"
      (pp_var x) (pp_var x') (pp_abstract_clause cl)
  | Start_clause -> "Start"
  | End_clause -> "End"
;;

let ppa_annotated_clause acl =
  match acl with
  | Unannotated_clause(cl) -> ppa_abstract_clause cl
  | Enter_clause(x,x',cl) -> Printf.sprintf "%s=%s@%s+"
      (pp_var x) (pp_var x') (ppa_abstract_clause cl)
  | Exit_clause(x,x',cl) -> Printf.sprintf "%s=%s@%s+"
      (pp_var x) (pp_var x') (ppa_abstract_clause cl)
  | Start_clause -> "Start"
  | End_clause -> "End"
;;

let is_annotated_clause_immediate acl =
  match acl with
  | Unannotated_clause(cl) -> is_abstract_clause_immediate cl
  | Enter_clause _ | Exit_clause _ | Start_clause | End_clause -> true
;;

module Annotated_clause_ord =
struct
  type t = annotated_clause
  let compare = compare_annotated_clause
end;;

module Annotated_clause_set = Set.Make(Annotated_clause_ord);;

let pp_annotated_clause_set s =
  String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pp_annotated_clause @@
    Annotated_clause_set.enum s
;;

type ddpa_edge =
  | Ddpa_edge of annotated_clause * annotated_clause
  [@@deriving ord]
;;

let pp_ddpa_edge (Ddpa_edge(acl1,acl0)) =
  Printf.sprintf "%s ==> %s"
    (pp_annotated_clause acl1) (pp_annotated_clause acl0)
;;

module Ddpa_edge_ord =
struct
  type t = ddpa_edge
  let compare = compare_ddpa_edge
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type ddpa_graph
  
  val empty : ddpa_graph
  
  val add_edge : ddpa_edge -> ddpa_graph -> ddpa_graph
  
  val edges_of : ddpa_graph -> ddpa_edge Enum.t
  
  val has_edge : ddpa_edge -> ddpa_graph -> bool
  
  val edges_from : annotated_clause -> ddpa_graph -> ddpa_edge Enum.t

  val edges_to : annotated_clause -> ddpa_graph -> ddpa_edge Enum.t
  
  val preds : annotated_clause -> ddpa_graph -> annotated_clause Enum.t
  
  val succs : annotated_clause -> ddpa_graph -> annotated_clause Enum.t
end;;

(* TODO: improve the performance of this implementation! *)
module Graph_impl : Graph_sig =
struct
  module Ddpa_edge_set = Set.Make(Ddpa_edge_ord);;
  
  type ddpa_graph = Graph of Ddpa_edge_set.t;;

  let empty = Graph(Ddpa_edge_set.empty);;

  let add_edge edge (Graph(s)) = Graph(Ddpa_edge_set.add edge s);;

  let edges_of (Graph(s)) = Ddpa_edge_set.enum s;;

  let has_edge edge (Graph(s)) = Ddpa_edge_set.mem edge s;;

  let edges_from acl (Graph(s)) =
    Ddpa_edge_set.enum s
    |> Enum.filter (fun (Ddpa_edge(acl',_)) -> equal_annotated_clause acl acl')
  ;;

  let succs acl g =
    edges_from acl g |> Enum.map (fun (Ddpa_edge(_,acl)) -> acl)
  ;;

  let edges_to acl (Graph(s)) =
    Ddpa_edge_set.enum s
    |> Enum.filter (fun (Ddpa_edge(_,acl')) -> equal_annotated_clause acl acl')
  ;;

  let preds acl g =
    edges_to acl g |> Enum.map (fun (Ddpa_edge(acl,_)) -> acl)
  ;;
end;;

include Graph_impl;;

let pp_ddpa_graph g =
  String_utils.concat_sep_delim "{" "}" ", " @@
    Enum.map pp_ddpa_edge @@ edges_of g
;;

let rec lift_expr (Expr(cls)) =
  Abs_expr(List.map lift_clause cls)

and lift_clause (Clause(x,b)) =
  Abs_clause(x, lift_clause_body b)

and lift_clause_body b =
  match b with
  | Value_body v -> Abs_value_body(lift_value v)
  | Var_body x -> Abs_var_body x
  | Appl_body(x,x') -> Abs_appl_body(x,x')
  | Conditional_body(x,p,f1,f2) ->
    Abs_conditional_body(x,p,lift_function_value f1,lift_function_value f2)
  | Projection_body(x,i) -> Abs_projection_body(x,i)
  | Deref_body(x) -> Abs_deref_body(x)
  | Update_body(x,x') -> Abs_update_body(x,x')
  | Binary_operation_body(x1,op,x2) -> Abs_binary_operation_body(x1,op,x2)
  | Unary_operation_body(_,_) -> failwith "Not implemented"

and lift_value v =
  match v with
  | Value_record r -> Abs_value_record r
  | Value_function f -> Abs_value_function(lift_function_value f)
  | Value_ref r -> Abs_value_ref r
  | Value_int _ -> Abs_value_int
  | Value_bool _ -> failwith "Not implemented"

and lift_function_value (Function_value(x,e)) =
  Abs_function_value(x, lift_expr e)

;;
