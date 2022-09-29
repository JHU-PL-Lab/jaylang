(** This module contains a number of routines to validate the well-formedness of
    an AST. *)

open Batteries
open Ast
open Ast_pp
open Ast_tools

type illformedness =
  | Duplicate_variable_binding of var
  | Variable_not_in_scope of var * var
[@@deriving eq, ord, show]

module Illformedness_order = struct
  type t = illformedness

  let compare = compare_illformedness
end

module Illformedness_set = Set.Make (Illformedness_order)

exception Illformedness_found of illformedness list

(** Determines if an expression is well-formed.

    Raises `Illformedness_found' with list of illformednesses found, if given
    expression is not well-formed. *)
let check_wellformed_expr expression : unit =
  (let expression_non_unique_bindings = non_unique_bindings expression in
   if not (Var_set.is_empty expression_non_unique_bindings)
   then
     let illformednesses =
       let () = print_endline "non-unique bindings" in
       expression_non_unique_bindings |> Var_set.enum
       |> Enum.map (fun non_unique_binding ->
              let () = print_endline @@ show_var non_unique_binding in
              Duplicate_variable_binding non_unique_binding)
       |> List.of_enum
     in
     raise @@ Illformedness_found illformednesses) ;
  let expression_scope_violations = scope_violations expression in
  if not (List.is_empty expression_scope_violations)
  then
    let illformednesses =
      let () = print_endline "scope violations: " in
      expression_scope_violations |> List.enum
      |> Enum.map (fun (program_point, dependency) ->
             let () =
               print_endline @@ show_var program_point ^ " = "
               ^ show_var dependency
             in
             Variable_not_in_scope (program_point, dependency))
      |> List.of_enum
    in
    raise @@ Illformedness_found illformednesses
