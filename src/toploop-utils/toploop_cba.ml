open Batteries;;

open Analysis;;
open Ast;;
open Ast_pretty;;
open Cba_graph;;

include Toploop_cba_types;;

(** Finds all of the call sites in the provided expression.  Returns an
    enumeration of call sites as pairs between the function variable and the
    clause representing the call site. *)
let rec find_all_call_sites (Expr cls) =
  let rec call_sites_from_function_value (Function_value(_, e)) =
    find_all_call_sites e
  in
  let rec call_sites_from_clause cl =
    match cl with
    | Clause(_, Appl_body(x2, _)) ->
      Some (Enum.singleton (x2, cl))
    | Clause(_, Value_body(Value_function(f))) ->
      Some (call_sites_from_function_value f)
    | Clause(_, Conditional_body(_, _, f1, f2)) ->
      Some (Enum.append (call_sites_from_function_value f1)
                        (call_sites_from_function_value f2))
    | _ -> None
  in
  cls
  |> List.enum
  |> Enum.filter_map
    call_sites_from_clause
  |> Enum.concat
;;

let pp_inconsistency inconsistency =
  match inconsistency with
  | Application_of_non_function(x,c,v) ->
    Printf.sprintf
      "Error: call site %s has function variable %s to which a non-function value may flow: %s"
      (pretty_clause c) (pretty_var x) (pp_abstract_value v)
;;

module type Stack = Analysis_context_stack.Context_stack;;
let stack_from_name name =
  begin
    match name with
    | "cba0" ->
      Some (module Analysis_unit_stack.Stack : Stack)
    | "cba1" ->
      Some (module Analysis_single_element_stack.Stack : Stack)
    | "cba2" ->
      Some (module Analysis_two_element_stack.Stack : Stack)
    | "cbanr" ->
      Some (module Analysis_nonrepeating_stack.Stack : Stack)
    | "none" -> None
    | _ -> raise Not_found
  end
;;

module Make(A : Analysis_sig) = struct
  type analysis =
    { aref : A.cba_analysis ref
    ; expression : expr
    };;
  
  let create_analysis ?logging_prefix:(pfx=None) expr =
    let a = A.create_initial_analysis ~logging_prefix:pfx expr in
    { aref = ref @@ A.perform_full_closure a
    ; expression = expr
    }
  ;;

  let values_of_variable_from x acl analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.values_of_variable acl x a in
    analysis.aref := a';
    values
  ;;

  let check_inconsistencies analysis =
    find_all_call_sites analysis.expression
    |> Enum.map
      (fun (x2, cl) ->
        let acl = Unannotated_clause(lift_clause cl) in
        let (values, a') = A.values_of_variable acl x2 !(analysis.aref) in
        analysis.aref := a';
        values
        |> Abs_value_set.enum
        |> Enum.filter_map
          (fun v ->
            match v with
            | Abs_value_function _ -> None
            | _ -> Some (Application_of_non_function(x2, cl, v))
          )
      )
    |> Enum.concat
    |> List.of_enum (* force us to pull on the enum so the analysis
                       updates *)
    |> List.enum
  ;;

  let pp_analysis analysis =
    A.pp_cba !(analysis.aref)
  ;;
end;;
