open Batteries;;

open Core_ast;;
open Ddpa_analysis;;
open Ddpa_graph;;
open Ddpa_utils;;

include Toploop_ddpa_types;;

(* TODO: the "none" should be handled elsewhere as a special case *)
module type Stack = Ddpa_context_stack.Context_stack;;
let name_parsing_functions =
  [
    (* A function for the literally-named modules. *)
    (fun name ->
       match name with
       | "0ddpa" ->
         Some (module Ddpa_unit_stack.Stack : Stack)
       | "1ddpa" ->
         Some (module Ddpa_single_element_stack.Stack : Stack)
       | "2ddpa" ->
         Some (module Ddpa_two_element_stack.Stack : Stack)
       | "ddpaNR" ->
         Some (module Ddpa_nonrepeating_stack.Stack : Stack)
       | "none" -> None
       | _ -> raise Not_found
    )
    ;
    (* A function for parsing kddpa *)
    (fun name ->
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         let module Spec : Ddpa_n_element_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Ddpa_n_element_stack.Make(Spec) in
         Some (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    )
  ];;
let stack_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;;

module Make(A : Analysis_sig) : DDPA with module C = A.C =
struct
  type analysis =
    { aref : A.ddpa_analysis ref
    ; expression : expr
    };;

  module C = A.C;;

  let create_analysis expr =
    let a = A.create_initial_analysis expr in
    { aref = ref @@ A.perform_full_closure a
    ; expression = expr
    }
  ;;

  let values_of_variable_from x acl analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.values_of_variable x acl a in
    analysis.aref := a';
    values
  ;;

  let contextual_values_of_variable_from x acl ctx analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.contextual_values_of_variable x acl ctx a in
    analysis.aref := a';
    values
  ;;

  let check_inconsistencies analysis =
    let open Nondeterminism.Nondeterminism_monad in
    enum @@
    let%bind acl =
      analysis.expression
      |> lift_expr
      |> iterate_abstract_clauses
      |> pick_enum
    in
    let Abs_clause(x_clause,b) = acl in
    let lookup x =
      analysis
      |> values_of_variable_from x (Unannotated_clause(acl))
      |> Abs_filtered_value_set.enum
      |> Enum.map
        (fun ((Abs_filtered_value(v,_,_)) as filtv) -> v,filtv)
      |> pick_enum
    in
    match b with
    | Abs_value_body _
    | Abs_var_body _
    | Abs_conditional_body _ ->
      (* There's nothing this body that can be inconsistent. *)
      zero ()
    | Abs_appl_body(xf,xa) ->
      let%bind (v,filtv) = lookup xf in
      begin
        match v with
        | Abs_value_function _ -> zero ()
        | _ ->
          let filtvs =
            lookup xa
            |> Nondeterminism.Nondeterminism_monad.enum
            |> Enum.map snd
            |> Ddpa_graph.Abs_filtered_value_set.of_enum
          in
          return @@ Application_of_non_function(x_clause,xf,filtv,filtvs)
      end
    | Abs_projection_body(x,i) ->
      let%bind (v,filtv) = lookup x in
      begin
        match v with
        | Abs_value_record(Record_value(m)) ->
          if Ident_map.mem i m
          then zero ()
          else return @@ Projection_of_absent_label(x_clause,x,filtv,i)
        | _ -> return @@ Projection_of_non_record(x_clause,x,filtv)
      end
    | Abs_deref_body(x)->
      let%bind (v,filtv) = lookup x in
      begin
        match v with
        | Abs_value_ref _ -> zero ()
        | _ -> return @@ Deref_of_non_ref(x_clause,x,filtv)
      end
    | Abs_update_body(x,_) ->
      let%bind (v,filtv) = lookup x in
      begin
        match v with
        | Abs_value_ref _ -> zero ()
        | _ -> return @@ Update_of_non_ref(x_clause,x,filtv)
      end
    | Abs_binary_operation_body(x1,op,x2) ->
      let%bind (v1,filtv1) = lookup x1 in
      let%bind (v2,filtv2) = lookup x2 in
      let is_valid =
        match (v1,v2) with
        | (Abs_value_int, Abs_value_int) ->
          begin
            match op with
            | Binary_operator_plus
            | Binary_operator_int_minus
            | Binary_operator_int_less_than
            | Binary_operator_int_less_than_or_equal_to
            | Binary_operator_equal_to -> true
            | Binary_operator_bool_or | Binary_operator_bool_and -> false
          end
        | (Abs_value_bool _, Abs_value_bool _) ->
          begin
            match op with
            | Binary_operator_equal_to
            | Binary_operator_bool_or | Binary_operator_bool_and -> true
            | Binary_operator_plus
            | Binary_operator_int_minus
            | Binary_operator_int_less_than
            | Binary_operator_int_less_than_or_equal_to -> false
          end
        | (Abs_value_string, Abs_value_string) ->
          begin
            match op with
            | Binary_operator_equal_to
            | Binary_operator_plus -> true
            | Binary_operator_int_minus
            | Binary_operator_int_less_than
            | Binary_operator_int_less_than_or_equal_to
            | Binary_operator_bool_or | Binary_operator_bool_and -> false
          end
        | _ -> false
      in
      if is_valid
      then zero ()
      else return @@
        Invalid_binary_operation(x_clause, op, x1, filtv1, x2, filtv2)
    | Abs_unary_operation_body(op,x) ->
      let%bind (v,filtv) = lookup x in
      begin
        match op,v with
        | (Unary_operator_bool_not, Abs_value_bool _) -> zero ()
        | _ -> return @@ Invalid_unary_operation(x_clause, op, x, filtv)
      end
    | Abs_indexing_body(xa,xi) ->
      alternative
        begin
          let%bind (v,filtv) = lookup xa in
          match v with
          | Abs_value_string -> zero ()
          | _ -> return @@ Invalid_indexing_subject(x_clause,xa,filtv)
        end
        begin
          let%bind (v,filtv) = lookup xi in
          match v with
          | Abs_value_int -> zero ()
          | _ -> return @@ Invalid_indexing_argument(x_clause,xi,filtv)
        end
  ;;

  let pp_analysis formatter analysis =
    A.pp_ddpa_analysis formatter !(analysis.aref)
  ;;
  let show_analysis analysis =
    A.show_ddpa_analysis !(analysis.aref)
  ;;

  let get_size analysis =
    A.get_size !(analysis.aref)
  ;;
end;;
