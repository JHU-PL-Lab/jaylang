open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Toploop_analysis_types;;
open Toploop_ddpa_wrapper_types;;
open Toploop_utils;;
open Ddpa_abstract_ast;;
open Ddpa_graph;;
open Ddpa_utils;;

module Make(DDPA_wrapper : DDPA_wrapper) =
struct
  module DDPA_wrapper = DDPA_wrapper;;

  let find_errors analysis =
    let open Nondeterminism.Nondeterminism_monad in
    enum @@
    let%bind acl =
      analysis
      |> DDPA_wrapper.expression_of
      |> lift_expr
      |> iterate_abstract_clauses
      |> pick_enum
    in
    let Abs_clause(x_clause,b) = acl in
    let lookup x =
      analysis
      |> DDPA_wrapper.values_of_variable_from x (Unannotated_clause(acl))
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
            |> Ddpa_abstract_ast.Abs_filtered_value_set.of_enum
          in
          return @@ Application_of_non_function(x_clause,xf,filtv,filtvs)
      end
    | Abs_projection_body(x,i) ->
      let%bind (v,filtv) = lookup x in
      begin
        match v with
        | Abs_value_record(Abs_record_value(m)) ->
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
      let is_valid = Option.is_some (abstract_binary_operation op v1 v2) in
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
end;;
