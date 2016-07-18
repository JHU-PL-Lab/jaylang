open Batteries;;
open Uid;;

let nested_to_egg map nested =
  match nested with
  | Nested_toploop_analysis_types.Projection_of_non_record
      (nuid1, nuid2, fv) ->
    Egg_toploop_analysis_types.Projection_of_non_record(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Projection_of_absent_label
      (nuid1, nuid2, fv, i) ->
    Egg_toploop_analysis_types.Projection_of_absent_label(nuid1, nuid2, fv, i)
  | Nested_toploop_analysis_types.Deref_of_non_ref
      (nuid1, nuid2, fv) ->
    Egg_toploop_analysis_types.Deref_of_non_ref(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Update_of_non_ref
      (nuid1, nuid2, fv) ->
    Egg_toploop_analysis_types.Update_of_non_ref(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_binary_operation
      (nuid1, op, nuid2, fv1, nuid3, fv2) ->
    Egg_toploop_analysis_types.Invalid_binary_operation(nuid1, op, nuid2, fv1, nuid3, fv2)
  | Nested_toploop_analysis_types.Invalid_unary_operation
      (nuid1, op, nuid2, fv) ->
    Egg_toploop_analysis_types.Invalid_unary_operation(nuid1, op, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_indexing_subject
      (nuid1, nuid2, fv) ->
    Egg_toploop_analysis_types.Invalid_indexing_subject(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_indexing_argument
      (nuid1, nuid2, fv) ->
    Egg_toploop_analysis_types.Invalid_indexing_argument(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Application_of_non_function
      (nuid1, nuid2, fv1, fv2) ->
    match (Uid_map.Exceptionless.find nuid1 map) with
    | Some Egg_translator.Bad_if_branch_to_function(_, if_uid) ->
      Egg_toploop_analysis_types.If_depends_on_non_bool(if_uid, fv2)
    | Some Egg_translator.Inexhaustive_match_branch(_, m_uid) ->
      Egg_toploop_analysis_types.Inexhaustive_match(m_uid, fv2)
    | None ->
      Egg_toploop_analysis_types.Application_of_non_function(nuid1, nuid2, fv1, fv2)
    | _ -> failwith "Unexpected log entry mapped to NFA"
;;

let batch_translation map nested =
  Enum.map (nested_to_egg map) nested
;;
