open Batteries;;
open Uid;;

let nested_to_swan map swan =
  match swan with
  | Nested_toploop_analysis_types.Projection_of_non_record
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Projection_of_non_record(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Projection_of_absent_label
      (nuid1, nuid2, fv, i) ->
    Swan_toploop_analysis_types.Projection_of_absent_label(nuid1, nuid2, fv, i)
  | Nested_toploop_analysis_types.Deref_of_non_ref
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Deref_of_non_ref(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Update_of_non_ref
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Update_of_non_ref(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_binary_operation
      (nuid1, op, nuid2, fv1, nuid3, fv2) ->
    Swan_toploop_analysis_types.Invalid_binary_operation(nuid1, op, nuid2, fv1, nuid3, fv2)
  | Nested_toploop_analysis_types.Invalid_unary_operation
      (nuid1, op, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_unary_operation(nuid1, op, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_indexing_subject
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_indexing_subject(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Invalid_indexing_argument
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_indexing_argument(nuid1, nuid2, fv)
  | Nested_toploop_analysis_types.Application_of_non_function
      (nuid1, nuid2, fv1, fv2) ->
    match (Uid_map.Exceptionless.find nuid1 map) with
    | Some Egg_translator.Bad_if_branch_to_function(_, if_uid) ->
      Swan_toploop_analysis_types.If_depends_on_non_bool(if_uid, fv2)
    | Some Egg_translator.Inexhaustive_match_branch(_, m_uid) ->
      Swan_toploop_analysis_types.Inexhaustive_match(m_uid, fv2)
    | None ->
      Swan_toploop_analysis_types.Application_of_non_function(nuid1, nuid2, fv1, fv2)
    | _ -> failwith "Unexpected log entry mapped to NFA"
;;

let batch_translation map nested =
  Enum.map (nested_to_swan map) nested
;;
