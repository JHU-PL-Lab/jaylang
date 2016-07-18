open Batteries;;

let egg_to_swan egg =
  match egg with
  | Egg_toploop_analysis_types.Projection_of_non_record
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Projection_of_non_record(nuid1, nuid2, fv)
  | Egg_toploop_analysis_types.Projection_of_absent_label
      (nuid1, nuid2, fv, i) ->
    Swan_toploop_analysis_types.Projection_of_absent_label(nuid1, nuid2, fv, i)
  | Egg_toploop_analysis_types.Deref_of_non_ref
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Deref_of_non_ref(nuid1, nuid2, fv)
  | Egg_toploop_analysis_types.Update_of_non_ref
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Update_of_non_ref(nuid1, nuid2, fv)
  | Egg_toploop_analysis_types.Invalid_binary_operation
      (nuid1, op, nuid2, fv1, nuid3, fv2) ->
    Swan_toploop_analysis_types.Invalid_binary_operation(nuid1, op, nuid2, fv1, nuid3, fv2)
  | Egg_toploop_analysis_types.Invalid_unary_operation
      (nuid1, op, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_unary_operation(nuid1, op, nuid2, fv)
  | Egg_toploop_analysis_types.Invalid_indexing_subject
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_indexing_subject(nuid1, nuid2, fv)
  | Egg_toploop_analysis_types.Invalid_indexing_argument
      (nuid1, nuid2, fv) ->
    Swan_toploop_analysis_types.Invalid_indexing_argument(nuid1, nuid2, fv)
  | Egg_toploop_analysis_types.Application_of_non_function
      (nuid1, nuid2, fv1, fv2) ->
    Swan_toploop_analysis_types.Application_of_non_function(nuid1, nuid2, fv1, fv2)
  | Egg_toploop_analysis_types.If_depends_on_non_bool(if_uid, fv2) ->
      Swan_toploop_analysis_types.If_depends_on_non_bool(if_uid, fv2)
  | Egg_toploop_analysis_types.Inexhaustive_match(m_uid, fv2) ->
      Swan_toploop_analysis_types.Inexhaustive_match(m_uid, fv2)
;;

let batch_translation egg =
  Enum.map (egg_to_swan) egg
;;
