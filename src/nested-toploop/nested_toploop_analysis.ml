
open Batteries;;
open A_translator;;
open Core_ast;;

let core_to_nested map core =
  match core with
  | Core_toploop_analysis_types.Application_of_non_function
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv1, fv2) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Application_of_non_function(uid1, uid2, fv1, fv2)
  | Core_toploop_analysis_types.Projection_of_non_record
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Projection_of_non_record(uid1, uid2, fv)
  | Core_toploop_analysis_types.Projection_of_absent_label
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv, i) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Projection_of_absent_label(uid1, uid2, fv, i)
  | Core_toploop_analysis_types.Deref_of_non_ref
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Deref_of_non_ref(uid1, uid2, fv)
  | Core_toploop_analysis_types.Update_of_non_ref
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Update_of_non_ref(uid1, uid2, fv)
  | Core_toploop_analysis_types.Invalid_binary_operation
      (Core_ast.Var(v1,_), op, Core_ast.Var(v2,_), fv1, Core_ast.Var(v3,_), fv2) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    let Proof_rule(_, uid3) = Ident_map.find v3 map in
    Nested_toploop_analysis_types.Invalid_binary_operation(uid1, op, uid2, fv1, uid3, fv2)
  | Core_toploop_analysis_types.Invalid_unary_operation
      (Core_ast.Var(v1,_), op, Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Invalid_unary_operation(uid1, op, uid2, fv)
  | Core_toploop_analysis_types.Invalid_indexing_subject
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Invalid_indexing_subject(uid1, uid2, fv)
  | Core_toploop_analysis_types.Invalid_indexing_argument
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Nested_toploop_analysis_types.Invalid_indexing_argument(uid1, uid2, fv)
;;

let batch_translation map cores =
  Enum.map (core_to_nested map) cores
;;
