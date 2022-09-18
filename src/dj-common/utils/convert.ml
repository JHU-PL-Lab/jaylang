open! Core

let bluejay_edesc_to_jay bluejay_edesc =
  let bluejay_ast_internal =
    Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_edesc
  in
  let core_ast, _bluejay_jay_maps =
    Bluejay.Bluejay_to_jay.transform_bluejay bluejay_ast_internal.body
  in
  Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast

let jay_edesc_to_jayil ~is_instrumented jay_edesc =
  Jay_translate.Jay_to_jayil.translate ~is_jay:true ~is_instrumented jay_edesc
  |> fun (e, _, _) -> e

let jay_ast_to_jayil ~is_instrumented jay_ast =
  let jay_edesc = Jay.Jay_ast.new_expr_desc jay_ast in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true ~is_instrumented jay_edesc
  |> fun (e, _, _) -> e

let instrument_jayil_if ~is_instrumented jayil_ast =
  if is_instrumented
  then Jay_instrumentation.Instrumentation.instrument_jayil jayil_ast |> fst
  else jayil_ast
