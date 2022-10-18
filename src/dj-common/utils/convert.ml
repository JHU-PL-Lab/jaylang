open! Core

let bluejay_edesc_to_jay ~do_wrap bluejay_edesc =
  let bluejay_ast_internal =
    Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_edesc
  in
  let core_ast, _bluejay_jay_maps =
    Bluejay.Bluejay_to_jay.transform_bluejay ~do_wrap bluejay_ast_internal.body
  in
  Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast

let bluejay_edesc_to_jayil ~do_wrap ~is_instrumented ~consts bluejay_edesc =
  let consts =
    Bluejay.Bluejay_ast_tools.defined_vars_of_expr_desc bluejay_edesc
    |> Bluejay.Bluejay_ast.Ident_set.to_list
    |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
    |> Jayil.Ast.Var_set.of_list
  in
  let bluejay_ast_internal =
    Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_edesc
  in
  let core_ast, _bluejay_jay_maps =
    Bluejay.Bluejay_to_jay.transform_bluejay ~do_wrap bluejay_ast_internal.body
  in
  let jay_edesc = Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true ~is_instrumented ~consts
    jay_edesc
  |> fun (e, _, _) -> e

let jay_edesc_to_jayil ~is_instrumented ~consts jay_edesc =
  let consts =
    Jay.Jay_ast_tools.defined_vars_of_expr_desc jay_edesc
    |> Jay.Jay_ast.Ident_set.to_list
    |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
    |> Jayil.Ast.Var_set.of_list
  in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true ~is_instrumented ~consts
    jay_edesc
  |> fun (e, _, _) -> e

let jay_ast_to_jayil ~is_instrumented ~consts jay_ast =
  let jay_edesc = Jay.Jay_ast.new_expr_desc jay_ast in
  let consts =
    Jay.Jay_ast_tools.defined_vars_of_expr_desc jay_edesc
    |> Jay.Jay_ast.Ident_set.to_list
    |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
    |> Jayil.Ast.Var_set.of_list
  in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true ~is_instrumented ~consts
    jay_edesc
  |> fun (e, _, _) -> e

let instrument_jayil_if ~is_instrumented jayil_ast =
  if is_instrumented
  then Jay_instrumentation.Instrumentation.instrument_jayil jayil_ast |> fst
  else jayil_ast
