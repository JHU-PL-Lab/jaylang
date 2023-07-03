open! Core

let bluejay_edesc_to_consts bluejay_edesc =
  Bluejay.Bluejay_ast_tools.defined_vars_of_expr_desc bluejay_edesc
  |> Jay.Jay_ast.Ident_set.to_list
  |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
  |> Jayil.Ast.Var_set.of_list

let jay_edesc_to_consts jay_edesc =
  Jay.Jay_ast_tools.defined_vars_of_expr_desc jay_edesc
  |> Jay.Jay_ast.Ident_set.to_list
  |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
  |> Jayil.Ast.Var_set.of_list

let bluejay_edesc_to_core_ast ~do_wrap bluejay_edesc =
  let bluejay_ast_internal =
    Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_edesc
  in
  let core_ast, bluejay_jay_maps =
    Bluejay.Bluejay_to_jay.transform_bluejay ~do_wrap bluejay_ast_internal.body
  in
  (core_ast, bluejay_jay_maps)

let bluejay_edesc_to_jay ~do_wrap bluejay_edesc =
  let core_ast, _bluejay_jay_maps =
    bluejay_edesc_to_core_ast ~do_wrap bluejay_edesc
  in
  Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast

let bluejay_edesc_to_jayil ~do_wrap ~do_instrument ~consts bluejay_edesc =
  let consts = bluejay_edesc_to_consts bluejay_edesc in
  let jay_edesc = bluejay_edesc_to_jay ~do_wrap bluejay_edesc in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true
    ~is_instrumented:do_instrument ~consts jay_edesc
  |> fun (e, _, _) -> e

let jay_edesc_to_jayil ~do_instrument ~consts jay_edesc =
  let consts = jay_edesc_to_consts jay_edesc in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true
    ~is_instrumented:do_instrument ~consts jay_edesc
  |> fun (e, _, _) -> e

let jay_ast_to_jayil ~do_instrument ~consts jay_ast =
  let jay_edesc = Jay.Jay_ast.new_expr_desc jay_ast in
  let consts = jay_edesc_to_consts jay_edesc in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true
    ~is_instrumented:do_instrument ~consts jay_edesc
  |> fun (e, _, _) -> e

let instrument_jayil_if ~do_instrument jayil_ast =
  if do_instrument
  then Jay_instrumentation.Instrumentation.instrument_jayil jayil_ast |> fst
  else jayil_ast

let raw_bluejay_to_jayil ~do_wrap ~do_instrument raw_bluejay =
  let bluejay_ast = raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc in
  let init_consts = bluejay_edesc_to_consts bluejay_ast in
  bluejay_edesc_to_jayil ~do_wrap ~do_instrument bluejay_ast ~consts:init_consts

let raw_bluejay_to_jay ~do_wrap raw_bluejay =
  raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc
  |> bluejay_edesc_to_jay ~do_wrap

let raw_jay_to_jayil ~do_instrument raw_jay =
  let jay_ast = raw_jay |> Jay.Jay_ast.new_expr_desc in
  let consts = jay_edesc_to_consts jay_ast in
  jay_edesc_to_jayil ~do_instrument ~consts jay_ast
