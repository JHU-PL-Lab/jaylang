open! Core

let bluejay_edesc_to_consts bluejay_edesc =
  Bluejay.Bluejay_ast_tools.defined_vars_of_expr_desc bluejay_edesc
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
  let core_ast, bluejay_jay_map =
    bluejay_edesc_to_core_ast ~do_wrap bluejay_edesc
  in
  let bluejay_jay_map' =
    bluejay_edesc |> Bluejay.Bluejay_ast_internal.to_internal_expr_desc
    |> Bluejay.Bluejay_to_jay_maps.find_all_syn_tags bluejay_jay_map
  in
  (Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast, bluejay_jay_map')

let bluejay_to_jayil ~do_wrap ~do_instrument raw_bluejay =
  let bluejay_edesc = raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc in
  let consts = bluejay_edesc_to_consts bluejay_edesc in
  let jay_edesc, bluejay_jay_map =
    bluejay_edesc_to_jay ~do_wrap bluejay_edesc
  in
  let bluejay_instruments = bluejay_jay_map.instrumented_tags in
  let a, b, c =
    Jay_translate.Jay_to_jayil.translate ~is_jay:true
      ~is_instrumented:do_instrument ~consts ~bluejay_instruments jay_edesc
  in
  (a, b, c, bluejay_jay_map)

let jay_ast_to_jayil ~do_instrument ~consts jay_ast =
  let jay_edesc = Jay.Jay_ast.new_expr_desc jay_ast in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true
    ~is_instrumented:do_instrument ~consts jay_edesc
  |> fun (e, _, _) -> e

(* entrance functions (should have no real implementation) *)

let raw_bluejay_to_jay ~do_wrap raw_bluejay =
  raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc
  |> bluejay_edesc_to_jay ~do_wrap

let raw_jay_to_jayil ~do_instrument raw_jay =
  let jay_ast = raw_jay |> Jay.Jay_ast.new_expr_desc in
  Jay_translate.Jay_to_jayil.translate ~is_jay:true
    ~is_instrumented:do_instrument jay_ast
