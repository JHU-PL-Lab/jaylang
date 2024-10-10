open! Core
open Jay_translate

type convert_t = {
  jil_ast : Jayil.Ast.expr;
  jay_ast : Jay.Jay_ast.expr_desc option;
  bluejay_ast : Bluejay.Bluejay_ast.expr_desc option;
  jil_inst_map : Jay_instrumentation.Jayil_instrumentation_maps.t;
  jay_jil_map : Jay_to_jayil_maps.t option;
  bluejay_jay_map : Bluejay.Bluejay_to_jay_maps.t option;
}

let convert_t_of4 jil_ast jil_inst_map jay_jil_map bluejay_jay_map =
  {
    jil_ast;
    jay_ast = None;
    bluejay_ast = None;
    jil_inst_map;
    jay_jil_map;
    bluejay_jay_map;
  }

let convert_t_of6 jil_ast jay_ast bluejay_ast jil_inst_map jay_jil_map
    bluejay_jay_map =
  { jil_ast; jay_ast; bluejay_ast; jil_inst_map; jay_jil_map; bluejay_jay_map }

let convert_t_of5 jil_ast jay_ast jil_inst_map jay_jil_map bluejay_jay_map =
  {
    jil_ast;
    jay_ast;
    bluejay_ast = None;
    jil_inst_map;
    jay_jil_map;
    bluejay_jay_map;
  }

let convert_t_to4 ct =
  let {
    jil_ast;
    jay_ast;
    bluejay_ast;
    jil_inst_map;
    jay_jil_map;
    bluejay_jay_map;
  } =
    ct
  in
  (jil_ast, jil_inst_map, jay_jil_map, bluejay_jay_map)

let jil_ast_of_convert c = c.jil_ast
let jay_ast_of_convert c = c.jay_ast
let bluejay_ast_of_convert c = c.bluejay_ast

(* internal functions - start *)

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

(* internal functions - end *)

(* external functions *)

let[@landmarks] bluejay_to_jayil ~do_wrap ~do_instrument raw_bluejay =
  let bluejay_edesc = raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc in
  (* let consts = bluejay_edesc_to_consts bluejay_edesc in *)
  let jay_edesc, bluejay_jay_map =
    bluejay_edesc_to_jay ~do_wrap bluejay_edesc
  in
  let bluejay_instruments = bluejay_jay_map.instrumented_tags in
  (* let () =
    Fmt.pr "\nThis is the Jay expr: %a \n"
      Jay.Jay_ast_pp.pp_expr_desc_without_tag jay_edesc
  in *)
  let a, b, c =
    Jay_translate.Jay_to_jayil.translate ~is_jay:true
      ~is_instrumented:do_instrument (*~consts*) ~bluejay_instruments jay_edesc
  in
  (* let () = Fmt.pr "\nThis is the Jayil expr: %a \n" Jayil.Pp.expr a in *)
  convert_t_of6 a (Some jay_edesc) (Some bluejay_edesc) b (Some c)
    (Some bluejay_jay_map)

let bluejay_to_jay ~do_wrap raw_bluejay =
  raw_bluejay |> Bluejay.Bluejay_ast.new_expr_desc
  |> bluejay_edesc_to_jay ~do_wrap
  |> fst
  |> fun (e : Jay.Jay_ast.expr_desc) -> e.body

let jay_to_jayil ~do_instrument (*?(consts = [])*) raw_jay =
  let jay_ast = raw_jay |> Jay.Jay_ast.new_expr_desc in
  (* let consts = Jayil.Ast.Var_set.of_list consts in *)
  let a, b, c =
    Jay_translate.Jay_to_jayil.translate ~is_jay:true
      ~is_instrumented:do_instrument (*~consts*) jay_ast
  in
  convert_t_of5 a (Some jay_ast) b (Some c) None

let instrument_jayil ~do_instrument raw_jay =
  let inst_jay, jil_inst_map =
    if do_instrument
    then Jay_instrumentation.Instrumentation.instrument_jayil raw_jay
    else (raw_jay, Jay_instrumentation.Jayil_instrumentation_maps.empty false)
  in
  convert_t_of4 inst_jay jil_inst_map None None
