open Core

let jay_edesc_to_consts jay_edesc =
  Jay.Jay_ast_tools.defined_vars_of_expr_desc jay_edesc
  |> Jay.Jay_ast.Ident_set.to_list
  |> List.map ~f:(fun x -> Jayil.Ast.Var (x, None))
  |> Jayil.Ast.Var_set.of_list

let translate ?(is_jay = false) ?(suffix = "_j_") ?(is_instrumented = true)
    ?(bluejay_instruments = []) ?(consts = Jayil.Ast.Var_set.empty) jay_edesc =
  let consts = Jayil.Ast.Var_set.union (jay_edesc_to_consts jay_edesc) consts in
  let context = Translation_context.new_translation_context suffix is_jay () in

  Main.do_translate ~is_instrumented context consts bluejay_instruments
    jay_edesc
