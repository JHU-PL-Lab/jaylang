let translate ?(is_jay = false) ?(suffix = "_j_") ?(is_instrumented = true)
    ?(consts = Jayil.Ast.Var_set.empty) jay_edesc =
  let context = Translation_context.new_translation_context suffix is_jay () in

  Main.do_translate ~is_instrumented context consts jay_edesc
