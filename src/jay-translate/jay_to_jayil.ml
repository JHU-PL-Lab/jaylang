let translate ?(is_jay = false) ?(suffix = "___") ?(is_instrumented = true) =
  let context = Translation_context.new_translation_context suffix is_jay () in

  Main.do_translate ~is_instrumented context
