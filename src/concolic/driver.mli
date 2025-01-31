
val test_expr : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Fun.a
(** [test_expr pgm] is the result of concolic evaluation on [pgm],
    or timeout if the timeout limit was exceeded. The result is printed
    to stdout. *)

val test : (Core.Filename.t, Status.Terminal.t) Options.Fun.a
(** [test filename] is the result of concolic evaluation on the Bluejay
    program parsed from [filename], or timeout if the timeout limit was
    exceeded. The result is printed to stdout. *)
