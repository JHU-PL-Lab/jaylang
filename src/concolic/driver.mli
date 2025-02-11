
type 'a test = ('a, do_wrap:bool -> in_parallel:bool -> Status.Terminal.t) Options.Arrow.t

val test_bjy : Lang.Ast.Bluejay.pgm test
(** [test_bjy pgm in_parallel] is the result of concolic evaluation on [pgm],
    or timeout if the timeout limit was exceeded. The result is printed
    to stdout. *)

val test : Core.Filename.t test
(** [test filename do_wrap in_parallel] is the result of concolic evaluation on the Bluejay
    program parsed from [filename], or timeout if the timeout limit was
    exceeded. The result is printed to stdout. *)
