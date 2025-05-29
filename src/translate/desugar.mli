(**
  Module [Desugar].

  Desugar Bluejay programs into desugared programs.

  The implementation of this module is a best attempt closely,
  directly implement the specification in the docs.
*)

val desugar_pgm : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Bluejay.pgm -> do_type_splay:bool -> Lang.Ast.Desugared.pgm
(** [desugar_pgm (module F) bjy] is a desugared program with identical behavior to [bjy], using
    names from [module F] that are guaranteed to be fresh in the program. *)
