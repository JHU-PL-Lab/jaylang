
val desugar_pgm : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Bluejay.pgm -> Lang.Ast.Desugared.pgm
(** [desugar_pgm (module F) bjy] is a desugared program with identical behavior to [bjy], using
    names from [module F] that are guaranteed to be fresh in the program. *)
