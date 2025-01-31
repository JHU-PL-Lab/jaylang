
val desugar_bluejay : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Bluejay.t -> Lang.Ast.Desugared.t
(** [desugar_bluejay (module F) bjy] is a desugared program with identical behavior to [bjy], using
    names from [module F] that are guaranteed to be fresh in the program. *)
