
val erase : Lang.Ast.Bluejay.pgm -> Lang.Ast.Type_erased.pgm
(** [erase pgm] is a program where all types in the given Bluejay program [pgm]
    are erased. Types are replaced with unusable unit values. *)