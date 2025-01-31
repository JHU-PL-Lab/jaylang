
val embed_desugared : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Desugared.t -> do_wrap:bool -> Lang.Ast.Embedded.t
(** [embed_desugared (module F) des do_wrap] is a program where all types from [des] have been embedded
    as expressions, and type declarations are instrumented as runtime checks. Thus, behavior of
    the output program is not necessarily identical to the input program. Names for new variables
    are created using [module F], which must all be fresh to the program.
    The "wrap" behavior is only on if the [do_wrap] is true. *)
