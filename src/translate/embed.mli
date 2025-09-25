(**
  Module [Embed].

  Embed the desugared programs into type-embedded programs.

  The implementation of this module is a best attempt closely,
  directly implement the specification in the docs.
*)

val rec_var_pick : int ref
(** [rec_var_pick] defaults to [123456] and is the integer the interpreter must pick to
    generate a recursive variant constructor. *)

val embed_pgm : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Desugared.pgm ->
    do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm
(** [embed_pgm (module F) des do_wrap do_type_splay] is a program where all types from
    [des] have been embedded as expressions, and type declarations are instrumented as
    runtime checks. Thus, behavior of the output program is not necessarily identical
    to the input program. Names for new variables are created using [module F], which
    must all be fresh to the program.  The "wrap" behavior is only on if the [do_wrap]
    is true. Generated depth of recursive types is stubbed if and only if [do_type_splay]
    is yes. *)

val embed_fragmented : (module Translation_tools.Fresh_names.S) -> Lang.Ast.Desugared.pgm ->
    do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm Preface.Nonempty_list.t
(** [embed_fragmented (module F) des do_wrap do_type_splay] embeds the [des] into many programs
    that each have a different check turned on. *)
