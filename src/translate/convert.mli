(**
  Module [Convert].

  An interface to convert Bluejay programs to desugared
  or type-embedded programs.
*)

val cmd_arg_term : ([ `Do_wrap of bool ] * [ `Do_type_splay of bool ]) Cmdliner.Term.t

val bjy_to_des : Lang.Ast.Bluejay.pgm -> do_type_splay:bool -> Lang.Ast.Desugared.pgm
(** [bjy_to_des bjy] is a desugared program that is identical to [bjy]. *)

val bjy_to_emb : Lang.Ast.Bluejay.pgm -> do_wrap:bool -> do_type_splay:bool -> Lang.Ast.Embedded.pgm
(** [bjy_to_emb bjy do_wrap do_type_splay] is a program where all types in [bjy] have
    been embedded as expressions with runtime checks. Thus, behavior of the output program
    is not guaranteed to be identical to the input program.
    The "wrap" behavior is only on if [do_wrap] is true.
    See the docs for translation behavior if [do_type_splay] is true. *)

val bjy_to_many_emb : Lang.Ast.Bluejay.pgm -> do_wrap:bool -> do_type_splay:bool -> Lang.Ast.Embedded.pgm Preface.Nonempty_list.t
(** [bjy_to_many_emb bjy do_wrap do_type_splay] embeds the Bluejay program into many embedded programs,
    each with a different check turned on so that the checks can be run in parallel. *)
