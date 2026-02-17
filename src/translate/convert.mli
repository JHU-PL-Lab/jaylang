(**
   Module [Convert].

   An interface to convert Bluejay programs to desugared
   or type-embedded programs.
*)

val cmd_arg_term : ([ `Do_wrap of bool ] * [ `Do_type_splay of Splay.t ]) Cmdliner.Term.t

val des_to_emb : Lang.Ast.Desugared.pgm -> do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm
(** [des_to_emb des do_wrap do_type_splay] is a program where all types in [des] have
    been embedded as expressions with runtime checks. Thus, behavior of the output program
    is not guaranteed to be identical to the input program.
    The "wrap" behavior is only on if [do_wrap] is true.
    See the docs for translation behavior if [do_type_splay] is yes. *)

val bjy_to_des : Lang.Ast.Bluejay.pgm -> do_type_splay:Splay.t -> Lang.Ast.Desugared.pgm
(** [bjy_to_des bjy] is a desugared program that is identical to [bjy]. *)

val bjy_to_emb : Lang.Ast.Bluejay.pgm -> do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm
(** [bjy_to_emb bjy do_wrap do_type_splay] is a program where all types in [bjy] have
    been embedded as expressions with runtime checks. Thus, behavior of the output program
    is not guaranteed to be identical to the input program.
    The "wrap" behavior is only on if [do_wrap] is true.
    See the docs for translation behavior if [do_type_splay] is yes. *)

val bjy_to_many_emb : Lang.Ast.Bluejay.pgm -> do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm Preface.Nonempty_list.t
(** [bjy_to_many_emb bjy do_wrap do_type_splay] embeds the Bluejay program into many embedded programs,
    each with a different check turned on so that the checks can be run in parallel. *)

val bjy_to_erased : Lang.Ast.Bluejay.pgm -> Lang.Ast.Type_erased.pgm
(** [bjy_to_erased pgm] is a program where all types in the given Bluejay program [pgm]
    are erased. Types are replaced with unusable unit values. *)

val some_program_to_emb : Lang.Ast.some_program -> do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm
(** [some_program_to_emb prog do_wrap do_type_splay] converts a [some_program] structure
    into an embedded program.  Note that [do_wrap] and [do_type_splay] may not apply depending
    upon the contents of [some_program]. *)

val some_program_to_many_emb : Lang.Ast.some_program -> do_wrap:bool -> do_type_splay:Splay.t -> Lang.Ast.Embedded.pgm Preface.Nonempty_list.t
(** [some_program_to_emb prog do_wrap do_type_splay] converts a [some_program] structure
    into many embedded programs, each with a different check turned on so that the checks
    can be run in parallel.  Note that [do_wrap] and [do_type_splay] may not apply
    depending upon the contents of [some_program]. *)