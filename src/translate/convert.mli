
val bjy_to_des : Lang.Ast.Bluejay.t -> Lang.Ast.Desugared.t
(** [bjy_to_des bjy] is a desugared program that is identical to [bjy]. *)

val bjy_to_emb : Lang.Ast.Bluejay.t -> do_wrap:bool -> Lang.Ast.Embedded.t
(** [bjy_to_emb bjy do_wrap] is a program where all types in [bjy] have been embedded as 
    expressions with runtime checks. Thus, behavior of the output program is
    not guaranteed to be identical to the input program.
    The "wrap" behavior is only on if [do_wrap] is true. *)
