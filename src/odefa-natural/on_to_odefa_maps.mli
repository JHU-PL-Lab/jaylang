open Odefa_ast;;

type t [@@deriving show];;

val empty : bool -> t;;

(* **** Setter functions **** *)

(** Add an mapping from an odefa ident added during instrumentation to an
    odefa ident option, corresponding to some pre-instrumentation ident it
    aliases.  The value is None if there is not a corresponding aliased
    ident (e.g. vars added during match expr flattening). *)
val add_odefa_instrument_var : t -> Ast.ident -> Ast.ident option -> t;;

(** Add a mapping from an odefa ident to the clause it identifies.  These ident
    to clause pairs must be added before instrumentation. *)
val add_odefa_var_clause_mapping : t -> Ast.ident -> Ast.clause -> t;;

(** Add a mapping from an odefa ident to the natodefa expression that, when
    flattened, produced its odefa clause. *)
val add_odefa_var_on_expr_mapping : t -> Ast.ident -> On_ast.expr_desc -> t;;

(** Add a mapping between two natodefa expressions.  These pairs are added
    when let rec, list, and variant expressions/patterns are desuraged. *)
val add_on_expr_to_expr_mapping : t -> On_ast.expr_desc -> On_ast.expr_desc -> t;;

(** Add a mapping between two natodefa idents.  These pairs are to be added
    when an ident is renamed during alphatization. *)
val add_on_var_to_var_mapping : t -> On_ast.ident -> On_ast.ident -> t;;

(** Add a mapping between a set of natodefa idents to a natodefa type.  These
    are used to identify record expressions/patterns that are the result of
    desugaring lists or variants. *)
val add_on_idents_to_type_mapping : t -> On_ast.Ident_set.t -> On_ast.type_sig -> t;;

(* **** Getter functions **** *)

(** Get an odefa clause that existed before the odefa program was
    instrumented.  If the odefa var was added during type instrumentation,
    it will return the clause representing the constrained operation.
    If the var was added before, it will return the clause it identified
    before instrumentation. *)
val get_pre_inst_equivalent_clause : t -> Ast.ident -> Ast.clause;;

(** Get the natodefa expression that the odefa clause that the odefa var
    identifies maps to. *)
val get_natodefa_equivalent_expr : t -> Ast.ident -> On_ast.expr_desc;;

(** Get the natodefa type that a set of record labels corresponds to.  If
    there is no mapping that exists, return a record type by default. *)
val get_type_from_idents : t -> Ast.Ident_set.t -> On_ast.type_sig;;

(** Returns whether the mapping was created during a translation of natodefa
    code or not. *)
val is_natodefa : t -> bool;;

(** Returns true if the ident was added during instrumentation, false
    otherwise. *)
val is_var_instrumenting : t -> Ast.ident -> bool;;