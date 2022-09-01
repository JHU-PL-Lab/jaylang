open Jayil;;

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

(** Returns whether the mapping was created during a translation of natodefa
    code or not. *)
val is_natodefa : t -> bool;;

(** Returns true if the ident was added during instrumentation, false
    otherwise. *)
val is_var_instrumenting : t -> Ast.ident -> bool;;

(** Get an odefa clause that existed before the odefa program was
    instrumented.  If the odefa var was added during type instrumentation,
    it will return the clause representing the constrained operation.
    If the var was added before, it will return the clause it identified
    before instrumentation. *)
val get_pre_inst_equivalent_clause : t -> Ast.ident -> Ast.clause;;

val get_pre_inst_var_opt : t -> Ast.ident -> Ast.ident option;;

val inherit_from_on_to_odefa_maps : (Ast.ident option) Ast.Ident_map.t -> t;; 