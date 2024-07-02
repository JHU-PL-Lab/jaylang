open Jay
open Jayil

type t [@@deriving show]

val empty : bool -> t

(* **** Setter functions **** *)

val add_jayil_var_jay_expr_mapping : t -> Ast.ident -> Jay_ast.expr_desc -> t
(** Add a mapping from an jayil ident to the jay expression that, when
    flattened, produced its jayil clause. *)

val add_jay_expr_to_expr_mapping :
  t -> Jay_ast.expr_desc -> Jay_ast.expr_desc -> t
(** Add a mapping between two jay expressions. These pairs are added when let
    rec, list, and variant expressions/patterns are desuraged. *)

val add_jay_var_to_var_mapping : t -> Jay_ast.ident -> Jay_ast.ident -> t
(** Add a mapping between two jay idents. These pairs are to be added when an
    ident is renamed during alphatization. *)

val add_jay_idents_to_type_mapping :
  t -> Jay_ast.Ident_set.t -> Jay_ast.type_sig -> t
(** Add a mapping between a set of jay idents to a jay type. These are used to
    identify record expressions/patterns that are the result of desugaring lists
    or variants. *)

val add_jay_instrument_var : t -> Ast.ident -> Ast.ident option -> t
(** Add an mapping from an jayil ident added during instrumentation to an jayil
    ident option, corresponding to some pre-instrumentation ident it aliases.
    The value is None if there is not a corresponding aliased ident (e.g. vars
    added during match expr flattening). *)

val add_const_var : t -> Ast.var -> t

val update_jayil_mappings : t -> Ast.var Ast.Var_map.t -> t

val update_instrumented_tags : t -> int list -> t

val is_jay_instrumented : t -> int -> bool

val add_jay_instrumented : t -> int -> t

(* **** Getter functions **** *)

val get_jay_equivalent_expr : t -> Ast.ident -> Jay_ast.expr_desc option
(** Get the jay expression that the jayil clause that the jayil var identifies
    maps to. *)

val get_jay_equivalent_expr_exn : t -> Ast.ident -> Jay_ast.expr_desc

val get_type_from_idents : t -> Ast.Ident_set.t -> Jay_ast.type_sig
(** Get the jay type that a set of record labels corresponds to. If there is no
    mapping that exists, return a record type by default. *)

val jayil_to_jay_aliases : t -> Ast.ident list -> Jay_ast.expr_desc list

val get_jayil_vars_from_jay_expr : t -> Jay_ast.expr_desc -> Ast.var list
(** Given a jay expression, returns the corresponding variable in desugared
    jayil. *)

val get_const_vars : t -> Ast.var list

val get_jay_inst_map : t -> Ast.ident option Ast.Ident_map.t

