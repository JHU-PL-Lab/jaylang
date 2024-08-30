open Jayil

module Direction :
  sig
    type t =
      | True_direction
      | False_direction
      [@@deriving equal, compare, sexp]

    val to_string : t -> string
    val other_direction : t -> t
    (** [other_direction dir] gets the direction that is not [dir]. Trivial because
        there are only two variant constructors. *)
    val to_value_bool : t -> Ast.value
    (** [to_value_bool dir] gets the boolean associated with the direction
        as an AST value in the Jayil AST. *)
    val of_bool : bool -> t
end

module T :
  sig
    type t =
      { branch_ident : Ast.ident
      ; direction    : Direction.t }
  end

type t = T.t =
  { branch_ident : Ast.ident
  ; direction    : Direction.t }
  [@@deriving sexp, compare]
(** [t] is an AST branch with just the identifier of the clause. The runtime stack is not considered. *)

val to_string : t -> string
(** [to_string branch] ignores the condition of the branch and is the branch ident and direction. *)

val of_ident_and_bool : Ast.ident -> bool -> t
(** [of_ident_and_bool branch_ident dir] converts the bool [dir] to a direction and uses that to
    make a branch. *)

val other_direction : t -> t

(*
  "Runtime" is a modifier/adjective on "Branch", so it is a "Runtime Branch".
*)
module Runtime :
  sig
    type t =
      { branch_key    : Concolic_key.t
      ; condition_key : Concolic_key.t
      ; direction     : Direction.t } [@@deriving compare, sexp]
    (** [t] is a branch in the AST during runtime, where its branch and condition both have a stack
        to identify them (hence they are a [Concolic_key.t]).
        The [branch_key] is the key of the clause to identify the node in the AST, and the [condition_key]
        is the key of the condition variable. 
        
        To use a runtime branch as a target for the concolic evaluator (because the concolic evaluator
        attempts to hit all directions of a branch), the [direction] might be switched to the other side. *)

    val to_expr : t -> Z3.Expr.expr
    (** [to_expr x] is a Z3 expression that sets the condition key in [x] to be equal to the direction as a bool. *)

    val to_string : t -> string
    (** [to_string x] shows all of the branch, condition, and direction in [x] as a string. *)

    val to_string_short : t -> string

    val to_ast_branch : t -> T.t
    (** [to_ast_branch x] is an AST branch, where only the ident is kept from the branch key, and condition is discarded. *)

    val other_direction : t -> t
    (** [other_direction x] is a new branch keeping all attributes of [x], but [x.direction] is flipped. *)

    val print_target_option : t option -> unit
    (** [print_target_option x] prints the branch [x] as "target branch", or prints it as "None". *)
  end

module Or_global :
  sig
    type t =
      | Global
      | Branch of T.t
  end