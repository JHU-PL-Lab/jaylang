(**
  File: expression.mli
  Purpose: represent expressions for symbolic values

  Detailed description:
    This module builds and simplifies expressions from
    constants, keys, and operations.

    It simplifies constant expressions to be smaller.

    Given a Z3 interface, it converts expressions to Z3
    formulas.

  Dependencies:
    Z3_api -- is the destination of expressions
    Stepkey -- keys represent unconstrained symbolic inputs
*)

module Typed_binop : sig
  type iii = int * int * int
  type iib = int * int * bool
  type bbb = bool * bool * bool

  type _ t =
    | Plus : iii t
    | Minus : iii t
    | Times : iii t
    | Divide : iii t
    | Modulus : iii t
    | Less_than : iib t
    | Less_than_eq : iib t
    | Greater_than : iib t
    | Greater_than_eq : iib t
    | Equal_int : iib t
    | Equal_bool : bbb t
    | Not_equal : iib t
    | And : bbb t
    | Or : bbb t
end

type 'a t
(** ['a t] is a symbolic expression. *)

val is_const : 'a t -> bool
(** [is_const e] is true if and only if the expression is purely constant.
    That is, there are no symbolic components in it. *)

val true_ : bool t
(** [true_] is the constant expression representing [true]. *)

val false_ : bool t
(** [false_] is the constant expression representing [false]. *)

val const_int : int -> int t
(** [const_int i] is the constant expression representing the constant int [i]. *)

val const_bool : bool -> bool t
(** [const_bool b] is the constant expression representing the constant bool [b]. *)

val key : 'a Stepkey.t -> 'a t
(** [key k] is a symbolic expression for the key [k]. *)

val not_ : bool t -> bool t
(** [not_ e] negates the expression [e]. *)

val op : 'a t -> 'a t -> ('a * 'a * 'b) Typed_binop.t -> 'b t
(** [op e1 e2 binop] is an expression for the result of the binary operation [binop]
    on [e1] and [e2]. *)

val equal : 'a t -> 'a t -> bool
(** [equal e1 e2] is true if and only if the expressions [e1] and [e2] are structurally
    equivalent. E.g. [x + 1] is not equivalent to [1 + x]. *)

(* defined only to show Subst *)
module X : sig
  type 'a t = 'a Stepkey.t * 'a
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module Subst : module type of Utils.Pack.Make (X)
(** [Subst] specifies that in the solution to some expressions, the key
    should be replaced with the value. *)

val simplify : bool t list -> Subst.t list * bool t list
(** [simplify exprs] is a list of substitutions that are necessary to satisfy the expressions,
    as well as the remaining expressions.
    
    This only does some basic constant simplification and propagation. *)

module Solve (Expr : Z3_api.S) : sig
  val to_formula : 'a t -> 'a Expr.t
  (** [to_formula e] is a Z3 formula equivalent to [e]. *)
end
