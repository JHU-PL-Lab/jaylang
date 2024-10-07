
module Untyped_binop :
  sig
    type t =
      | Plus
      | Minus
      | Times
      | Divide
      | Modulus
      | Less_than
      | Less_than_eq
      | Equal_int
      | Equal_bool
      | Not_equal
      | And
      | Or
  end

module T :
  sig
    type 'a t
  end

type 'a t = 'a T.t
(** [t] is an expression interface for Z3 formulas that makes simplifications on constant expressions. *)

val const_bool : bool -> bool t
(** [const_bool b] is an expression for the constant bool [b]. *)

val const_int : int -> int t
(** [const_int i] is an expression for the constant int [i]. *)

val true_ : bool t
(** [true_] is [const_bool true]. *)

val bool_key : Concolic_key.t -> bool t
(** [bool_key key] is an expression that the [key] is an abstract boolean. *)

val int_key : Concolic_key.t -> int t
(** [int_key key] is an expression that the [key] is an abstract integer. *)

val not_ : bool t -> bool t
(** [not_ e] is the negation of [e]. *)

val plus : int t -> int t -> int t
(** [plus e1 e2] is an expression for [e1] plus [e2]. *)

val minus : int t -> int t -> int t
(** [minus e1 e2] is an expression for [e1] minus [e2]. *)

val times : int t -> int t -> int t
(** [times e1 e2] is an expression for [e1] times [e2]. *)

val divide : int t -> int t -> int t
(** [divide e1 e2] is an expression for [e1] divided by [e2]. *)

val modulus : int t -> int t -> int t
(** [modulus e1 e2] is an expression for [e1] mod [e2]. *)

val less_than : int t -> int t -> bool t
(** [less_than e1 e2] is an expression for [e1] is less than [e2]. *)

val less_than_eq : int t -> int t -> bool t
(** [less_than_eq e1 e2] is an expression for [e1] is less than or equal to [e2]. *)

val equal_int : int t -> int t -> bool t
(** [equal_int e1 e2] is an expression for [e1] is equal to [e2]. *)

val equal_bool : bool t -> bool t -> bool t
(** [equal_bool e1 e2] is an expression for [e1] is equal to [e2]. *)

val not_equal : int t -> int t -> bool t
(** [not_equal e1 e2] is an expression for [e1] is not equal to [e2]. *)

val and_ : bool t -> bool t -> bool t
(** [and_ e1 e2] is an expression for the logical operation [e1] and [e2]. *)

val or_ : bool t -> bool t -> bool t
(** [or_ e1 e2] is an expression for the logical operation [e1] or [e2]. *)

val t_to_formula : 'a t -> 'a C_sudu.Gexpr.t
(** [t_to_formula e] is a typed Z3 formula for the expression [e]. *)

module Cache :
  sig
    type t
    (** [t] maps keys to expressions. It is the medium between loosely typed keys and strongly typed expressions.
        Thus, exceptions may be thrown if the a key is unbound or if the type of the expression for the key is
        incompatible with the desired use. (e.g. "not 1", or something of the like). *)

    val empty : t
    (** [empty] knows no mappings. *)

    val lookup_int : t -> Concolic_key.t -> int T.t
    (** [lookup_int t key] is an unsafe lookup for the int expression associated with [key] in [t]. *)
    
    val lookup_bool : t -> Concolic_key.t -> bool T.t
    (** [lookup_bool t key] is an unsafe lookup for the bool expression associated with [key] in [t]. *)

    val add_expr : t -> Concolic_key.t -> 'a T.t -> t
    (** [add_expr t key e] has that [key] maps to [e]. *)

    val add_alias : Concolic_key.t -> Concolic_key.t -> t -> t
    (** [add_alias k k' t] has that [k] maps to the same expression as [k'], where [k'] is already
        known to map to some expression. Otherwise, [t] is unchanged. *)

    val is_const_bool : t -> Concolic_key.t -> bool
    (** [is_const_bool key t] is false if and only if [key] maps to an abstract expression. *)

    val not_ : t -> Concolic_key.t -> Concolic_key.t -> t
    (** [not_ t k k'] has that [k] maps to the negation of the expression that [k'] maps to in [t]. *)

    val binop : Concolic_key.t -> Untyped_binop.t -> Concolic_key.t -> Concolic_key.t -> t -> t
    (** [binop k op left right t] has that [k] maps to [left op right] in [t], or exception if the types of the keys
        are not compatible with the binop.*)
  end

