(**
   A type-only declaration module.  This module is intended to be re-exported
   from the Toploop_ddpa module interface and implementation; it should not be
   used directly.
*)

open Batteries;;

open Analysis_context_stack;;
open Ast;;
open Ast_pp;;
open Ddpa_graph;;

type inconsistency =
  | Application_of_non_function of var * var * abs_filtered_value
  (** Represents the application of a non-function value.  The arguments
      are the variable identifying the call site clause, the invoked variable,
      and the abstract non-function value which appeared at the call site. *)
  | Projection_of_non_record of var * var * abs_filtered_value
  (** Represents the projection of a label from a non-record value.  The
      arguments are the variable identifying the clause containing the
      projection, the record variable, and the abstract value from which the
      projection occurred. *)
  | Projection_of_absent_label of var * var * abs_filtered_value * ident
  (** Represents the projection of a label from a record value which does not
      have that label.  The arguments are the variable identifying the clause
      containing the projection, the record variable, the abstract value from
      which the projection occurred, and the ident we failed to project. *)
  | Deref_of_non_ref of var * var * abs_filtered_value
  (** Represents the dereference of a non-ref value.  The arguments are the
      variable identifying the clause where the assignment occurred, the
      dereferenced variable, and the abstract non-cell value which appeared
      there. *)
  | Update_of_non_ref of var * var * abs_filtered_value
  (** Represents the cell-set update of a non-ref value.  The arguments are
      the variable identifying the clause where the assignment occurred, the
      updated variable, and the abstract non-cell value which appeared
      there. *)
  | Invalid_binary_operation of
      var * binary_operator * var * abs_filtered_value *
      var * abs_filtered_value
  (** Represents invalid use of a binary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The binary operator appearing in the clause.
      - The variable for the first operand.
      - A possible value of the first operand.
      - The variable for the second operand.
      - A possible value of the second operand.
  *)
  | Invalid_unary_operation of
      var * unary_operator * var * abs_filtered_value
  (** Represents invalid use of a unary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The unary operator appearing in the clause.
      - The variable for the operand.
      - A possible value of the operand.
  *)
  | Invalid_indexing_subject of
      var * var * abs_filtered_value
  (** Represents the indexing of a non-indexable subject.  The arguments are
      the variable identifying the indexing clause, the variable of the indexing
      subject, and a possible value of the indexing subject. *)
  | Invalid_indexing_argument of
      var * var * abs_filtered_value
  (** Represents an invalid indexing argument.  The arguments are the
      variable identifying the indexing clause, the variable of the index,
      and a possible value of the index. *)
  [@@deriving show]
;;

module type DDPA = sig
  type analysis

  module C : Context_stack;;

  val create_analysis : ?logging_prefix:string option -> expr -> analysis

  val values_of_variable_from :
    var -> annotated_clause -> analysis -> Abs_filtered_value_set.t

  val contextual_values_of_variable_from :
    var -> annotated_clause -> C.t -> analysis -> Abs_filtered_value_set.t

  val check_inconsistencies : analysis -> inconsistency Enum.t

  val pp_analysis : Format.formatter -> analysis -> unit
  val show_analysis : analysis -> string

  val get_size : analysis -> int * int * int * int * int
end;;
