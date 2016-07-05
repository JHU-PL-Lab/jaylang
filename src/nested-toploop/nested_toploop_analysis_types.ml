open Uid;;
open Ddpa_graph;;
open Core_toploop_analysis_types;;
open Core_ast;;
open Core_ast_pp;;

type error =
  | Application_of_non_function of uid * uid * abs_filtered_value * abs_filtered_value_set
  (** Represents the application of a non-function value.  The arguments
      are the uid identifying the call site clause, the invoked uid,
      the abstract non-function value which appeared at the call site,
      and the value of the argument applied.  *)
  | Projection_of_non_record of uid * uid * abs_filtered_value
  (** Represents the projection of a label from a non-record value.  The
      arguments are the uid identifying the clause containing the
      projection, the record uid, and the abstract value from which the
      projection occurred. *)
  | Projection_of_absent_label of uid * uid * abs_filtered_value * ident
  (** Represents the projection of a label from a record value which does not
      have that label.  The arguments are the uid identifying the clause
      containing the projection, the record uid, the abstract value from
      which the projection occurred, and the ident we failed to project. *)
  | Deref_of_non_ref of uid * uid * abs_filtered_value
  (** Represents the dereference of a non-ref value.  The arguments are the
      uid identifying the clause where the assignment occurred, the
      dereferenced uid, and the abstract non-cell value which appeared
      there. *)
  | Update_of_non_ref of uid * uid * abs_filtered_value
  (** Represents the cell-set update of a non-ref value.  The arguments are
      the uid identifying the clause where the assignment occurred, the
      updated uid, and the abstract non-cell value which appeared
      there. *)
  | Invalid_binary_operation of
      uid * binary_operator * uid * abs_filtered_value *
      uid * abs_filtered_value
  (** Represents invalid use of a binary operator.  The arguments are, in order,
      - The uid identifying the clause where the assignment occurred.
      - The binary operator appearing in the clause.
      - The uid for the first operand.
      - A possible value of the first operand.
      - The uid for the second operand.
      - A possible value of the second operand.
  *)
  | Invalid_unary_operation of
      uid * unary_operator * uid * abs_filtered_value
  (** Represents invalid use of a unary operator.  The arguments are, in order,
      - The uid identifying the clause where the assignment occurred.
      - The unary operator appearing in the clause.
      - The uid for the operand.
      - A possible value of the operand.
  *)
  | Invalid_indexing_subject of
      uid * uid * abs_filtered_value
  (** Represents the indexing of a non-indexable subject.  The arguments are
      the uid identifying the indexing clause, the uid of the indexing
      subject, and a possible value of the indexing subject. *)
  | Invalid_indexing_argument of
      uid * uid * abs_filtered_value
  (** Represents an invalid indexing argument.  The arguments are the
      uid identifying the indexing clause, the uid of the index,
      and a possible value of the index. *)
  [@@deriving ord,show,eq]
;;
