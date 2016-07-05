(** Two parts - type inconsistencies, and then taking a given core inconsistency and translating it to its nested version *)

open Batteries;;
open Core_ast;;
open Core_ast_pp;;
open Uid;;
open Ddpa_graph;;

type inconsistency =
  | Inexhaustive_match of uid * Toploop_ddpa_types.abs_filtered_value_set
  (** Represents a match expression where the passed in
      expression did not match any of the provided patterns.contents
      Arguments are the uid of the call site clause, and the value
      of the argument passed in. *)
  | If_depends_on_non_bool of uid * Toploop_ddpa_types.abs_filtered_value_set
  (** Represents an if expression where the first expression, the
      "if" clause, was not of type bool. Arguments are the uid of
      the call site clause and the value of the argument passed in. *)
  | Application_of_non_function of uid * uid * abs_filtered_value * Toploop_ddpa_types.abs_filtered_value_set
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

let nested_to_swan map swan =
  match swan with
  | Nested_sugaring.Projection_of_non_record
      (nuid1, nuid2, fv) ->
    Projection_of_non_record(nuid1, nuid2, fv)
  | Nested_sugaring.Projection_of_absent_label
      (nuid1, nuid2, fv, i) ->
    Projection_of_absent_label(nuid1, nuid2, fv, i)
  | Nested_sugaring.Deref_of_non_ref
      (nuid1, nuid2, fv) ->
    Deref_of_non_ref(nuid1, nuid2, fv)
  | Nested_sugaring.Update_of_non_ref
      (nuid1, nuid2, fv) ->
    Update_of_non_ref(nuid1, nuid2, fv)
  | Nested_sugaring.Invalid_binary_operation
      (nuid1, op, nuid2, fv1, nuid3, fv2) ->
    Invalid_binary_operation(nuid1, op, nuid2, fv1, nuid3, fv2)
  | Nested_sugaring.Invalid_unary_operation
      (nuid1, op, nuid2, fv) ->
    Invalid_unary_operation(nuid1, op, nuid2, fv)
  | Nested_sugaring.Invalid_indexing_subject
      (nuid1, nuid2, fv) ->
    Invalid_indexing_subject(nuid1, nuid2, fv)
  | Nested_sugaring.Invalid_indexing_argument
      (nuid1, nuid2, fv) ->
    Invalid_indexing_argument(nuid1, nuid2, fv)
  | Nested_sugaring.Application_of_non_function
      (nuid1, nuid2, fv1, fv2) ->
    match (Uid_map.Exceptionless.find nuid1 map) with
    | Some Egg_translator.Bad_if_branch_to_function(_, if_uid) ->
      If_depends_on_non_bool(if_uid, fv2)
    | Some Egg_translator.Inexhaustive_match_branch(_, m_uid) ->
      Inexhaustive_match(m_uid, fv2)
    | None -> Application_of_non_function(nuid1, nuid2, fv1, fv2)
    | _ -> failwith "Unexpected log entry mapped to NFA"
;;

let batch_translation map nested =
  Enum.map (nested_to_swan map) nested
;;
