(** Two parts - type inconsistencies, and then taking a given core inconsistency and translating it to its nested version *)

open Batteries;;
open A_translator;;
open Core_ast;;
open Core_ast_pp;;
open Uid;;
open Ddpa_graph;;

type inconsistency =
  | Application_of_non_function of uid * uid * abs_filtered_value
  (** Represents the application of a non-function value.  The arguments
      are the uid identifying the call site clause, the invoked uid,
      and the abstract non-function value which appeared at the call site. *)
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

let core_to_nested map core =
  match core with
  | Toploop_ddpa.Application_of_non_function
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Application_of_non_function(uid1, uid2, fv)
  | Toploop_ddpa.Projection_of_non_record
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Projection_of_non_record(uid1, uid2, fv)
  | Toploop_ddpa.Projection_of_absent_label
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv, i) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Projection_of_absent_label(uid1, uid2, fv, i)
  | Toploop_ddpa.Deref_of_non_ref
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Deref_of_non_ref(uid1, uid2, fv)
  | Toploop_ddpa.Update_of_non_ref
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Update_of_non_ref(uid1, uid2, fv)
  | Toploop_ddpa.Invalid_binary_operation
      (Core_ast.Var(v1,_), op, Core_ast.Var(v2,_), fv1, Core_ast.Var(v3,_), fv2) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    let Proof_rule(_, uid3) = Ident_map.find v3 map in
    Invalid_binary_operation(uid1, op, uid2, fv1, uid3, fv2)
  | Toploop_ddpa.Invalid_unary_operation
      (Core_ast.Var(v1,_), op, Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Invalid_unary_operation(uid1, op, uid2, fv)
  | Toploop_ddpa.Invalid_indexing_subject
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Invalid_indexing_subject(uid1, uid2, fv)
  | Toploop_ddpa.Invalid_indexing_argument
      (Core_ast.Var(v1,_), Core_ast.Var(v2,_), fv) ->
    let Proof_rule(_, uid1) = Ident_map.find v1 map in
    let Proof_rule(_, uid2) = Ident_map.find v2 map in
    Invalid_indexing_argument(uid1, uid2, fv)
;;

let batch_translation map cores =
  Enum.map (core_to_nested map) cores
;;
