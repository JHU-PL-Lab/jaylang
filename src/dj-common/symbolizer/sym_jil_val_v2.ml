open Core
module Jil_val_z3 = Sudu.Z3_datatype_c.Make_z3_datatype_V2 (Solver_helper.C)
include Jil_val_z3
include Sudu.Z3_datatype.Make_datatype_ops (Jil_val_z3) (Solver_helper.C)
open Jayil.Ast
open Sudu.Z3_datatype_c

let t_of_value (key : Lookup_key.t) = function
  | Value_function _ ->
      let (Id.Ident fid) = key.x in
      Sudu.Z3_datatype_c.Fun fid
  | Value_int i -> Sudu.Z3_datatype_c.Int i
  | Value_bool b -> Sudu.Z3_datatype_c.Bool b
  | Value_record (Record_value m) ->
      Sudu.Z3_datatype_c.Record
        (m |> Ident_map.key_list
        |> Record_logic_bv.create_bv_from_labels ~are_labels_predefined:false)

let phi_of_value key v = t_of_value key v |> box_value
let record_ bv = inject_record (box_bitvector bv)

let if_pattern e pat =
  match pat with
  | Fun_pattern -> is_fun e
  | Int_pattern -> is_int e
  | Bool_pattern -> is_bool e
  | Rec_pattern label_set ->
      let sub_bv =
        Record_logic_bv.create_bv_from_labels ~are_labels_predefined:false
          (Ident_set.to_list label_set)
      in
      (* this bitvector should be contained within the record's bv *)
      let projected = project_record (record_ sub_bv) in
      and_
        [
          is_record e;
          eq projected
            (Z3.BitVector.mk_and Solver_helper.ctx projected (project_record e));
        ]
  | Strict_rec_pattern label_set ->
      let eq_bv =
        Record_logic_bv.create_bv_from_labels ~are_labels_predefined:false
          (Ident_set.to_list label_set)
      in
      (* the record's bv should be exactly this *)
      let desired_record = record_ eq_bv in
      and_
        [ is_record e; eq (project_record desired_record) (project_record e) ]
  | Any_pattern -> box_bool true
