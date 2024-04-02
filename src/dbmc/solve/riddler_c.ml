open Core
open Dj_common
open Log.Export

let ctx = Riddler.ctx

module Jil_val = struct
  open Riddler.Make_solver_helper (Riddler.C)
  module JZ = Sudu.Z3_datatype_c.Make_z3_datatype_V2 (Riddler.C)
  include JZ
  include Sudu.Z3_datatype.Make_datatype_ops (JZ) (Riddler.C)
  open Jayil.Ast
  open Sudu.Z3_datatype_c

  let t_v2_of_value (key : Lookup_key.t) = function
    | Value_function _ ->
        let (Id.Ident fid) = key.x in
        Sudu.Z3_datatype_c.Fun fid
    | Value_int i -> Sudu.Z3_datatype_c.Int i
    | Value_bool b -> Sudu.Z3_datatype_c.Bool b
    | Value_record (Record_value m) ->
        Sudu.Z3_datatype_c.Record
          (m |> Ident_map.key_list
          |> Record_logic.create_bv_from_labels ~are_labels_predefined:false)

  let phi_of_value key v = t_v2_of_value key v |> box_value
end

module Make_specific (Jil_val : Riddler.S) = struct
  open Jil_val
  open Riddler.Make_common (Jil_val)

  let keys = Key_map.create ()

  let reset () =
    counter := 0 ;
    Key_map.clear keys ;
    Record_logic.clear_labels ()
  (* let key_to_i key =
     Key_map.get_i keys key *)

  (* let key_to_var key = key |> Lookup_key.to_string |> Jil_val.var_s *)
  let key_to_var key = Key_map.get_expr keys key Jil_val.var_i

  let phi_of_value_opt (key : Lookup_key.t) = function
    | Some v -> phi_of_value key v
    | None -> key_to_var key
end

module Make_more (Jil_val : Riddler.S) (Sudu_more : Riddler.S2) = struct
  open Jil_val
  open Sudu_more
  open Jayil.Ast
  open Riddler.Make_common (Jil_val)

  (* AST primitive (no picked) *)
  include Riddler.Make_common_more (Jil_val) (Sudu_more)

  let record_ bv = inject_record (box_bitvector bv)

  (* Pattern *)

  let if_pattern term pat =
    let x = key_to_var term in
    let open Jayil.Ast in
    match pat with
    | Fun_pattern -> is_fun x
    | Int_pattern -> is_int x
    | Bool_pattern -> Jil_val.is_bool x
    | Rec_pattern label_set ->
        let sub_bv =
          Record_logic.create_bv_from_labels ~are_labels_predefined:false
            (Ident_set.to_list label_set)
        in
        (* this bitvector should be contained within the record's bv *)
        let projected = Jil_val.project_record (record_ sub_bv) in
        Jil_val.and_
          [
            is_record x;
            Jil_val.eq projected
              (Z3.BitVector.mk_and ctx projected (Jil_val.project_record x));
          ]
    | Strict_rec_pattern label_set ->
        let eq_bv =
          Record_logic.create_bv_from_labels ~are_labels_predefined:false
            (Ident_set.to_list label_set)
        in
        (* the record's bv should be exactly this *)
        let desired_record = record_ eq_bv in
        Jil_val.and_
          [
            is_record x;
            Jil_val.eq
              (Jil_val.project_record desired_record)
              (Jil_val.project_record x);
          ]
    | Any_pattern -> true_

  let pattern x x' key_rv rv pat =
    LS2Log.debug (fun m ->
        m "pattern %a = %a (<-%a = %a) ~ %a@." Lookup_key.pp x Lookup_key.pp x'
          Lookup_key.pp key_rv Jayil.Pp.clause_body rv Jayil.Pp.pattern pat) ;

    let value_matched = Jayil.Ast.pattern_match pat rv in
    let matching_result =
      match value_matched with Some b -> [ Z (x, bool_ b) ] | None -> []
    in
    let type_pattern = if_pattern x' pat in
    let value_pattern =
      if Jayil.Ast.is_record_pattern pat
      then
        match value_matched with
        | Some v -> Jil_val.inject_bool (and2 type_pattern (box_bool v))
        | None -> Jil_val.inject_bool type_pattern
      else Jil_val.inject_bool type_pattern
    in
    imply x
      ([
         Z (x, value_pattern);
         Phi (is_bool x);
         (* Z (x, bool_ value_matched); *)
         K (x', key_rv);
       ]
      @ matching_result)
end

module Make (Jil_val : Riddler.S) (MS : Riddler.S2) = struct
  open Jayil.Ast
  open Jil_val
  open Log.Export
  include Riddler.Make_common (Jil_val)
  include MS
  include Make_more (Jil_val) (MS)
  module Solver = Riddler.Make_solver_helper (Riddler.C)
end

include Make (Jil_val) (Make_specific (Jil_val))
