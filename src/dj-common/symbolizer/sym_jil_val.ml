open Core
open Log.Export
module Jil_val_z3 = Sudu.Z3_datatype.Make_z3_datatype (Solver_helper.C)
include Jil_val_z3
include Sudu.Z3_datatype.Make_datatype_ops (Jil_val_z3) (Solver_helper.C)
open Jayil.Ast

let t_of_value (key : Lookup_key.t) = function
  | Value_function _ ->
      let (Id.Ident fid) = key.x in
      Sudu.Z3_datatype.Fun fid
  | Value_int i -> Sudu.Z3_datatype.Int i
  | Value_bool b -> Sudu.Z3_datatype.Bool b
  | Value_record i -> Sudu.Z3_datatype.Record (Lookup_key.to_string key)

let phi_of_value key v = t_of_value key v |> box_value

let if_pattern e pat =
  let open Jayil.Ast in
  match pat with
  | Fun_pattern -> is_fun e
  | Int_pattern -> is_int e
  | Bool_pattern -> is_bool e
  | Rec_pattern _ -> is_record e
  | Strict_rec_pattern _ -> is_record e
  | Any_pattern -> box_bool true
