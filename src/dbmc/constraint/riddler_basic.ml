open Core
open Dj_common
open Jayil.Ast
module SuduZ3 = Solver.SuduZ3
open SuduZ3
open Log.Export

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

let ctx = Solver.ctx
let top_stack = SuduZ3.var_s "X_topstack"
(* let picked key = "P_" ^ Lookup_key.to_string key |> SuduZ3.mk_bool_s *)

(* let picked (key : Lookup_key.t) =
   "P_" ^ Rstack.to_string key.r_stk |> SuduZ3.mk_bool_s *)

let picked (key : Lookup_key.t) =
  "P_" ^ Lookup_key.to_string key |> SuduZ3.mk_bool_s

let key_to_var key = key |> Lookup_key.to_string |> SuduZ3.var_s
let counter = ref 0
let reset () = counter := 0
let ( @=> ) = SuduZ3.( @=> )
let true_ = box_bool true
let false_ = box_bool false
let and_ = SuduZ3.and_

let eqv term v =
  let x = key_to_var term in
  let zv =
    match v with
    | Value_int i -> SuduZ3.int_ i
    | Value_bool b -> SuduZ3.bool_ b
    | Value_function _ -> failwith "should not be a function"
    | Value_record _ -> SuduZ3.record_ (Lookup_key.to_string term)
  in
  SuduZ3.eq x zv

let eq_fid term (Id.Ident fid) = SuduZ3.eq (key_to_var term) (SuduZ3.fun_ fid)
let eq key key' = SuduZ3.eq (key_to_var key) (key_to_var key')

let eq_term_v term v =
  match v with
  (* Ast.Value_body for function *)
  | Some (Value_function _) -> eq_fid term term.x
  (* Ast.Value_body *)
  | Some v -> eqv term v
  (* Ast.Input_body *)
  | None -> eq term term

(* with picked *)

let picked_imply key key' = picked key @=> picked key'

let pick_key_list (key : Lookup_key.t) i =
  Lookup_key.to_string key
  (* Rstack.to_string key.r_stk  *)
  ^ "_"
  ^ string_of_int i
  |> SuduZ3.mk_bool_s

let list_head key = picked key @=> pick_key_list key 0

let list_append key i ele =
  pick_key_list key i @=> or_ [ ele; pick_key_list key (i + 1) ]

let is_picked model key =
  Option.value_map model ~default:false ~f:(fun model ->
      Option.value (SuduZ3.get_bool model (picked key)) ~default:true)
