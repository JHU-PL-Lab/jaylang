open Core
open Dj_common
open Cfg
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

let not_ t t1 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  fn_not e e1

let binop t op t1 t2 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  let e2 = key_to_var t2 in
  let fop =
    match op with
    | Binary_operator_plus -> fn_plus
    | Binary_operator_minus -> fn_minus
    | Binary_operator_times -> fn_times
    | Binary_operator_divide -> fn_divide
    | Binary_operator_modulus -> fn_modulus
    | Binary_operator_less_than -> fn_lt
    | Binary_operator_less_than_or_equal_to -> fn_le
    | Binary_operator_equal_to -> fn_eq
    | Binary_operator_not_equal_to -> failwith "refactoring"
    | Binary_operator_and -> fn_and
    | Binary_operator_or -> fn_or
  in
  fop e e1 e2

(* with picked *)

let picked_imply key key' = picked key @=> picked key'

let not_with_picked t t1 =
  let e_not = not_ t t1 in
  picked t @=> and_ [ e_not; picked t1 ]

let binop_with_picked t op t1 t2 =
  let e_bop = binop t op t1 t2 in
  picked t @=> and_ [ e_bop; picked t1; picked t2 ]

let eq_with_picked key key' = picked key @=> and_ [ eq key key'; picked key' ]

let is_pattern term pat =
  let x = key_to_var term in
  let is_pattern =
    match pat with
    | Fun_pattern -> ifFun x
    | Int_pattern -> ifInt x
    | Bool_pattern -> ifBool x
    | Rec_pattern _ -> ifRecord x
    | Strict_rec_pattern _ -> ifRecord x
    | Any_pattern -> true_
  in
  is_pattern

let picked_pattern x x' pat =
  let left = key_to_var x in
  let right = is_pattern x' pat in
  picked x
  @=> and_
        [ picked x'; ifBool left; SuduZ3.eq (SuduZ3.project_bool left) right ]

let eqv_with_picked key key' v = picked key @=> and_ [ eqv key v; picked key' ]

let picked_record_pattern x x' matched pat =
  let left = key_to_var x in
  let right = is_pattern x' pat in
  picked x @=> and_ [ picked x'; ifBool left; eqv x matched; right ]

let cond_top term term_x term_c beta =
  picked term
  @=> and_
        [
          picked term_x;
          picked term_c;
          eq term term_x;
          eqv term_c (Value_bool beta);
        ]

let cond_bottom term term_c cond_both =
  let cs, rs =
    List.fold [ true; false ] ~init:([], []) ~f:(fun (cs, rs) beta ->
        let cond_case_block_opt =
          if beta then cond_both.then_ else cond_both.else_
        in
        match cond_case_block_opt with
        | Some cond_case_block ->
            let term_ret =
              Lookup_key.return_key_of_cond term beta cond_case_block
            in
            let p_ret = picked term_ret in
            let eq_beta = eqv term_c (Value_bool beta) in
            let eq_lookup = eq term term_ret in
            (cs @ [ p_ret ], rs @ [ p_ret @=> and2 eq_beta eq_lookup ])
        | None -> (cs, rs))
  in
  picked term @=> and_ (or_ cs :: rs)

let pick_key_list (key : Lookup_key.t) i =
  Lookup_key.to_string key
  (* Rstack.to_string key.r_stk  *)
  ^ "_"
  ^ string_of_int i
  |> SuduZ3.mk_bool_s

let list_head key = picked key @=> pick_key_list key 0

let list_append key i ele =
  pick_key_list key i @=> or_ [ ele; pick_key_list key (i + 1) ]

let record_start key key_r key_rv key_l =
  and_
    [ eq key key_l; eq key_r key_rv; picked key_r; picked key_rv; picked key_l ]

let fun_enter_nonlocal key key_f key_fv fid key_arg =
  and_
    [
      eq_fid key_f fid;
      eq_fid key_fv fid;
      eq key key_arg;
      picked key_f;
      picked key_fv;
      picked key_arg;
    ]

let same_funenter key_f fid key_para key_arg =
  and2 (eq_fid key_f fid) (eq key_para key_arg)

let same_funexit key_f fid key_in key_out =
  and2 (eq_fid key_f fid) (eq key_in key_out)

let fun_enter_local (term : Lookup_key.t) fid callsites block_map =
  let cs, rs =
    List.fold callsites ~init:([], []) ~f:(fun (cs, rs) callsite ->
        let cs_block, x', x'', x''' =
          Cfg.fun_info_of_callsite callsite block_map
        in
        match Rstack.pop term.r_stk (x', fid) with
        | Some callsite_stk ->
            let key_f = Lookup_key.of3 x'' callsite_stk cs_block in
            let key_arg = Lookup_key.of3 x''' callsite_stk cs_block in
            let p = and2 (picked key_f) (picked key_arg) in
            (cs @ [ p ], rs @ [ p @=> same_funenter key_f fid term key_arg ])
        | None -> (cs, rs))
  in
  picked term @=> and_ (or_ cs :: rs)

let fun_exit term key_f fids block_map =
  let cs, rs =
    List.fold fids ~init:([], []) ~f:(fun (cs, rs) fid ->
        let key_ret = Lookup_key.get_f_return block_map fid term in
        let p = picked key_ret in
        (cs @ [ p ], rs @ [ p @=> same_funexit key_f fid key_ret term ]))
  in
  picked term @=> and_ (or_ cs :: rs)

let mismatch_with_picked key = picked key @=> box_bool false

let stack_in_main r_stk =
  SuduZ3.eq top_stack
    (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
   |> Sexp.to_string_mach |> SuduZ3.fun_)

let discover_main (term : Lookup_key.t) v =
  and_ [ eq_term_v term v; stack_in_main term.r_stk ]

let discover_main_with_picked term v = picked term @=> discover_main term v

let discover_non_main key key_first v =
  picked key @=> and2 (eq_term_v key v) (picked key_first)

let is_picked model key =
  Option.value_map model ~default:false ~f:(fun model ->
      Option.value (SuduZ3.get_bool model (picked key)) ~default:true)
