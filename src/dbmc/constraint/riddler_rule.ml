open Core
open Dj_common
module SuduZ3 = Solver.SuduZ3
open SuduZ3
open Riddler_basic

(* Value rules for main and non-main *)

let stack_in_main r_stk =
  SuduZ3.eq top_stack
    (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
   |> Sexp.to_string_mach |> SuduZ3.fun_)

let discover_main (term : Lookup_key.t) eq_phi =
  and_ [ eq_phi; stack_in_main term.r_stk ]

let discover_main_with_picked term eq_phi =
  picked term @=> discover_main term eq_phi

let discover_non_main key key_first eq_phi =
  picked key @=> and2 eq_phi (picked key_first)

(* Alias *)

let eq_with_picked key key' = picked key @=> and_ [ eq key key'; picked key' ]

(* Not *)

let not_ t t1 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  fn_not e e1

let not_with_picked t t1 =
  let e_not = not_ t t1 in
  picked t @=> and_ [ e_not; picked t1 ]

(* Binop *)

let binop t op t1 t2 =
  let open Jayil.Ast in
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
    (* TODO: This might be buggy. Check later *)
    | Binary_operator_not_equal_to -> fn_neq
    | Binary_operator_and -> fn_and
    | Binary_operator_or -> fn_or
  in
  fop e e1 e2

let binop_with_picked t op t1 t2 =
  let e_bop = binop t op t1 t2 in
  and_ [ e_bop; picked t @=> and_ [ picked t1; picked t2 ] ]

(* Record start *)

let record_start key key_r key_rv key_l =
  and_
    [ eq key key_l; eq key_r key_rv; picked key_r; picked key_rv; picked key_l ]

(* Cond Top *)

let cond_top term term_x term_c beta =
  picked term
  @=> and_ [ picked term_x; picked term_c; eq term term_x; eq_bool term_c beta ]

(* Cond Bottom *)

let cond_bottom term term_c cond_both =
  let open Cfg in
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
            let eq_beta = eq_bool term_c beta in
            let eq_lookup = eq term term_ret in
            (cs @ [ p_ret ], rs @ [ p_ret @=> and2 eq_beta eq_lookup ])
        | None -> (cs, rs))
  in
  picked term @=> and_ (or_ cs :: rs)

(* Fun *)

(* Fun Enter Nonlocal *)

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

(* Fun Enter Local *)

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

(* Fun Exit *)

let fun_exit term key_f fids block_map =
  let cs, rs =
    List.fold fids ~init:([], []) ~f:(fun (cs, rs) fid ->
        let key_ret = Lookup_key.get_f_return block_map fid term in
        let p = picked key_ret in
        (cs @ [ p ], rs @ [ p @=> same_funexit key_f fid key_ret term ]))
  in
  picked term @=> and_ (or_ cs :: rs)

(* Pattern *)

let eqv_with_picked key key' v = picked key @=> and_ [ eqv key v; picked key' ]

let is_pattern term pat =
  let x = key_to_var term in
  let open Jayil.Ast in
  match pat with
  | Fun_pattern -> ifFun x
  | Int_pattern -> ifInt x
  | Bool_pattern -> ifBool x
  | Rec_pattern _ -> ifRecord x
  | Strict_rec_pattern _ -> ifRecord x
  | Any_pattern -> true_

let picked_record_pattern x x' matched pat =
  let left = key_to_var x in
  let right = is_pattern x' pat in
  picked x @=> and_ [ picked x'; ifBool left; eqv x matched; right ]

let picked_pattern x x' pat =
  let left = key_to_var x in
  let right = is_pattern x' pat in
  picked x
  @=> and_
        [ picked x'; ifBool left; SuduZ3.eq (SuduZ3.project_bool left) right ]

(* Mismatch *)

let mismatch_with_picked key = picked key @=> box_bool false

let eq_one_picked_of key choices =
  List.map choices ~f:(fun choice -> and_ [ eq key choice; picked choice ])

let picked_eq_choices key choices =
  picked key
  @=> or_
        (List.map choices ~f:(fun choice ->
             and_ [ eq key choice; picked choice ]))
