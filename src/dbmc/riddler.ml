open Core
open Dj_common
module SuduZ3 = Solver.SuduZ3
open SuduZ3
open Jayil.Ast
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

(* Solver primitives *)

let ( @=> ) = SuduZ3.( @=> )
let true_ = box_bool true
let false_ = box_bool false
let and_ = SuduZ3.and_

(* AST primitive (no picked) *)

let not_ t t1 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  fn_not e e1

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

let eq_bool key b = SuduZ3.eq (key_to_var key) (SuduZ3.bool_ b)
let eq_fid key (Id.Ident fid) = SuduZ3.eq (key_to_var key) (SuduZ3.fun_ fid)

let phi_of_value (key : Lookup_key.t) = function
  | Value_function _ ->
      let (Id.Ident fid) = key.x in
      SuduZ3.fun_ fid
  | Value_int i -> SuduZ3.int_ i
  | Value_bool i -> SuduZ3.bool_ i
  | Value_record i -> SuduZ3.record_ (Lookup_key.to_string key)

let phi_of_value_opt (key : Lookup_key.t) = function
  | Some v -> phi_of_value key v
  | None -> key_to_var key

let eqv key v = SuduZ3.eq (key_to_var key) (phi_of_value key v)
let eqvo key v = SuduZ3.eq (key_to_var key) (phi_of_value_opt key v)
let eq key key' = SuduZ3.eq (key_to_var key) (key_to_var key')

let stack_in_main r_stk =
  SuduZ3.eq top_stack
    (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
   |> Sexp.to_string_mach |> SuduZ3.fun_)

(* with picked *)

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

type eq_edge =
  | Dummy
  | Imply of Lookup_key.t * Lookup_key.t
  | Eq of Lookup_key.t * Lookup_key.t
  | Eq_v of Lookup_key.t * Z3.Expr.expr * Z3.Expr.expr
  | One of Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Imply_with of Lookup_key.t * Lookup_key.t * Z3.Expr.expr list
  | And2 of Lookup_key.t * Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Choices of Lookup_key.t * Lookup_key.t list * Z3.Expr.expr

let phi_of_picked pe =
  let loop acc = function
    | Imply (k1, k2) -> (picked k1 @=> picked k2) :: acc
    | Eq (k1, k2) -> (picked k1 @=> and2 (picked k2) (eq k1 k2)) :: acc
    | Eq_v (k, v, phis) ->
        (picked k @=> and2 (SuduZ3.eq (key_to_var k) v) phis) :: acc
    | One (k1, k2, phis) -> (picked k1 @=> and2 (picked k2) phis) :: acc
    | And2 (k, k1, k2, phis) ->
        (picked k @=> and_ [ picked k1; picked k2; phis ]) :: acc
    | Choices (k, ks, phis) ->
        (picked k
        @=> and2
              (or_ (List.map ks ~f:(fun kv -> and_ [ eq k kv; picked kv ])))
              phis)
        :: acc
    | _ -> []
  in
  and_ (loop [] pe)

let picked_imply key key' = phi_of_picked (Imply (key, key'))

let picked_imply_with key key' payload' =
  phi_of_picked (One (key, key', payload'))

let picked_imply_with_v1 key key' v = phi_of_picked (One (key, key', eqv key v))
let not_with_picked t t1 = picked_imply_with t t1 (not_ t t1)

let picked_imply2 key key1 key2 payload12 =
  phi_of_picked (And2 (key, key1, key2, payload12))

(* Alias *)
let eq_with_picked key key' = phi_of_picked (Eq (key, key'))
let picked_false key = picked key @=> box_bool false

(* Binop *)
let binop_with_picked t op t1 t2 =
  let e_bop = binop t op t1 t2 in
  and_ [ e_bop; picked_imply2 t t1 t2 true_ ]

(* Cond Top *)
let cond_top term term_x term_c beta =
  picked_imply2 term term_x term_c (and2 (eq term term_x) (eq_bool term_c beta))

let picked_eq_choices key choices extra =
  phi_of_picked (Choices (key, choices, extra))

(* Rules *)
(* Value rules for main and non-main *)

let picked_main key vo =
  phi_of_picked (Eq_v (key, phi_of_value_opt key vo, stack_in_main key.r_stk))

(* Pattern *)

let picked_record_pattern x x' matched =
  let left = key_to_var x in
  picked_imply_with x x'
    (and_ [ ifBool left; ifRecord (key_to_var x'); eqv x matched ])

let picked_pattern x x' pat =
  let left = key_to_var x in
  picked_imply_with x x'
    (and_
       [ ifBool left; SuduZ3.eq (SuduZ3.project_bool left) (is_pattern x' pat) ])

(* Cond Bottom *)

let cond_bottom term term_c rets =
  let cs, rs =
    List.fold rets ~init:([], []) ~f:(fun (cs, rs) (beta, term_ret) ->
        let p_ret = picked term_ret in
        let eq_beta = eq_bool term_c beta in
        let eq_lookup = eq term term_ret in
        (cs @ [ p_ret ], rs @ [ p_ret @=> and2 eq_beta eq_lookup ]))
  in
  picked term @=> and_ (or_ cs :: rs)

(* Fun *)

(* Fun Enter Nonlocal *)

let fun_enter_nonlocal key_para key_f key_fv fid key_arg =
  and_
    [
      eq_fid key_f fid;
      eq_fid key_fv fid;
      eq key_para key_arg;
      picked key_f;
      picked key_fv;
      picked key_arg;
    ]

let align_fun_para_arg key_f fid key_para key_arg =
  and2 (eq_fid key_f fid) (eq key_para key_arg)

(* Fun Enter Local *)

let fun_enter_local (term : Lookup_key.t) (p : Rule.Fun_enter_local_rule.t) =
  let cs, rs =
    List.fold p.callsites_with_stk ~init:([], [])
      ~f:(fun (cs, rs) (key_f, key_arg) ->
        let pp = and2 (picked key_f) (picked key_arg) in
        ( cs @ [ pp ],
          rs @ [ pp @=> align_fun_para_arg key_f p.fid term key_arg ] ))
  in
  picked term @=> and_ (or_ cs :: rs)

(* Fun Exit *)

let fun_exit term key_f fids block_map =
  let cs, rs =
    List.fold fids ~init:([], []) ~f:(fun (cs, rs) fid ->
        let key_ret = Lookup_key.get_f_return block_map fid term in
        let p = picked key_ret in
        (cs @ [ p ], rs @ [ p @=> align_fun_para_arg key_f fid key_ret term ]))
  in
  picked term @=> and_ (or_ cs :: rs)

(*
   Unbounded cases
*)

(* Record start *)
let record_start key key_r key_rv key_l =
  and_
    [ eq key key_l; eq key_r key_rv; picked key_r; picked key_rv; picked key_l ]
