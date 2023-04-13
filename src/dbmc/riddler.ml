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

let eq_bool key b = SuduZ3.eq (key_to_var key) (SuduZ3.bool_ b)
let z_of_fid (Id.Ident fid) = SuduZ3.fun_ fid
let eq_fid key id = SuduZ3.eq (key_to_var key) (z_of_fid id)
let is_bool key = ifBool (key_to_var key)

let phi_of_value (key : Lookup_key.t) = function
  | Value_function _ -> z_of_fid key.x
  | Value_int i -> SuduZ3.int_ i
  | Value_bool i -> SuduZ3.bool_ i
  | Value_record i -> SuduZ3.record_ (Lookup_key.to_string key)

let phi_of_value_opt (key : Lookup_key.t) = function
  | Some v -> phi_of_value key v
  | None -> key_to_var key

let eqv key v = SuduZ3.eq (key_to_var key) (phi_of_value key v)
let eqvo key v = SuduZ3.eq (key_to_var key) (phi_of_value_opt key v)
let eq key key' = SuduZ3.eq (key_to_var key) (key_to_var key')
let eqz key v = SuduZ3.eq (key_to_var key) v

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
  | Imply_v of Lookup_key.t * Z3.Expr.expr
  | Eq of Lookup_key.t * Lookup_key.t
  | Eq_v of Lookup_key.t * Z3.Expr.expr * Z3.Expr.expr
  | Imply_kv of Lookup_key.t * Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Imply_uni of Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Imply_post of Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Imply_with of Lookup_key.t * Lookup_key.t * Z3.Expr.expr list
  | And2 of Lookup_key.t * Lookup_key.t * Lookup_key.t * Z3.Expr.expr
  | Choices of Lookup_key.t * Lookup_key.t list * Z3.Expr.expr
  | Choices_alt of Lookup_key.t * (Lookup_key.t * Z3.Expr.expr) list
  | Choices_group of
      Lookup_key.t * (Lookup_key.t * Lookup_key.t * Z3.Expr.expr) list

let phi_of_picked pe =
  let loop = function
    | Imply (k1, k2) -> picked k1 @=> picked k2
    | Imply_v (k, v) -> picked k @=> v
    | Eq (k1, k2) -> picked k1 @=> and2 (picked k2) (eq k1 k2)
    | Eq_v (k, v, phis) -> picked k @=> and2 (SuduZ3.eq (key_to_var k) v) phis
    | Imply_kv (k, k1, k2, v2) ->
        picked k
        @=> and_ [ eq k k1; picked k1; SuduZ3.eq (key_to_var k2) v2; picked k2 ]
    | Imply_uni (k1, k2, phis) -> and2 (picked k1 @=> picked k2) phis
    | Imply_post (k1, k2, phis) -> picked k1 @=> and2 (picked k2) phis
    | And2 (k, k1, k2, phis) -> picked k @=> and_ [ picked k1; picked k2; phis ]
    | Choices (k, ks, phis) ->
        picked k
        @=> and2
              (or_ (List.map ks ~f:(fun kv -> and_ [ eq k kv; picked kv ])))
              phis
    | Choices_alt (k, es) ->
        picked k
        @=> or_
              (List.map es ~f:(fun (kv, phiv) ->
                   and_ [ phiv; eq k kv; picked kv ]))
    | Choices_group (k, es) ->
        picked k
        @=> or_
              (List.map es ~f:(fun (kv, k2, k2v) ->
                   and_
                     [
                       eq k kv;
                       picked kv;
                       SuduZ3.eq (key_to_var k2) k2v;
                       picked k2;
                     ]))
    | _ -> true_
  in
  loop pe

type eg_edge =
  | K of (Lookup_key.t * Lookup_key.t)
  | Z of (Lookup_key.t * Z3.Expr.expr)

and eq_edge_neo = eg_edge list

let eq_list ?exclude es =
  let picked k =
    match exclude with
    | Some ke -> if Lookup_key.equal k ke then true_ else picked k
    | None -> picked k
  in
  List.map es ~f:(function
    | K (k1, k2) -> [ eq k1 k2; picked k1; picked k2 ]
    | Z (k, z) -> [ eqz k z; picked k ])
  |> List.concat |> and_

let invalid key = phi_of_picked (Imply_v (key, box_bool false))
let implies key key' = phi_of_picked (Imply (key, key'))
let implies_v1 key key' v = phi_of_picked (Imply_post (key, key', eqv key v))
let not_with_picked t t1 = phi_of_picked (Imply_post (t, t1, not_ t t1))

(* Alias *)
let eq_with_picked key key' = phi_of_picked (Eq (key, key'))

(* Binop *)
let binop_with_picked t op t1 t2 =
  let e_bop = binop t op t1 t2 in
  (* and_ [ e_bop; picked_imply2 t t1 t2 true_ ] *)
  phi_of_picked (And2 (t, t1, t2, e_bop))

(* Cond Top *)
let cond_top key key_x key_c beta =
  phi_of_picked (Imply_kv (key, key_x, key_c, SuduZ3.bool_ beta))

let picked_eq_choices key choices extra =
  phi_of_picked (Choices (key, choices, extra))

(* Rules *)
(* Value rules for main and non-main *)

let at_main key vo =
  phi_of_picked (Eq_v (key, phi_of_value_opt key vo, stack_in_main key.r_stk))

(* Pattern *)

let is_pattern term pat v =
  let x = key_to_var term in
  let open Jayil.Ast in
  match pat with
  | Fun_pattern -> ifFun x
  | Int_pattern -> ifInt x
  | Bool_pattern -> ifBool x
  | Rec_pattern _ -> ifRecord x
  | Strict_rec_pattern _ -> ifRecord x
  | Any_pattern -> true_

let picked_record_pattern x x' matched =
  phi_of_picked
    (Imply_post
       (x, x', and_ [ is_bool x; ifRecord (key_to_var x'); eq_bool x matched ]))
(*
   let is_pattern term pat v =
     let x = key_to_var term in
     let open Jayil.Ast in
     match pat with
     | Fun_pattern -> SuduZ3.eq (SuduZ3.project_bool x) (ifFun x)
     | Int_pattern -> SuduZ3.eq (SuduZ3.project_bool x) (ifInt x)
     | Bool_pattern -> SuduZ3.eq (SuduZ3.project_bool x) (ifBool x)
     | Rec_pattern _ -> and2 (ifRecord x) (eq_bool term v)
     | Strict_rec_pattern _ -> and2 (ifRecord x) (eq_bool term v)
     | Any_pattern -> SuduZ3.eq (SuduZ3.project_bool x) true_ *)

let picked_pattern x x' v' pat =
  let phi_k = match v' with Some b -> eq_bool x b | None -> true_ in
  phi_of_picked
    (Imply_post
       ( x,
         x',
         and_
           [
             is_bool x;
             phi_k;
             SuduZ3.eq
               (SuduZ3.project_bool (key_to_var x))
               (is_pattern x' pat true);
           ] ))

(* Cond Bottom *)
let cond_bottom key key_c rets =
  let es =
    List.map rets ~f:(fun (beta, key_ret) -> (key_ret, eq_bool key_c beta))
  in
  phi_of_picked (Choices_alt (key, es))

(* Fun Enter Local *)
let fun_enter_local (key_para : Lookup_key.t) (p : Rule.Fun_enter_local_rule.t)
    =
  let cs =
    List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
        (key_arg, key_f, z_of_fid p.fid))
  in
  phi_of_picked (Choices_group (key_para, cs))

(* Fun Exit *)
let fun_exit key_arg key_f fids block_map =
  let cs =
    List.map fids ~f:(fun fid ->
        let key_ret = Lookup_key.get_f_return block_map fid key_arg in
        (key_ret, key_f, z_of_fid fid))
  in
  phi_of_picked (Choices_group (key_arg, cs))

(*
   Unbounded cases
*)

(* Fun Enter Nonlocal *)
(* let align_fun_nonlocal key_para key_f key_fv fid key_arg =
   and_
     [
       eq_fid key_f fid;
       eq key_para key_arg;
       eq_fid key_fv fid;
       picked key_f;
       picked key_fv;
       picked key_arg;
     ] *)
(*
let cond_bottom term term_c rets =
  let cs, rs =
    List.fold rets ~init:([], []) ~f:(fun (cs, rs) (beta, term_ret) ->
        let p_ret = picked term_ret in
        let eq_beta = eq_bool term_c beta in
        let eq_lookup = eq term term_ret in
        (cs @ [ p_ret ], rs @ [ p_ret @=> and2 eq_beta eq_lookup ]))
  in
  picked term @=> and_ (or_ cs :: rs)
 let cs =
     List.fold rets ~init:[] ~f:(fun cs (beta, term_ret) ->
         let p_ret = picked term_ret in
         let eq_beta = eq_bool term_c beta in
         let eq_lookup = eq term term_ret in
         cs @ [ and_ [ eq_beta; eq_lookup; p_ret ] ])
   in
   picked term @=> or_ cs *)

(* let fun_enter_local (term : Lookup_key.t) (p : Rule.Fun_enter_local_rule.t) =
   let cs, rs =
     List.fold p.callsites_with_stk ~init:([], [])
       ~f:(fun (cs, rs) (key_f, key_arg) ->
         let pp = and2 (picked key_f) (picked key_arg) in
         ( cs @ [ pp ],
           rs @ [ pp @=> align_fun_para_arg key_f p.fid term key_arg ] ))
   in
   picked term @=> and_ (or_ cs :: rs) *)

(*

   let fun_exit term key_f fids block_map =
     let cs, rs =
       List.fold fids ~init:([], []) ~f:(fun (cs, rs) fid ->
           let key_ret = Lookup_key.get_f_return block_map fid term in
           let p = picked key_ret in
           (cs @ [ p ], rs @ [ p @=> align_fun_para_arg key_f fid key_ret term ]))
     in
     picked term @=> and_ (or_ cs :: rs)
*)
