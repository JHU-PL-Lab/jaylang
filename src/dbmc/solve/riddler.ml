open Core
open Dj_common
open Log.Export

let ctx = Z3.mk_context []

module C = struct
  let ctx = ctx
end

module Make_solver_helper (C : Sudu.Z3_helper.Context) = struct
  include C
  include Sudu.Z3_helper.Make_helper (C)
  include Sudu.Z3_helper.Contextless_functions
end

module Jil_val = struct
  open Make_solver_helper (C)
  module JZ = Sudu.Z3_datatype.Make_z3_datatype (C)
  include JZ
  include Sudu.Z3_datatype.Make_datatype_ops (JZ) (C)
  open Jayil.Ast

  let t_v1_of_value (key : Lookup_key.t) = function
    | Value_function _ ->
        let (Id.Ident fid) = key.x in
        Sudu.Z3_datatype.Fun fid
    | Value_int i -> Sudu.Z3_datatype.Int i
    | Value_bool b -> Sudu.Z3_datatype.Bool b
    | Value_record i -> Sudu.Z3_datatype.Record (Lookup_key.to_string key)

  let phi_of_value key v = t_v1_of_value key v |> box_value
end

(* module type S = module type of Jil_val *)

module type S = sig
  include module type of struct
    include Sudu.Z3_datatype.Make_datatype_ops (Jil_val.JZ) (C)
  end

  include Sudu.Z3_helper.Jil_z3_datatye

  val phi_of_value : Lookup_key.t -> Jayil.Ast.value -> Z3.Expr.expr
end

module Make_common (Jil_val : S) = struct
  open Jil_val
  include Riddler_helper

  let top_stack = Jil_val.var_s "Topstack"

  let picked (key : Lookup_key.t) =
    "P_" ^ Lookup_key.to_string key |> Jil_val.mk_bool_s

  let picked_string (s : string) = "P_" ^ s |> Jil_val.mk_bool_s
  let counter = ref 0

  (* Solver primitives *)

  let ( @=> ) = Jil_val.( @=> )
  let true_ = box_bool true
  let false_ = box_bool false
  let bool_ = Jil_val.bool_
  let and_ = Jil_val.and_
  let z_of_fid (Id.Ident fid) = Jil_val.fun_ fid

  type eg_edge =
    | K of (Lookup_key.t * Lookup_key.t)
    | K2 of (Lookup_key.t * Lookup_key.t)
    | Z of (Lookup_key.t * Z3.Expr.expr)
    | D of (Lookup_key.t * Lookup_key.t list)
    | P of Lookup_key.t
    | Phi of Z3.Expr.expr
end

module Make_specific (Jil_val : S) = struct
  open Jil_val
  open Make_common (Jil_val)

  (* specific *)
  let reset () = counter := 0

  (* let eq_bool key b = Jil_val.eq (key_to_var key) (Jil_val.bool_ b) *)
  let key_to_var key = key |> Lookup_key.to_string |> Jil_val.var_s

  let phi_of_value_opt (key : Lookup_key.t) = function
    | Some v -> phi_of_value key v
    | None -> key_to_var key
end

module type S2 = module type of struct
  include Make_specific (Jil_val)
end

module Make_common_more (Jil_val : S) (Sudu_more : S2) = struct
  open Jil_val
  open Sudu_more
  open Jayil.Ast
  open Make_common (Jil_val)

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

  let is_bool key = Jil_val.is_bool (key_to_var key)
  let eqv key v = Jil_val.eq (key_to_var key) (phi_of_value key v)
  let eq key key' = Jil_val.eq (key_to_var key) (key_to_var key')
  let eqz key v = Jil_val.eq (key_to_var key) v

  let stack_in_main r_stk =
    Jil_val.eq top_stack
      (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
     |> Sexp.to_string_mach |> Jil_val.fun_)

  (* with picked *)
  let pick_key_list (key : Lookup_key.t) i =
    Lookup_key.to_string key
    (* Rstack.to_string key.r_stk  *)
    ^ "_"
    ^ string_of_int i
    |> Jil_val.mk_bool_s

  let list_head key = picked key @=> pick_key_list key 0

  let list_append key i ele =
    pick_key_list key i @=> or_ [ ele; pick_key_list key (i + 1) ]

  let is_picked model key =
    Option.value_map model ~default:false ~f:(fun model ->
        Option.value (Jil_val.get_bool model (picked key)) ~default:true)

  let eq_domain k kvs =
    or_ (List.map kvs ~f:(fun kv -> and_ [ eq k kv; picked kv ]))

  let eq_list es =
    List.map es ~f:(function
      | K (k1, k2) -> [ eq k1 k2; picked k1; picked k2 ]
      | K2 (k1, k2) -> [ eq k1 k2; picked k2 ]
      | Z (k, z) -> [ eqz k z; picked k ]
      | D (k, kvs) -> [ eq_domain k kvs ]
      | P k -> [ picked k ]
      | Phi p -> [ p ])
    |> List.concat |> and_

  let imply k pe = picked k @=> and_ [ eq_list pe ]
  let choices k pes = picked k @=> or_ (List.map pes ~f:eq_list)
  let invalid key = imply key [ Phi (box_bool false) ]
  let implies key key' = imply key [ P key' ]
  let implies_v key key' v = imply key [ P key'; Z (key, phi_of_value key v) ]
  let not_lookup t t1 = imply t [ P t1; Phi (not_ t t1) ]

  (* Alias *)
  let eq_lookup key key' = imply key [ K (key, key') ]

  (* Binop *)

  let binop_without_picked =
    binop (* patch to bring back old binop for concolic evaluator *)

  (* TODO: redefined binop. not good *)
  let binop t op t1 t2 =
    let e_bop = binop t op t1 t2 in
    imply t [ P t1; P t2; Phi e_bop ]

  (* Cond Top *)
  let cond_top key key_x key_c beta =
    imply key [ K2 (key, key_x); Z (key_c, Jil_val.bool_ beta) ]

  let imply_domain k kd = imply k [ D (k, kd) ]
  let imply_domain_with k kd pe = imply k ([ D (k, kd) ] @ pe)

  (* Rules *)
  (* Value rules for main and non-main *)

  let at_main key vo =
    imply key
      [ Z (key, phi_of_value_opt key vo); Phi (stack_in_main key.r_stk) ]

  (* Cond Bottom *)
  let cond_bottom key key_c rets =
    let es =
      List.map rets ~f:(fun (beta, key_ret) ->
          [ K2 (key, key_ret); Z (key_c, bool_ beta) ])
    in
    choices key es

  (* Fun Enter Local *)
  let fun_enter_local (key_para : Lookup_key.t)
      (p : Rule.Fun_enter_local_rule.t) =
    let cs =
      List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
          [ K2 (key_para, key_arg); Z (key_f, z_of_fid p.fid) ])
    in
    choices key_para cs

  (* Fun Exit *)
  let fun_exit key_arg key_f fids block_map =
    let cs =
      List.map fids ~f:(fun fid ->
          let key_ret = Lookup_key.get_f_return block_map fid key_arg in
          [ K2 (key_arg, key_ret); Z (key_f, z_of_fid fid) ])
    in
    choices key_arg cs

  (* let enter_fun key_para key_arg = eq key_para key_arg
     let exit_fun key_in key_out = eq key_in key_out

     let is_pattern term pat =
       let x = key_to_var term in
       let is_pattern =
         match pat with
         | Fun_pattern -> is_fun x
         | Int_pattern -> is_int x
         | Bool_pattern -> Jil_val.is_bool x
         | Rec_pattern _ -> is_record x
         | Strict_rec_pattern _ -> is_record x
         | Any_pattern -> true_
       in
       is_pattern *)

  (* used by concolic interpreter *)
  (* OBSOLETE *)
  let eq_fid term (Id.Ident fid) =
    Jil_val.eq (key_to_var term) (Jil_val.fun_ fid)

  let eq_term_v term v =
    match v with
    (* Ast.Value_body for function *)
    | Some (Value_function _) -> eq_fid term term.x
    (* Ast.Value_body *)
    | Some v -> eqv term v
    (* Ast.Input_body *)
    | None -> eq term term
end

module Make_more (Jil_val : S) (Sudu_more : S2) = struct
  open Jil_val
  open Sudu_more
  open Jayil.Ast
  open Make_common (Jil_val)

  (* AST primitive (no picked) *)
  (* the folloing are based on Jil_val, key_to_var, phi_of_value *)
  include Make_common_more (Jil_val) (Sudu_more)

  (* Pattern *)

  let if_pattern term pat =
    let x = key_to_var term in
    let open Jayil.Ast in
    match pat with
    | Fun_pattern -> is_fun x
    | Int_pattern -> is_int x
    | Bool_pattern -> Jil_val.is_bool x
    | Rec_pattern _ -> is_record x
    | Strict_rec_pattern _ -> is_record x
    | Any_pattern -> ground_truth

  (* OB1: For some patterns, we can immediately know the result of the matching:
     when the returning value is a literal value. We can use it in the interpreter.
     We lose this information when the lookup go through a conditional block or
     some binop. *)
  (* OB2: The pattern matching can tolerate infeasible cases caused by the analysis,
     because the literal value is incorrect. A conditional block can use this result
     to go into a then-block or a else-block.
  *)

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

module Make (Jil_val : S) (MS : S2) = struct
  open Jayil.Ast
  open Jil_val
  open Log.Export
  include Make_common (Jil_val)
  include MS
  include Make_more (Jil_val) (MS)
  module Solver = Make_solver_helper (C)
end

include Make (Jil_val) (Make_specific (Jil_val))

(* module Make (Jil_val : S) = struct
     open Jayil.Ast
     open Jil_val
     open Log.Export
     include Make_common (Jil_val)
     module MS = Make_specific (Jil_val)
     include MS
     include Make_more (Jil_val) (MS)
     module Solver = Make_solver_helper (C)
   end

   include Make (Jil_val) *)
