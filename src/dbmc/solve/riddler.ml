open Core
open Dj_common
open Jayil.Ast
open Log.Export

let ctx = Z3.mk_context []

module Jil_solver = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module Key_map = struct
  type t = {
    key_to_expr : (Lookup_key.t, Z3.Expr.expr) Hashtbl.t;
    mutable next_key_i : int;
  }
  (** [t] is a mutable type to track how lookup keys relate to unique integers *)

  let create () : t =
    { key_to_expr = Hashtbl.create (module Lookup_key); next_key_i = 0 }

  let clear (x : t) : unit =
    Hashtbl.clear x.key_to_expr ;
    x.next_key_i <- 0

  (* mutates [x] if the key is not found *)
  let get_expr (x : t) (key : Lookup_key.t) : Z3.Expr.expr =
    match Hashtbl.find x.key_to_expr key with
    | Some expr -> expr
    | None ->
        let expr = Jil_solver.var_i x.next_key_i in
        (* make variable out of int *)
        Hashtbl.set x.key_to_expr ~key ~data:expr ;
        x.next_key_i <- x.next_key_i + 1 ;
        (* mark key as used and update to next unused key *)
        expr
end

module Key_to_var_dbmc = struct
  let keys = Key_map.create () (* Not used *)
  let key_to_var key = key |> Lookup_key.to_string |> Jil_solver.var_s
end

module Key_to_var_concolic = struct
  let keys = Key_map.create ()
  (* let key_to_i key =
     Key_map.get_i keys key *)

  let key_to_var key = Key_map.get_expr keys key
end

module type K = sig
  val keys : Key_map.t
  val key_to_var : Lookup_key.t -> Z3.Expr.expr
end

module Make_shared (K : K) = struct
  include K
  include Jil_solver

  type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

  exception Found_solution of result_info

  (* module states *)
  (* let ctx = Jil_solver.ctx *)
  let top_stack = Jil_solver.var_s "Topstack"
  let clear_keys () = Key_map.clear keys
  let counter = ref 0

  let reset () =
    counter := 0 ;
    clear_keys () ;
    Record_logic.clear_labels ()

  let ( @=> ) = Jil_solver.( @=> )

  (* Riddler primitives - no picked *)

  let not_ t t1 =
    let e = key_to_var t in
    let e1 = key_to_var t1 in
    Jil_solver.fn_not e e1

  let stack_in_main r_stk =
    Jil_solver.mk_eq top_stack
      (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
     |> Sexp.to_string_mach |> Jil_solver.fun_)

  let binop t op t1 t2 =
    let open Jayil.Ast in
    let e = key_to_var t in
    let e1 = key_to_var t1 in
    let e2 = key_to_var t2 in
    let fop =
      match op with
      | Binary_operator_plus -> Jil_solver.fn_plus
      | Binary_operator_minus -> Jil_solver.fn_minus
      | Binary_operator_times -> Jil_solver.fn_times
      | Binary_operator_divide -> Jil_solver.fn_divide
      | Binary_operator_modulus -> Jil_solver.fn_modulus
      | Binary_operator_less_than -> Jil_solver.fn_lt
      | Binary_operator_less_than_or_equal_to -> Jil_solver.fn_le
      | Binary_operator_equal_to -> Jil_solver.fn_eq
      (* TODO: This might be buggy. Check later *)
      | Binary_operator_not_equal_to -> Jil_solver.fn_neq
      | Binary_operator_and -> Jil_solver.fn_and
      | Binary_operator_or -> Jil_solver.fn_or
    in
    fop e e1 e2

  let z_of_fid (Id.Ident fid) = Jil_solver.fun_ fid
  let is_bool key = Jil_solver.ifBool (key_to_var key)

  (* let phi_of_value (key : Lookup_key.t) = function
     | Value_function _ -> z_of_fid key.x
     | Value_int i -> Jil_solver.int_ i
     | Value_bool i -> Jil_solver.bool_ i
     | Value_record i -> Jil_solver.record_ (Lookup_key.to_string key) *)

  let phi_of_value (key : Lookup_key.t) = function
    | Value_function _ -> z_of_fid key.x
    | Value_int i -> Jil_solver.int_ i
    | Value_bool i -> Jil_solver.bool_ i
    | Value_record (Record_value m) ->
        m |> Ident_map.key_list
        |> Record_logic.create_bv_from_labels ~are_labels_predefined:false
        |> Jil_solver.record_

  let phi_of_value_opt (key : Lookup_key.t) = function
    | Some v -> phi_of_value key v
    | None -> key_to_var key

  let eqv key v = Jil_solver.mk_eq (key_to_var key) (phi_of_value key v)
  let eq key key' = Jil_solver.mk_eq (key_to_var key) (key_to_var key')
  let eqz key v = Jil_solver.mk_eq (key_to_var key) v

  (* Riddler primitives - with picked *)

  let pick_key_list (key : Lookup_key.t) i =
    Lookup_key.to_string key
    (* Rstack.to_string key.r_stk  *)
    ^ "_"
    ^ string_of_int i
    |> Jil_solver.mk_bool_s

  let picked (key : Lookup_key.t) =
    "P_" ^ Lookup_key.to_string key |> Jil_solver.mk_bool_s

  let picked_string (s : string) = "P_" ^ s |> Jil_solver.mk_bool_s
  let list_head key = picked key @=> pick_key_list key 0

  let list_append key i ele =
    pick_key_list key i @=> Jil_solver.or_ [ ele; pick_key_list key (i + 1) ]

  let is_picked model key =
    Option.value_map model ~default:false ~f:(fun model ->
        Option.value (Jil_solver.get_bool model (picked key)) ~default:true)

  (* A lightweight DSL to simplify constraint encoding.
      This usually suggests a double-layered constraint encoding is probably better
  *)

  type eg_edge =
    | K of (Lookup_key.t * Lookup_key.t)
    | K2 of (Lookup_key.t * Lookup_key.t)
    | Z of (Lookup_key.t * Z3.Expr.expr)
    | D of (Lookup_key.t * Lookup_key.t list)
    | P of Lookup_key.t
    | Phi of Z3.Expr.expr

  let eq_domain k kvs =
    Jil_solver.or_
      (List.map kvs ~f:(fun kv -> Jil_solver.and_ [ eq k kv; picked kv ]))

  let eq_list es =
    List.map es ~f:(function
      | K (k1, k2) -> [ eq k1 k2; picked k1; picked k2 ]
      | K2 (k1, k2) -> [ eq k1 k2; picked k2 ]
      | Z (k, z) -> [ eqz k z; picked k ]
      | D (k, kvs) -> [ eq_domain k kvs ]
      | P k -> [ picked k ]
      | Phi p -> [ p ])
    |> List.concat |> Jil_solver.and_

  let imply k pe = picked k @=> Jil_solver.and_ [ eq_list pe ]
  let choices k pes = picked k @=> Jil_solver.or_ (List.map pes ~f:eq_list)
  let invalid key = imply key [ Phi (Jil_solver.box_bool false) ]
  let implies key key' = imply key [ P key' ]
  let implies_v key key' v = imply key [ P key'; Z (key, phi_of_value key v) ]
  let not_lookup t t1 = imply t [ P t1; Phi (not_ t t1) ]
  let eq_lookup key key' = imply key [ K (key, key') ]

  let binop_without_picked =
    binop (* patch to bring back old binop for concolic evaluator *)

  let binop t op t1 t2 =
    let e_bop = binop t op t1 t2 in
    imply t [ P t1; P t2; Phi e_bop ]

  let imply_domain k kd = imply k [ D (k, kd) ]
  let imply_domain_with k kd pe = imply k ([ D (k, kd) ] @ pe)

  let at_main key vo =
    imply key
      [ Z (key, phi_of_value_opt key vo); Phi (stack_in_main key.r_stk) ]

  let enter_fun key_para key_arg = eq key_para key_arg
  let exit_fun key_in key_out = eq key_in key_out
end

module V_dbmc = struct
  include Make_shared (Key_to_var_dbmc)
  (* OB1: For some patterns, we can immediately know the result of the matching:
     when the returning value is a literal value. We can use it in the interpreter.
     We lose this information when the lookup go through a conditional block or
     some binop. *)
  (* OB2: The pattern matching can tolerate infeasible cases caused by the analysis,
     because the literal value is incorrect. A conditional block can use this result
     to go into a then-block or a else-block.
  *)

  let if_pattern term pat =
    let x = key_to_var term in
    let open Jayil.Ast in
    match pat with
    | Fun_pattern -> Jil_solver.ifFun x
    | Int_pattern -> Jil_solver.ifInt x
    | Bool_pattern -> Jil_solver.ifBool x
    | Rec_pattern _ -> Jil_solver.ifRecord x
    | Strict_rec_pattern _ -> Jil_solver.ifRecord x
    | Any_pattern -> Jil_solver.ground_truth

  let pattern x x' key_rv rv pat =
    LS2Log.debug (fun m ->
        m "pattern %a = %a (<-%a = %a) ~ %a@." Lookup_key.pp x Lookup_key.pp x'
          Lookup_key.pp key_rv Jayil.Pp.clause_body rv Jayil.Pp.pattern pat) ;

    let value_matched = Jayil.Ast.pattern_match pat rv in
    let matching_result =
      match value_matched with
      | Some b -> [ Z (x, Jil_solver.bool_ b) ]
      | None -> []
    in
    let type_pattern = if_pattern x' pat in
    let value_pattern =
      if Jayil.Ast.is_record_pattern pat
      then
        match value_matched with
        | Some v ->
            Jil_solver.inject_bool
              (Jil_solver.and2 type_pattern (Jil_solver.box_bool v))
        | None -> Jil_solver.inject_bool type_pattern
      else Jil_solver.inject_bool type_pattern
    in
    imply x
      ([
         Z (x, value_pattern);
         Phi (is_bool x);
         (* Z (x, bool_ value_matched); *)
         K (x', key_rv);
       ]
      @ matching_result)

  let cond_top key key_x key_c beta =
    imply key [ K2 (key, key_x); Z (key_c, Jil_solver.bool_ beta) ]

  let cond_bottom key key_c rets =
    let es =
      List.map rets ~f:(fun (beta, key_ret) ->
          [ K2 (key, key_ret); Z (key_c, Jil_solver.bool_ beta) ])
    in
    choices key es

  let fun_enter_local (key_para : Lookup_key.t)
      (p : Rule.Fun_enter_local_rule.t) =
    let cs =
      List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
          [ K2 (key_para, key_arg); Z (key_f, z_of_fid p.fid) ])
    in
    choices key_para cs

  let fun_exit key_arg key_f fids block_map =
    let cs =
      List.map fids ~f:(fun fid ->
          let key_ret = Lookup_key.get_f_return block_map fid key_arg in
          [ K2 (key_arg, key_ret); Z (key_f, z_of_fid fid) ])
    in
    choices key_arg cs
end

module V_concolic = struct
  include Make_shared (Key_to_var_concolic)

  (* used by concolic interpreter *)
  (* OBSOLETE *)
  let eq_fid term (Id.Ident fid) =
    Jil_solver.mk_eq (key_to_var term) (Jil_solver.fun_ fid)

  let eq_term_v term v =
    match v with
    (* Ast.Value_body for function *)
    | Some (Value_function _) -> eq_fid term term.x
    (* Ast.Value_body *)
    | Some v -> eqv term v
    (* Ast.Input_body *)
    | None -> eq term term

  (* Pattern *)

  let if_pattern term pat =
    let x = key_to_var term in
    let open Jayil.Ast in
    match pat with
    | Fun_pattern -> Jil_solver.ifFun x
    | Int_pattern -> Jil_solver.ifInt x
    | Bool_pattern -> Jil_solver.ifBool x
    | Rec_pattern label_set ->
        let sub_bv =
          Record_logic.create_bv_from_labels ~are_labels_predefined:false
            (Ident_set.to_list label_set)
        in
        (* this bitvector should be contained within the record's bv *)
        let projected = Jil_solver.project_record (Jil_solver.record_ sub_bv) in
        Jil_solver.and_
          [
            Jil_solver.ifRecord x;
            Jil_solver.mk_eq projected
              (Z3.BitVector.mk_and ctx projected (Jil_solver.project_record x));
          ]
    | Strict_rec_pattern label_set ->
        let eq_bv =
          Record_logic.create_bv_from_labels ~are_labels_predefined:false
            (Ident_set.to_list label_set)
        in
        (* the record's bv should be exactly this *)
        let desired_record = Jil_solver.record_ eq_bv in
        Jil_solver.and_
          [
            Jil_solver.ifRecord x;
            Jil_solver.mk_eq
              (Jil_solver.project_record desired_record)
              (Jil_solver.project_record x);
          ]
    | Any_pattern -> Jil_solver.ground_truth
end

include V_concolic
