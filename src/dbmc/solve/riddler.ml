open Core
open Dj_common
module SuduZ3 = Solver.SuduZ3
open SuduZ3
open Jayil.Ast
open Log.Export

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

module Record_logic =
  struct
  (* maps ident to bit, where rightmost is bit 0 *)
    let record_label_table : (ident, int) Hashtbl.t = Hashtbl.create (module Ident_new)

    (* assigns bitvector positions from the given labels *)
    let set_labels (labels : Ident_new.t list) : unit =
      Hashtbl.clear record_label_table;
      let b_max = 
        List.fold
          labels
          ~init:0
          ~f:(fun b id ->
            match Hashtbl.find record_label_table id with
            | None -> Hashtbl.set record_label_table ~key:id ~data:b; b + 1 (* give new label the next bit *)
            | Some _ -> b (* repeat label *)
          )
      in
      if b_max > 62 then failwith "too many record labels" else ()

    (* find all labels used anywhere in the ast, then set the labels *)
    let set_labels_from_ast (expr : Jayil.Ast.expr) : unit =
      let rec find_labels_in_ast (Expr clauses : Jayil.Ast.expr) : ident list =
        List.fold clauses ~init:[] ~f:(fun acc clause -> find_labels_in_clause clause @ acc)
      and find_labels_in_clause (clause : Jayil.Ast.clause) : ident list =
        let Clause (Var (x, _), cbody) = clause in
        match cbody with
        | Conditional_body (_, e1, e2) ->
          find_labels_in_ast e1 @ find_labels_in_ast e2
        | Value_body (Value_function (Function_value (_, e))) ->
          find_labels_in_ast e
        | Value_body (Value_record (Record_value m)) ->
          Ident_map.key_list m
        | Match_body (_, (Rec_pattern id_set)) | Match_body (_, (Strict_rec_pattern id_set)) ->
          Ident_set.to_list id_set
        | _ -> []
      in
      find_labels_in_ast expr
      |> set_labels

    let create_bv_from_labels (labels : ident list) : int =
      let set_bit i b = i lor (1 lsl b) in
      List.fold
        labels
        ~init:0
        ~f:(fun acc id -> set_bit acc (Hashtbl.find_exn record_label_table id))
  end (* Record_logic *)

include Record_logic

let ctx = Solver.ctx
let top_stack = SuduZ3.var_s "Topstack"

let picked (key : Lookup_key.t) =
  "P_" ^ Lookup_key.to_string key |> SuduZ3.mk_bool_s

let picked_string (s : string) =
  "P_" ^ s |> SuduZ3.mk_bool_s

let key_to_var key = key |> Lookup_key.to_string |> SuduZ3.var_s
let counter = ref 0
let reset () = counter := 0

(* Solver primitives *)

let ( @=> ) = SuduZ3.( @=> )
let true_ = box_bool true
let false_ = box_bool false
let bool_ = SuduZ3.bool_
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

(* let eq_bool key b = SuduZ3.eq (key_to_var key) (SuduZ3.bool_ b) *)
let z_of_fid (Id.Ident fid) = SuduZ3.fun_ fid
let is_bool key = ifBool (key_to_var key)

let phi_of_value (key : Lookup_key.t) = function
  | Value_function _ -> z_of_fid key.x
  | Value_int i -> SuduZ3.int_ i
  | Value_bool i -> SuduZ3.bool_ i
  | Value_record (Record_value m) ->
    m
    |> Ident_map.key_list
    |> create_bv_from_labels
    |> SuduZ3.record_

let phi_of_value_opt (key : Lookup_key.t) = function
  | Some v -> phi_of_value key v
  | None -> key_to_var key

let eqv key v = SuduZ3.eq (key_to_var key) (phi_of_value key v)
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

type eg_edge =
  | K of (Lookup_key.t * Lookup_key.t)
  | K2 of (Lookup_key.t * Lookup_key.t)
  | Z of (Lookup_key.t * Z3.Expr.expr)
  | D of (Lookup_key.t * Lookup_key.t list)
  | P of Lookup_key.t
  | Phi of Z3.Expr.expr

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
let binop_without_picked = binop (* patch to bring back old binop for concolic evaluator *)

let binop t op t1 t2 =
  let e_bop = binop t op t1 t2 in
  imply t [ P t1; P t2; Phi e_bop ]


(* Cond Top *)
let cond_top key key_x key_c beta =
  imply key [ K2 (key, key_x); Z (key_c, SuduZ3.bool_ beta) ]

let imply_domain k kd = imply k [ D (k, kd) ]
let imply_domain_with k kd pe = imply k ([ D (k, kd) ] @ pe)

(* Rules *)
(* Value rules for main and non-main *)

let at_main key vo =
  imply key [ Z (key, phi_of_value_opt key vo); Phi (stack_in_main key.r_stk) ]

(* Pattern *)

let if_pattern term pat =
  let x = key_to_var term in
  let open Jayil.Ast in
  match pat with
  | Fun_pattern -> ifFun x
  | Int_pattern -> ifInt x
  | Bool_pattern -> ifBool x
  | Rec_pattern label_set ->
    let sub_bv = create_bv_from_labels (Ident_set.to_list label_set) in (* this bitvector should be contained within the record's bv *)
    let projected = SuduZ3.project_record (SuduZ3.record_ sub_bv) in
    SuduZ3.and_
      [ ifRecord x
      ; SuduZ3.eq
          projected
          (Z3.BitVector.mk_and ctx projected (SuduZ3.project_record x))
      ]
  | Strict_rec_pattern label_set ->
    let eq_bv = create_bv_from_labels (Ident_set.to_list label_set) in (* the record's bv should be exactly this *)
    let desired_record = SuduZ3.record_ eq_bv in
    SuduZ3.and_
      [ ifRecord x
      ; SuduZ3.eq
          (SuduZ3.project_record desired_record)
          (SuduZ3.project_record x)
      ]
  | Any_pattern -> true_

(* OB1: For some patterns, we can immediately know the result of the matching:
   when the returning value is a literal value. We can use it in the interpreter.
   We lose this information when the lookup go through a conditional block or
   some binop. *)
(* OB2: The pattern matching can tolerate infeasible cases caused by the analysis,
   because the literal value is incorrect. A conditional block can use this result
   to go into a then-block or a else-block.
*)

let pattern x x' key_rv rv pat =
  let value_matched = Jayil.Ast.pattern_match pat rv in
  let matching_result =
    match value_matched with Some b -> [ Z (x, bool_ b) ] | None -> []
  in
  let type_pattern = if_pattern x' pat in
  let value_pattern =
    if Jayil.Ast.is_record_pattern pat
    then
      match value_matched with
      | Some v -> SuduZ3.inject_bool (and2 type_pattern (box_bool v))
      | None -> SuduZ3.inject_bool type_pattern
    else SuduZ3.inject_bool type_pattern
  in
  imply x
    ([
       Z (x, value_pattern);
       Phi (is_bool x);
       (* Z (x, bool_ value_matched); *)
       K (x', key_rv);
     ]
    @ matching_result)

(* Cond Bottom *)
let cond_bottom key key_c rets =
  let es =
    List.map rets ~f:(fun (beta, key_ret) ->
        [ K2 (key, key_ret); Z (key_c, bool_ beta) ])
  in
  choices key es

(* Fun Enter Local *)
let fun_enter_local (key_para : Lookup_key.t) (p : Rule.Fun_enter_local_rule.t)
    =
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

(* used by concolic interpreter *)
(* OBSOLETE *)
let eq_fid term (Id.Ident fid) = SuduZ3.eq (key_to_var term) (SuduZ3.fun_ fid)

let eq_term_v term v =
  match v with
  (* Ast.Value_body for function *)
  | Some (Value_function _) -> eq_fid term term.x
  (* Ast.Value_body *)
  | Some v -> eqv term v
  (* Ast.Input_body *)
  | None -> eq term term

let enter_fun key_para key_arg = eq key_para key_arg
let exit_fun key_in key_out = eq key_in key_out

(* let is_pattern term pat =
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
  is_pattern *)
