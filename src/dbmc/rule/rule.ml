open Core
open Dj_common
open Jayil.Ast
open Jayil.Ast_pp

module Discovery_main_rule = struct
  type t = { v : value }
end

module Discovery_nonmain_rule = struct
  type t = { v : value }
end

module Input_rule = struct
  type t = { is_in_main : bool }
end

module Alias_rule = struct
  type t = { x' : Lookup_key.t }
end

module Not_rule = struct
  type t = { x' : Lookup_key.t }
end

module Binop_rule = struct
  type t = { bop : binary_operator; x1 : Lookup_key.t; x2 : Lookup_key.t }
end

module Record_start_rule = struct
  type t = { r : Lookup_key.t; lbl : Id.t }
end

module Cond_top_rule = struct
  type t = { cond_case_info : Cfg.cond_case_info; condsite_block : Cfg.block }
end

module Cond_btm_rule = struct
  type t = { x' : Lookup_key.t; cond_both : Cfg.cond_both_info }
end

module Fun_enter_local_rule = struct
  type t = { fb : Cfg.fun_block_info; is_local : bool }
end

module Fun_enter_nonlocal_rule = struct
  type t = { fb : Cfg.fun_block_info; is_local : bool }
end

module Fun_exit_rule = struct
  type t = { xf : Lookup_key.t; fids : Id.t list }
end

module Pattern_rule = struct
  type t = { x' : Lookup_key.t; pat : pattern }
end

module Assume_rule = struct
  type t = { x' : Lookup_key.t }
end

module Assert_rule = struct
  type t = { x' : Lookup_key.t }
end

module Abort_rule = struct
  type t = Abort_tag
end

type t =
  | Discovery_main of Discovery_main_rule.t
  | Discovery_nonmain of Discovery_nonmain_rule.t
  | Input of Input_rule.t
  | Alias of Alias_rule.t
  | Not of Not_rule.t
  | Binop of Binop_rule.t
  | Cond_top of Cond_top_rule.t
  | Cond_btm of Cond_btm_rule.t
  | Fun_enter_local of Fun_enter_local_rule.t
  | Fun_enter_nonlocal of Fun_enter_nonlocal_rule.t
  | Fun_exit of Fun_exit_rule.t
  | Record_start of Record_start_rule.t
  | Pattern of Pattern_rule.t
  | Assume of Assume_rule.t
  | Assert of Assert_rule.t
  | Abort of Abort_rule.t
  | Mismatch

let rule_of_runtime_status (key : Lookup_key.t) block_map : t =
  let x = key.x in
  let block = key.block in
  let open Cfg in
  match clause_of_x block x with
  | Some tc -> (
      match tc with
      | { clause = Clause (_, Input_body); _ } ->
          let is_in_main = Ident.equal block.id Id.main_block in
          Input { is_in_main }
      | { clause = Clause (_, Var_body (Var (ix', _))); _ } ->
          let x' = Lookup_key.with_x key ix' in
          Alias { x' }
      | { clause = Clause (_, Value_body v); _ } ->
          if Ident.equal block.id Id.main_block
          then Discovery_main { v }
          else Discovery_nonmain { v }
      | { clause = Clause (_, Projection_body (Var (ir, _), lbl)); _ } ->
          let r = Lookup_key.with_x key ir in
          Record_start { r; lbl }
      | { clause = Clause (_, Not_body (Var (ix', _))); _ } ->
          let x' = Lookup_key.with_x key ix' in
          Not { x' }
      | {
       clause =
         Clause (_, Binary_operation_body (Var (id1, _), bop, Var (id2, _)));
       _;
      } ->
          let x1 = Lookup_key.with_x key id1 in
          let x2 = Lookup_key.with_x key id2 in
          Binop { bop; x1; x2 }
      | { clause = Clause (_, Conditional_body (Var (ix', _), _, _)); _ } ->
          let cond_both_info = Cfg.find_cond_blocks x block_map in
          let x' = Lookup_key.with_x key ix' in
          Cond_btm { x'; cond_both = cond_both_info }
      | {
       clause = Clause (_, Appl_body (Var (ixf, _), Var (_xv, _)));
       cat = App fids;
       _;
      } ->
          let xf = Lookup_key.with_x key ixf in
          Fun_exit { xf; fids }
      | { clause = Clause (_, Abort_body); _ } -> Abort Abort_tag
      | { clause = Clause (_, Assume_body (Var (ix', _))); _ } ->
          let x' = Lookup_key.with_x key ix' in
          Assume { x' }
      | { clause = Clause (_, Assert_body (Var (ix', _))); _ } ->
          let x' = Lookup_key.with_x key ix' in
          Assert { x' }
      | { clause = Clause (_, Match_body (Var (ix', _), pat)); _ } ->
          let x' = Lookup_key.with_x key ix' in
          Pattern { x'; pat }
      | _ ->
          Log.Export.LLog.err (fun m -> m "%a" Jayil.Ast_pp.pp_clause tc.clause) ;
          failwith "Missing rules for this clause")
  | None -> (
      match block.kind with
      | Fun fb ->
          if Ident.(equal fb.para x)
          then Fun_enter_local { fb; is_local = true }
          else Fun_enter_nonlocal { fb; is_local = false }
      | Cond cb ->
          let condsite_block = Cfg.outer_block block block_map in
          Cond_top { cond_case_info = cb; condsite_block }
      | Main -> Mismatch)

let show_rule : t -> string = function
  | Discovery_main _ -> "Discovery_main"
  | Discovery_nonmain _ -> "Discovery_nonmain"
  | Input _ -> "Input"
  | Alias _ -> "Alias"
  | Not _ -> "Not"
  | Binop _ -> "Binop"
  | Cond_top _ -> "Cond_top"
  | Cond_btm _ -> "Cond_btm"
  | Fun_enter_local _ -> "Fun_enter_local"
  | Fun_enter_nonlocal _ -> "Fun_enter_nonlocal"
  | Fun_exit _ -> "Fun_exit"
  | Record_start _ -> "Record_start"
  | Pattern _ -> "Pattern"
  | Assume _ -> "Assume"
  | Assert _ -> "Assert"
  | Abort _ -> "Abort"
  | Mismatch -> "Mismatch"

let sexp_of_t r = r |> show_rule |> Sexp.of_string
let pp_rule = Fmt.of_to_string show_rule

(* module type Rule_sig = sig
     type payload
     type result

     val discovery_main : t -> payload -> result
     val discovery_nonmain : t -> payload -> result
     val value_discard : t -> payload -> result
     val alias : t -> payload -> result
     val to_first : t -> payload -> result
     val binop : t -> payload -> result
     val cond_top : t -> payload -> result
     val cond_btm : t -> payload -> result
     val fun_enter_local : t -> payload -> result
     val fun_enter_nonlocal : t -> payload -> result
     val record_start : t -> payload -> result
     val record_end : t -> payload -> result
     val mismatch : t -> payload -> result
   end *)

(* [@@deriving variants]
   let () =
     print_endline
     @@ Variants.map Value_discover_main
          ~value_discover_main:(fun _ -> "Value_discover_main")
          ~value_discard:(fun _ -> "Value_discard") *)
