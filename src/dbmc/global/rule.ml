open Core
open Odefa_ast.Ast
open Odefa_ast.Ast_pp

module Discovery_main_rule = struct
  type t = { x : Id.t; v : value }
end

module Discovery_nonmain_rule = struct
  type t = { x : Id.t; v : value }
end

module Input_rule = struct
  type t = { x : Id.t; is_in_main : bool }
end

module Alias_rule = struct
  type t = { x : Id.t; x' : Id.t }
end

module Binop_rule = struct
  type t = { x : Id.t; bop : binary_operator; x1 : Id.t; x2 : Id.t }
end

module Record_start_rule = struct
  type t = { x : Id.t; r : Id.t; lbl : Id.t }
end

module Record_end_rule = struct
  type t = { x : Id.t; r : record_value }
end

module Cond_top_rule = struct
  type t = Tracelet.cond_block
end

module Cond_btm_rule = struct
  type t = { x : Id.t; x' : Id.t; tid : Id.t }
end

module Fun_enter_local_rule = struct
  type t = { x : Id.t; fb : Tracelet.fun_block; is_local : bool }
end

module Fun_enter_nonlocal_rule = struct
  type t = { x : Id.t; fb : Tracelet.fun_block; is_local : bool }
end

module Fun_exit_rule = struct
  type t = { x : Id.t; xf : Id.t; fids : Id.t list }
end

type t =
  | Discovery_main of Discovery_main_rule.t
  | Discovery_nonmain of Discovery_nonmain_rule.t
  | Input of Input_rule.t
  | Alias of Alias_rule.t
  | Binop of Binop_rule.t
  | Cond_top of Cond_top_rule.t
  | Cond_btm of Cond_btm_rule.t
  | Fun_enter_local of Fun_enter_local_rule.t
  | Fun_enter_nonlocal of Fun_enter_nonlocal_rule.t
  | Fun_exit of Fun_exit_rule.t
  | Record_start of Record_start_rule.t
  | Record_end of Record_end_rule.t
  | Mismatch

let rule_of_runtime_status x block : t =
  let open Tracelet in
  match (clause_of_x block x, block) with
  | Some tc, _ -> (
      match tc with
      | { clause = Clause (_, Input_body); _ } ->
          let is_in_main =
            Ident.equal (Tracelet.id_of_block block) Tracelet.id_main
          in
          Input { x; is_in_main }
      | { clause = Clause (_, Var_body (Var (x', _))); _ } -> Alias { x; x' }
      | { clause = Clause (_, Value_body (Value_record r)); _ } ->
          Record_end { x; r }
      | { clause = Clause (_, Value_body v); _ } ->
          if Ident.equal (Tracelet.id_of_block block) Tracelet.id_main
          then Discovery_main { x; v }
          else Discovery_nonmain { x; v }
      | { clause = Clause (_, Projection_body (Var (r, _), lbl)); _ } ->
          Record_start { x; r; lbl }
      | {
       clause = Clause (_, Binary_operation_body (Var (x1, _), bop, Var (x2, _)));
       _;
      } ->
          Binop { x; bop; x1; x2 }
      | {
       clause = Clause (_, Conditional_body (Var (x', _), _, _));
       id = tid;
       _;
      } ->
          Cond_btm { x; x'; tid }
      | {
       clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
       cat = App fids;
       _;
      } ->
          Fun_exit { x; xf; fids }
      | _ ->
          Log.Export.LLog.err (fun m ->
              m "%a" Odefa_ast.Ast_pp.pp_clause tc.clause) ;
          failwith "should not mismatch here")
  | None, Fun fb ->
      if Ident.(equal fb.para x)
      then Fun_enter_local { x; fb; is_local = true }
      else Fun_enter_nonlocal { x; fb; is_local = false }
  | None, Cond cb -> Cond_top cb
  | None, Main _mb -> Mismatch

let show_rule : t -> string = function
  | Discovery_main _ -> "Discovery_main"
  | Discovery_nonmain _ -> "Discovery_nonmain"
  | Input _ -> "Input"
  | Alias _ -> "Alias"
  | Binop _ -> "Binop"
  | Cond_top _ -> "Cond_top"
  | Cond_btm _ -> "Cond_btm"
  | Fun_enter_local _ -> "Fun_enter_local"
  | Fun_enter_nonlocal _ -> "Fun_enter_nonlocal"
  | Fun_exit _ -> "Fun_exit"
  | Record_start _ -> "Record_start"
  | Record_end _ -> "Record_end"
  | Mismatch -> "Mismatch"

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
