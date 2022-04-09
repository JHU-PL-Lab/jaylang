open Core
open Odefa_ast.Ast
open Odefa_ast.Ast_pp

type discovery_main_rule = DM of { x : Id.t; v : value }
type discovery_nonmain_rule = DN of { x : Id.t; v : value }
type discard_rule = D of { x : Id.t; v : value }
type input_rule = I of { x : Id.t }
type alias_rule = A of { x : Id.t; x' : Id.t }

type binop_rule =
  | B of { x : Id.t; bop : binary_operator; x1 : Id.t; x2 : Id.t }

type cond_top_rule = CT of Tracelet.cond_block
type cond_btm_rule = CB of { x : Id.t; x' : Id.t; tid : Id.t }

type fun_enter_local_rule =
  | FEL of { x : Id.t; fb : Tracelet.fun_block; is_local : bool }

type fun_enter_nonlocal_rule =
  | FEN of { x : Id.t; fb : Tracelet.fun_block; is_local : bool }

type fun_exit_rule = FE of { x : Id.t; xf : Id.t; fids : Id.t list }
type record_start_rule = RS of { x : Id.t; r : Id.t; lbl : Id.t }
type record_end_rule = RE of { x : Id.t; v : value }

type t =
  | Discovery_main of discovery_main_rule
  | Discovery_nonmain of discovery_nonmain_rule
  | Discard of discard_rule
  | Input of input_rule
  | Alias of alias_rule
  | Binop of binop_rule
  | Cond_top of cond_top_rule
  | Cond_btm of cond_btm_rule
  | Fun_enter_local of fun_enter_local_rule
  | Fun_enter_nonlocal of fun_enter_nonlocal_rule
  | Fun_exit of fun_exit_rule
  | Record_start of record_start_rule
  | Record_end of record_end_rule
  | Mismatch

let rule_of_runtime_status x xs block : t =
  let open Tracelet in
  match (clause_of_x block x, block) with
  | Some tc, _ -> (
      match tc with
      | { clause = Clause (_, Value_body v); _ } when List.is_empty xs ->
          if Ident.equal (Tracelet.id_of_block block) Tracelet.id_main
          then Discovery_main (DM { x; v })
          else Discovery_nonmain (DN { x; v })
      | { clause = Clause (_, Value_body (Value_function _ as v)); _ }
        when not (List.is_empty xs) ->
          Discard (D { x; v })
      | { clause = Clause (_, Value_body (Value_record _r as v)); _ }
        when not (List.is_empty xs) ->
          Record_end (RE { x; v })
      | { clause = Clause (_, Value_body _); _ } when not (List.is_empty xs) ->
          Mismatch
      | { clause = Clause (_, Input_body); _ } -> Input (I { x })
      | { clause = Clause (_, Var_body (Var (x', _))); _ } ->
          Alias (A { x; x' })
      | { clause = Clause (_, Projection_body (Var (r, _), lbl)); _ } ->
          Record_start (RS { x; r; lbl })
      | {
       clause = Clause (_, Binary_operation_body (Var (x1, _), bop, Var (x2, _)));
       _;
      } ->
          Binop (B { x; bop; x1; x2 })
      | {
       clause = Clause (_, Conditional_body (Var (x', _), _, _));
       id = tid;
       _;
      } ->
          Cond_btm (CB { x; x'; tid })
      | {
       clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
       cat = App fids;
       _;
      } ->
          Fun_exit (FE { x; xf; fids })
      | _ ->
          Log.Export.LLog.err (fun m ->
              m "%a" Odefa_ast.Ast_pp.pp_clause tc.clause) ;
          failwith "should not mismatch here")
  | None, Fun fb ->
      if Ident.(equal fb.para x)
      then Fun_enter_local (FEL { x; fb; is_local = true })
      else Fun_enter_nonlocal (FEN { x; fb; is_local = false })
  | None, Cond cb -> Cond_top (CT cb)
  | None, Main _mb -> Mismatch

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
