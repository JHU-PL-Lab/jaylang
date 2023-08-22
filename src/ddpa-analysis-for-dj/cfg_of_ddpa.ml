(* The file is for temporary use to break some dependency mess
   The code is moved out from `dj_common` becuase I don't want `dj_common` depends on `ddpa`. Now

   dj_common depends on languages jil, jay, and bluejay

   dbmc depends on ddpa-analysis-for-dj
   ddpa-analysis-for-dj depends on ddpa and dj_common
   ddpa depends on languages jil

   jil_analysis depends on dj_common and language jil

   Therefore, dj_common doesn't know about both ddpa and jil_analysis.
*)

open Core
open Jayil.Ast
open Ddpa
open Ddpa_abstract_ast
open Ddpa_graph
open Ddpa_helper
open Dj_common.Cfg

let make_cond_block_possible tl_map acls cfg =
  let cond_site, possible =
    match acls with
    | [
     Unannotated_clause (Abs_clause (Abs_var cond_site, Abs_conditional_body _));
     choice_clause;
    ] ->
        let choice = find_cond_choice choice_clause cfg in
        (cond_site, Some choice)
    | [
     Unannotated_clause (Abs_clause (Abs_var cond_site, Abs_conditional_body _));
     _clause1;
     _clause2;
    ] ->
        (cond_site, None)
    | _ -> failwith "wrong precondition to call"
  in

  let cond_both = find_cond_blocks cond_site !tl_map in
  match possible with
  | Some beta ->
      let beta_block =
        if beta
        then Option.value_exn cond_both.else_
        else Option.value_exn cond_both.then_
      in
      set_block_impossible tl_map beta_block
  | None -> ()

(* annotate block from the ddpa cfg.
   for call-site `s = e1 e2`, annotate e1 with the real function def_var
   for cond-site `s = c ? e1 : e2`, annotate c with the possible bools
*)

let block_map_of_expr e pt : block Ident_map.t =
  let map = ref (Dj_common.Cfg_of_source.block_map_of_expr e) in
  let set_map map' = map := map' in
  let cfg = Ddpa_analysis.cfg_of e in
  let ret_to_fun_def_map = Jayil.Ast_tools.make_ret_to_fun_def_mapping e in
  let para_to_fun_def_map = Jayil.Ast_tools.make_para_to_fun_def_mapping e in
  let id_to_clause_map = Jayil.Ast_tools.clause_mapping e in
  let acl =
    let pt_clause = Ident_map.find pt id_to_clause_map in
    (* let is_abort_clause =
         match pt_clause with Clause (_, Abort_body) -> true | _ -> false
       in *)
    Unannotated_clause (lift_clause pt_clause)
  in

  (* let debug_bomb = ref 20 in *)
  let visited = ref Annotated_clause_set.empty in
  let rec loop acl dangling : unit =
    if Annotated_clause_set.mem acl !visited
    then ()
    else (
      visited := Annotated_clause_set.add acl !visited ;
      let prev_acls = preds_l acl cfg in

      (* debug to prevent infinite loop *)
      (* debug_bomb := !debug_bomb - 1;
         if !debug_bomb = 0
         then failwith "bomb" ; *)

      (* process logic *)
      (* if cfg shows only one of then-block and else-block is possible,
         we can change the block accordingly.
         e.g. [prev: [r = c ? ...; r = r1 @- r]]
      *)
      if List.length prev_acls > 1 && has_condition_clause prev_acls
      then make_cond_block_possible map prev_acls cfg ;

      (* step logic *)
      let continue = ref true and block_dangling = ref dangling in
      (match acl with
      | Unannotated_clause _ | Start_clause _ | End_clause _ -> ()
      (* into fbody *)
      | Binding_exit_clause
          ( Abs_var _para,
            Abs_var ret_var,
            Abs_clause (Abs_var site_r, Abs_appl_body _) ) ->
          (* para can also be ignored in Fun since para is a property of a Fun block, defined in the source code *)
          let f_def = Ident_map.find ret_var ret_to_fun_def_map in
          set_map @@ add_id_dst !map site_r f_def ;
          block_dangling := false
      (* out of fbody *)
      | Binding_enter_clause
          (Abs_var para, _, Abs_clause (Abs_var site_r, Abs_appl_body _)) ->
          let f_def = Ident_map.find para para_to_fun_def_map in
          set_map @@ add_id_dst !map site_r f_def ;
          set_map @@ add_callsite !map f_def site_r ;

          continue := dangling
      (* into cond-body *)
      | Binding_exit_clause
          ( _,
            Abs_var _ret_var,
            Abs_clause (Abs_var _site_r, Abs_conditional_body _) ) ->
          block_dangling := false
      (* out of cond-body *)
      | Nonbinding_enter_clause
          ( Abs_value_bool _cond,
            Abs_clause
              ( Abs_var _site_r,
                Abs_conditional_body (Abs_var _x1, _e_then, _e_else) ) ) ->
          continue := dangling
      | Binding_exit_clause (_, _, _) ->
          failwith "impossible binding exit for non-sites"
      | Binding_enter_clause (_, _, _) ->
          failwith "impossible binding enter for non callsites"
      | Nonbinding_enter_clause (_, _) ->
          failwith "impossible non-binding enter for non condsites") ;
      if !continue
      then List.iter ~f:(fun acl -> loop acl !block_dangling) (preds_l acl cfg))
  in
  let succ_acls = succs_l acl cfg in
  List.iter ~f:(fun acl -> loop acl true) succ_acls ;
  (* loop acl true ; *)
  !map
