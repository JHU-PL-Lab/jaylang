open Core
open Dj_common
open Jayil
open Jayil.Ast
open Log.Export
open Rule
module U = Unrolls.U_ddse

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> Phi_set.t -> Phi_set.t
  val block_map : Cfg.block Jayil.Ast.Ident_map.t
  val unroll : U.t
end

module Make (S : S) = struct
  let return_with this_key (t : Ddse_result.t) =
    let phi = Riddler.eq this_key t.v in
    Ddse_result.add_phi phi t

  let return_with_phis this_key phis (this_t : Ddse_result.t)
      (t : Ddse_result.t) =
    let phi = Riddler.eq this_key t.v in
    Ddse_result.(merge_all t this_t (Phi_set.of_list (phi :: phis)) [] [])

  let return_with_phis_with_choices this_key phis choices
      (this_t : Ddse_result.t) (t : Ddse_result.t) =
    let phi = Riddler.eq this_key t.v in
    Ddse_result.(merge_all t this_t (Phi_set.of_list (phi :: phis)) choices [])

  let return_phis_with_beta this_key phis choice_betas (this_t : Ddse_result.t)
      (t : Ddse_result.t) =
    let phi = Riddler.eq this_key t.v in
    Ddse_result.(
      merge_all t this_t (Phi_set.of_list (phi :: phis)) [] choice_betas)

  let return_phis outer_term phis choices choice_betas (outer_t : Ddse_result.t)
      (t : Ddse_result.t) =
    let phi = Riddler.eq outer_term t.v in
    Ddse_result.(
      merge_all t outer_t (Phi_set.of_list (phi :: phis)) choices choice_betas)

  let rule_main v (key : Lookup_key.t) _phis =
    let target_stk = Rstack.concretize_top key.r_stk in
    let phi = Riddler.discover_main key v in
    let phis = S.add_phi key phi Phi_set.empty in
    U.by_return S.unroll key (Ddse_result.of3 key phis target_stk)

  let rule_nonmain v key phis_top run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    run_task key_first phis_top ;
    let phi = Riddler.eq_term_v key v in
    let _ = S.add_phi key phi phis_top in
    U.by_map_u S.unroll key key_first (Ddse_result.with_v_and_phi key phi)

  let discovery_main p key phis =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) key phis

  let discovery_nonmain p key phis run_task =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) key phis run_task

  let input p this_key phis run_task =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes this_key ;
    if is_in_main
    then rule_main None this_key phis
    else rule_nonmain None this_key phis run_task

  let alias p key phis_top run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key_rx = Lookup_key.with_x key x' in
    run_task key_rx phis_top ;

    U.by_map_u S.unroll key key_rx (return_with key)

  let not_ _p _key _phis_top _run_task = ()

  let binop b key phis_top run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let key_x2 = Lookup_key.with_x key x2 in
    run_task key_x1 phis_top ;
    run_task key_x2 phis_top ;

    let cb ((t1 : Ddse_result.t), (t2 : Ddse_result.t)) =
      Ddse_result.(merge_with_v key bop t1 t2)
    in

    U.by_filter_map2_u S.unroll key key_x1 key_x2 cb

  let record_start p key phis_top run_task =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in

    run_task key_r phis_top ;

    let cb this_key (rv : Ddse_result.t) =
      let rv_block = Cfg.block_of_id rv.v.x S.block_map in
      let phi1 = Riddler.eq key_r rv.v in
      let clause_body = Cfg.clause_body_of_x rv_block rv.v.x in
      let rvv = Ast_tools.record_of_clause_body clause_body in
      (match Ident_map.Exceptionless.find lbl rvv with
      | Some (Var (field, _)) ->
          let key_l = Lookup_key.with_x rv.v field in
          run_task key_l phis_top ;
          U.by_filter_map_u S.unroll this_key key_l
            (return_with_phis this_key [ phi1 ] rv)
      | None -> ()) ;

      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_r cb

  let cond_top (cb : Cond_top_rule.t) key phis_top run_task =
    let condsite_block = Cfg.outer_block key.Lookup_key.block S.block_map in
    let choice = Option.value_exn cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop key.r_stk (cb.point, Id.cond_fid choice)
    in
    let x2 = cb.cond in

    let key_x2 = Lookup_key.of3 x2 condsite_stack condsite_block in
    let key_x = Lookup_key.of3 key.x condsite_stack condsite_block in

    let beta = Option.value_exn cb.choice in
    run_task key_x2 phis_top ;

    let cb key (rc : Ddse_result.t) =
      let phi_c = Riddler.eqv rc.v (Value_bool beta) in
      let phis_top_with_c = Phi_set.(add (union rc.phis phis_top) phi_c) in
      (* (match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
         | Some _ -> *)
      run_task key_x phis_top_with_c ;
      let phi = Riddler.eqv key_x2 (Value_bool beta) in
      let choice_beta = (key_x2, beta) in
      U.by_filter_map_u S.unroll key key_x
        (return_phis_with_beta key [ phi ] [ choice_beta ] rc)
      (* | None -> ()) *) ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_x2 cb

  let cond_btm p (this_key : Lookup_key.t) phis_top run_task =
    let ({ x; x' } : Cond_btm_rule.t) = p in
    let cond_block = Ident_map.find x S.block_map |> Cfg.cast_to_cond_block in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.with_x this_key x' in

    (* Method 1 : lookup condition then lookup beta-case *)
    run_task term_c phis_top ;

    let cb (this_key : Lookup_key.t) (rc : Ddse_result.t) =
      List.iter [ true; false ] ~f:(fun beta ->
          let key_ret =
            Lookup_key.return_key_of_cond this_key S.block_map beta
          in

          let phi_beta = Riddler.eqv rc.v (Value_bool beta) in
          let phis_top_with_c =
            Phi_set.(add (union rc.phis phis_top) phi_beta)
          in
          (* match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
             | Some _ -> *)
          run_task key_ret phis_top_with_c ;

          (* Method 1-a: slow in looping *)
          (* let cb this_key (r_ret : Ddse_result.t) =
               U.by_map_u S.unroll this_key r_ret.v
                 (return_with_phis this_key
                    (Phi_set.add rc.phis phi_beta)
                    r_ret) ;
               Lwt.return_unit
             in
             U.by_bind_u S.unroll this_key key_ret cb) *)
          (* Method 1-b: slow in looping *)
          let choice_beta = (term_c, beta) in
          U.by_filter_map_u S.unroll this_key key_ret
            (return_phis_with_beta this_key [ phi_beta ] [ choice_beta ] rc))
      (* End of Method 1-a/b *)
      (* | None -> ()  *) ;

      Lwt.return_unit
    in

    U.by_bind_u S.unroll this_key term_c cb

  (* Method 1 End *)

  (* Method 2 : lookup two bools together, each *)
  (* List.iter [ true; false ] ~f:(fun beta ->
      let key_ret = Lookup_key.return_key_of_cond this_key S.block_map beta in
      run_task term_c phis_top ;

      let cb key_ret ((key_c : Lookup_key.t), phis_c) =
        let phi_beta = Riddler.eqv key_c (Value_bool beta) in
        (* match Riddler.check_phis (Phi_set.to_list phis_top') false with
           | Some _ -> *)
        run_task key_ret phis_top ;

        U.by_map_u S.unroll this_key key_ret
          (return_with_phis this_key (Phi_set.add phis_c phi_beta)) ;
        Lwt.return_unit
        (* | None -> () *)
      in
      U.by_bind_u S.unroll key_ret term_c cb) *)

  (* Method 2 End *)

  let fun_enter_local p (key : Lookup_key.t) phis_top run_task =
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites key.r_stk fb in
    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop key.r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              run_task key_f phis_top ;
              let key_arg = Lookup_key.of3 x''' callsite_stack callsite_block in
              let phi = Riddler.same_funenter key_f fid key key_arg in
              (* let _choice_this =
                   Decision.make r_stk Cfg.(id_of_block (Fun fb))
                 in *)
              let choice_f =
                Decision.make callsite_stack (Cfg.id_of_block callsite_block)
              in
              let cb key (rf : Ddse_result.t) =
                run_task key_arg phis_top ;
                let phi_f = Riddler.eq key_f rf.v in
                (* This function contains `key = key_arg.v` in the phis *)
                U.by_filter_map_u S.unroll key key_arg
                  (return_phis key [ phi; phi_f ] [ choice_f ] [] rf) ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll key key_f cb ;

              sub_trees
              (* @ [ (node_f, node_arg) ] *)
          | None -> failwith "why Rstack.pop fails here"
          (* sub_trees *))
        ~init:[]
    in
    ()

  let fun_enter_nonlocal p (this_key : Lookup_key.t) phis_top run_task =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites this_key.r_stk fb in

    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop this_key.r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              run_task key_f phis_top ;

              let phi = Riddler.eq_fid key_f fb.point in

              (* let choice_this =
                   Decision.make r_stk Cfg.(id_of_block (Fun fb))
                 in *)
              let choice_f =
                Decision.make callsite_stack (Cfg.id_of_block callsite_block)
              in

              let cb_f key (rf : Ddse_result.t) =
                let key_arg = Lookup_key.with_x rf.v this_key.x in
                let fv_block = Cfg.block_of_id rf.v.x S.block_map in
                run_task key_arg phis_top ;

                let phi_f = Riddler.eq key_f rf.v in
                U.by_filter_map_u S.unroll key key_arg
                  (return_phis key [ phi_f; phi ] [ choice_f ] [] rf) ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll this_key key_f cb_f ;

              sub_trees
              (* @ [ (node_f, node_f) ] *)
          | None -> failwith "why Rstack.pop fails here"
          (* sub_trees *))
        ~init:[]
    in
    ()

  let fun_exit p (this_key : Lookup_key.t) phis_top run_task =
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.with_x this_key xf in
    let b_id = Cfg.id_of_block this_key.block in

    run_task key_f phis_top ;

    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_ret = Lookup_key.get_f_return S.block_map fid this_key in
          let phi = Riddler.same_funexit key_f fid key_ret this_key in
          let cb (key : Lookup_key.t) (rf : Ddse_result.t) =
            let fid' = rf.v.x in
            if Id.equal fid fid' (* if List.mem fids fid ~equal:Id.equal *)
            then (
              let phis_top' = Phi_set.union phis_top rf.phis in
              run_task key_ret phis_top' ;
              let choice_this = Decision.make key.r_stk b_id in
              let choice_f =
                Decision.make key_ret.r_stk (Cfg.id_of_block fblock)
              in
              U.by_filter_map_u S.unroll this_key key_ret
                (return_with_phis_with_choices this_key [ phi ]
                   [ choice_this; choice_f ] rf))
            else () ;
            Lwt.return_unit
          in
          U.by_bind_u S.unroll this_key key_f cb ;

          sub_trees)
        ~init:[]
    in
    ()

  let pattern _p _key _phis_top _run_task = ()
  let assume _p _key _phis_top _run_task = ()

  let assert_ _p this_key phis_top _run_task =
    let _phis' = S.add_phi this_key Riddler.false_ phis_top in
    ()

  let abort _p _key _phis_top _run_task = ()

  let mismatch this_key phis =
    let _phis' = S.add_phi this_key Riddler.false_ phis in
    ()
end
