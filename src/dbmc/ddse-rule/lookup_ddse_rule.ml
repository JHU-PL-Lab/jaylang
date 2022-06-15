open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule
module U = Unrolls.U_ddse

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> Phi_set.t -> Phi_set.t
  val find_or_add_node : Lookup_key.t -> Cfg.block -> Node.ref_t -> Node.ref_t
  val block_map : Cfg.block Odefa_ast.Ast.Ident_map.t
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

  let rule_main v (key : Lookup_key.t) this_node _phis =
    let target_stk = Rstack.concretize_top key.r_stk in
    Node.update_rule this_node (Node.done_ target_stk) ;
    let phi = Riddler.discover_main key v in
    let phis = S.add_phi key phi Phi_set.empty in
    U.by_return S.unroll key (Ddse_result.of3 key phis target_stk)

  let rule_nonmain v key this_node block phis_top run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    let node_child = S.find_or_add_node key_first block this_node in
    Node.update_rule this_node (Node.to_first node_child) ;
    run_task key_first block phis_top ;
    let phi = Riddler.eq_term_v key v in
    let _ = S.add_phi key phi phis_top in
    U.by_map_u S.unroll key key_first (Ddse_result.with_v_and_phi key phi)

  let discovery_main p key this_node phis =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) key this_node phis

  let discovery_nonmain p key this_node block phis run_task =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) key this_node block phis run_task

  let input p this_key this_node block phis run_task =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes this_key ;
    if is_in_main
    then rule_main None this_key this_node phis
    else rule_nonmain None this_key this_node block phis run_task

  let alias p key this_node block phis_top run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key_rx = Lookup_key.with_x key x' in
    let node_rx = S.find_or_add_node key_rx block this_node in
    Node.update_rule this_node (Node.alias node_rx) ;
    run_task key_rx block phis_top ;

    (* let _ = S.add_phi key phi phis_top in *)
    U.by_map_u S.unroll key key_rx (return_with key)

  let binop b key this_node block phis_top run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let node_x1 = S.find_or_add_node key_x1 block this_node in
    let key_x2 = Lookup_key.with_x key x2 in
    let node_x2 = S.find_or_add_node key_x2 block this_node in
    Node.update_rule this_node (Node.binop node_x1 node_x2) ;
    run_task key_x1 block phis_top ;
    run_task key_x2 block phis_top ;

    (* let phi = Riddler.binop key bop key_x1 key_x2 in *)
    (* let _ = S.add_phi key phi phis_top in *)
    let cb ((t1 : Ddse_result.t), (t2 : Ddse_result.t)) =
      Ddse_result.(merge_with_v key bop t1 t2)
    in

    U.by_filter_map2_u S.unroll key key_x1 key_x2 cb

  let record_start p key this_node block phis_top run_task =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in
    let node_r = S.find_or_add_node key_r block this_node in

    run_task key_r block phis_top ;

    let cb this_key (rv : Ddse_result.t) =
      let rv_block = Cfg.find_by_id rv.v.x S.block_map in
      let node_rv = S.find_or_add_node rv.v rv_block this_node in
      let phi1 = Riddler.eq key_r rv.v in
      let rvv = Cfg.record_of_id S.block_map rv.v.x in
      (match Ident_map.Exceptionless.find lbl rvv with
      | Some (Var (field, _)) ->
          let key_l = Lookup_key.with_x rv.v field in
          let node_l = S.find_or_add_node key_l rv_block this_node in
          Node.update_rule this_node (Node.project node_r node_l) ;

          run_task key_l rv_block phis_top ;
          U.by_filter_map_u S.unroll this_key key_l
            (return_with_phis this_key [ phi1 ] rv)
      | None -> ()) ;

      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_r cb

  let record_end p this_key this_node block phis run_task =
    let ({ r; is_in_main; _ } : Record_end_rule.t) = p in
    let rv = Some (Value_record r) in
    if is_in_main
    then rule_main rv this_key this_node phis
    else rule_nonmain rv this_key this_node block phis run_task

  let cond_top (cb : Cond_top_rule.t) key this_node block phis_top run_task =
    let condsite_block = Cfg.outer_block block S.block_map in
    let x, r_stk = Lookup_key.to2 key in
    let choice = Option.value_exn cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop r_stk (cb.point, Id.cond_fid choice)
    in
    let x2 = cb.cond in

    let key_x2 = Lookup_key.of2 x2 condsite_stack in
    let node_x2 = S.find_or_add_node key_x2 condsite_block this_node in
    let key_x = Lookup_key.of2 x condsite_stack in
    let node_x = S.find_or_add_node key_x condsite_block this_node in
    Node.update_rule this_node (Node.cond_choice node_x2 node_x) ;

    let beta = Option.value_exn cb.choice in
    (* let phis_top' = S.add_phi key phi phis_top in *)
    run_task key_x2 condsite_block phis_top ;

    let cb key (rc : Ddse_result.t) =
      let phi_c = Riddler.eqv rc.v (Value_bool beta) in
      let phis_top_with_c = Phi_set.(add (union rc.phis phis_top) phi_c) in
      (* (match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
         | Some _ -> *)
      run_task key_x condsite_block phis_top_with_c ;
      let phi = Riddler.eqv key_x2 (Value_bool beta) in
      let choice_beta = (key_x2, beta) in
      U.by_filter_map_u S.unroll key key_x
        (return_phis_with_beta key [ phi ] [ choice_beta ] rc)
      (* | None -> ()) *) ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_x2 cb

  let cond_btm p this_key this_node block phis_top run_task =
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block = Ident_map.find tid S.block_map |> Cfg.cast_to_cond_block in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.of2 x' r_stk in
    let cond_var_tree = S.find_or_add_node term_c block this_node in

    (* Method 1 : lookup condition then lookup beta-case *)
    let sub_trees =
      List.fold [ true; false ]
        ~f:(fun sub_trees beta ->
          let case_block, key_ret =
            Lookup_key.get_cond_block_and_return cond_block beta r_stk x
          in
          let node_x_ret = S.find_or_add_node key_ret case_block this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees) ;

    run_task term_c block phis_top ;

    let cb this_key (rc : Ddse_result.t) =
      List.iter [ true; false ] ~f:(fun beta ->
          let case_block, key_ret =
            Lookup_key.get_cond_block_and_return cond_block beta r_stk x
          in
          let phi_beta = Riddler.eqv rc.v (Value_bool beta) in
          let phis_top_with_c =
            Phi_set.(add (union rc.phis phis_top) phi_beta)
          in
          (* match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
             | Some _ -> *)
          run_task key_ret case_block phis_top_with_c ;

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
  (* let sub_trees =
       List.fold [ true; false ]
         ~f:(fun sub_trees beta ->
           let case_block, key_ret =
             Lookup_key.get_cond_block_and_return cond_block beta r_stk x
           in
           let node_x_ret = S.find_or_add_node key_ret case_block this_node in
           run_task term_c block phis_top ;

           let cb key_ret ((key_c : Lookup_key.t), phis_c) =
             let phi_beta = Riddler.eqv key_c (Value_bool beta) in
             (* match Riddler.check_phis (Phi_set.to_list phis_top') false with
                | Some _ -> *)
             run_task key_ret case_block phis_top ;

             U.by_map_u S.unroll this_key key_ret
               (return_with_phis this_key (Phi_set.add phis_c phi_beta)) ;
             Lwt.return_unit
             (* | None -> () *)
           in
           U.by_bind_u S.unroll key_ret term_c cb ;
           sub_trees @ [ node_x_ret ])
         ~init:[]
     in
     Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees) *)
  (* Method 2 End *)

  let fun_enter_local p key this_node phis_top run_task =
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let _x, r_stk = Lookup_key.to2 key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in

              run_task key_f callsite_block phis_top ;
              let key_arg = Lookup_key.of2 x''' callsite_stack in
              let phi = Riddler.same_funenter key_f fid key key_arg in
              let node_arg =
                S.find_or_add_node key_arg callsite_block this_node
              in
              (* let _choice_this =
                   Decision.make r_stk Cfg.(id_of_block (Fun fb))
                 in *)
              let choice_f =
                Decision.make callsite_stack (Cfg.id_of_block callsite_block)
              in
              let cb key (rf : Ddse_result.t) =
                run_task key_arg callsite_block phis_top ;
                let phi_f = Riddler.eq key_f rf.v in
                (* This function contains `key = key_arg.v` in the phis *)
                U.by_filter_map_u S.unroll key key_arg
                  (return_phis key [ phi; phi_f ] [ choice_f ] [] rf) ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll key key_f cb ;

              sub_trees @ [ (node_f, node_arg) ]
          | None -> failwith "why Rstack.pop fails here"
          (* sub_trees *))
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_para ~sub_trees)

  let fun_enter_nonlocal p key this_node phis_top run_task =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let x, r_stk = Lookup_key.to2 key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in

    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              run_task key_f callsite_block phis_top ;

              let phi = Riddler.eq_fid key_f fb.point in

              (* let choice_this =
                   Decision.make r_stk Cfg.(id_of_block (Fun fb))
                 in *)
              let choice_f =
                Decision.make callsite_stack (Cfg.id_of_block callsite_block)
              in

              let cb_f key (rf : Ddse_result.t) =
                let key_arg = Lookup_key.with_x rf.v x in
                let fv_block = Cfg.find_by_id rf.v.x S.block_map in
                let node_arg = S.find_or_add_node key_arg fv_block this_node in
                run_task key_arg fv_block phis_top ;

                let phi_f = Riddler.eq key_f rf.v in
                U.by_filter_map_u S.unroll key key_arg
                  (return_phis key [ phi_f; phi ] [ choice_f ] [] rf) ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll key key_f cb_f ;

              sub_trees @ [ (node_f, node_f) ]
          | None -> failwith "why Rstack.pop fails here"
          (* sub_trees *))
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_para ~sub_trees)

  let fun_exit p this_key this_node block phis_top run_task =
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of2 xf r_stk in
    let node_fun = S.find_or_add_node key_f block this_node in

    run_task key_f block phis_top ;

    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in
          let phi = Riddler.same_funexit key_f fid key_ret this_key in
          let node_x_ret = S.find_or_add_node key_ret fblock this_node in

          let cb this_key (rf : Ddse_result.t) =
            let fid' = rf.v.x in
            if Id.equal fid fid' (* if List.mem fids fid ~equal:Id.equal *)
            then (
              let phis_top' = Phi_set.union phis_top rf.phis in
              run_task key_ret fblock phis_top' ;
              let choice_this = Decision.make r_stk (Cfg.id_of_block block) in
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

          sub_trees @ [ node_x_ret ])
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_callsite ~fun_tree:node_fun ~sub_trees)

  let pattern _p _key _this_node _block _phis_top _run_task = ()
  let assume _p _key _this_node _block _phis_top _run_task = ()

  let assert_ _p this_key this_node _block phis_top _run_task =
    Node.update_rule this_node Node.mismatch ;
    let _phis' = S.add_phi this_key Riddler.false_ phis_top in
    ()

  let mismatch this_key this_node phis =
    Node.update_rule this_node Node.mismatch ;
    let _phis' = S.add_phi this_key Riddler.false_ phis in
    ()
end
