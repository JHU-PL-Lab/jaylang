open Core
open Dj_common
open Jayil
open Jayil.Ast
open Log.Export
open Rule

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> Phi_set.t -> Phi_set.t
  val block_map : Cfg.block Jayil.Ast.Ident_map.t
  val unroll : Unrolls.U_ddse.t
  val run_task : Lookup_key.t -> Phi_set.t -> unit
end

module U = Unrolls.U_ddse

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

  let rule_main vo (key : Lookup_key.t) _phis =
    let target_stk = Rstack.concretize_top key.r_stk in
    let phi =
      Riddler.(
        and_
          [ Riddler.stack_in_main key.r_stk; eqz key (phi_of_value_opt key vo) ])
    in
    let phis = S.add_phi key phi Phi_set.empty in
    U.one_shot S.unroll key (Ddse_result.of3 key phis target_stk)

  let rule_nonmain vo key phis_top =
    let key_first = Lookup_key.to_first key S.state.info.first in
    S.run_task key_first phis_top ;
    let eq_phi = Riddler.eqz key (Riddler.phi_of_value_opt key vo) in
    let _ = S.add_phi key eq_phi phis_top in
    U.map S.unroll key_first key (Ddse_result.with_v_and_phi key eq_phi)

  let discovery_main p key phis =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) key phis

  let discovery_nonmain p key phis =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) key phis

  let input p key phis =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.search.input_nodes key ;
    if is_in_main then rule_main None key phis else rule_nonmain None key phis

  let alias p key phis_top =
    let ({ x' } : Alias_rule.t) = p in
    S.run_task x' phis_top ;

    U.map S.unroll x' key (return_with key)

  let not_ _p _key _phis_top = ()

  let binop b key phis_top =
    let ({ bop; x1; x2 } : Binop_rule.t) = b in
    S.run_task x1 phis_top ;
    S.run_task x2 phis_top ;

    let cb ((t1 : Ddse_result.t), (t2 : Ddse_result.t)) =
      Ddse_result.(merge_with_v key bop t1 t2)
    in

    U.filter_map2 S.unroll x1 x2 key cb

  let record_start p key phis_top =
    let ({ r; lbl } : Record_start_rule.t) = p in

    S.run_task r phis_top ;

    let cb this_key (rv : Ddse_result.t) =
      let rv_block = rv.v.block in
      let phi1 = Riddler.eq r rv.v in
      let clause_body = Cfg.clause_body_of_x rv_block rv.v.x in
      let rvv = Ast_tools.record_of_clause_body clause_body in
      match Ident_map.Exceptionless.find lbl rvv with
      | Some (Var (field, _)) ->
          let key_l = Lookup_key.with_x rv.v field in
          S.run_task key_l phis_top ;
          U.filter_map S.unroll key_l this_key
            (return_with_phis this_key [ phi1 ] rv)
      | None -> ()
    in
    U.iter S.unroll r (cb key)

  let cond_top p (key : Lookup_key.t) phis_top =
    let ({ cond_case_info = cb; condsite_block; _ } : Cond_top_rule.t) = p in
    let beta = cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop key.r_stk (cb.condsite, Id.cond_id beta)
    in
    let x2 = cb.cond in

    let key_x2 = Lookup_key.of3 x2 condsite_stack condsite_block in
    let key_x = Lookup_key.of3 key.x condsite_stack condsite_block in

    S.run_task key_x2 phis_top ;

    let cb key (rc : Ddse_result.t) =
      let phi_c = Riddler.(eqz rc.v (bool_ beta)) in
      let phis_top_with_c = Phi_set.(add (union rc.phis phis_top) phi_c) in
      (* (match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
         | Some _ -> *)
      S.run_task key_x phis_top_with_c ;
      let phi = Riddler.(eqz key_x2 (bool_ beta)) in
      let choice_beta = (key_x2, beta) in
      U.filter_map S.unroll key_x key
        (return_phis_with_beta key [ phi ] [ choice_beta ] rc)
      (* | None -> ()) *)
    in
    U.iter S.unroll key_x2 (cb key)

  let cond_btm p (this_key : Lookup_key.t) phis_top =
    let ({ x'; rets; _ } : Cond_btm_rule.t) = p in

    (* Method 1 : lookup condition then lookup beta-case *)
    S.run_task x' phis_top ;

    let cb (this_key : Lookup_key.t) (rc : Ddse_result.t) =
      List.iter rets ~f:(fun (beta, key_ret) ->
          let phi_beta = Riddler.(eqz rc.v (bool_ beta)) in
          let phis_top_with_c =
            Phi_set.(add (union rc.phis phis_top) phi_beta)
          in
          (* match Riddler.check_phis (Phi_set.to_list phis_top_with_c) false with
             | Some _ -> *)
          S.run_task key_ret phis_top_with_c ;

          (* Method 1-a: slow in looping *)
          (* let cb this_key (r_ret : Ddse_result.t) =
               U.map S.unroll  r_ret.v this_key
                 (return_with_phis this_key
                    (Phi_set.add rc.phis phi_beta)
                    r_ret) ;
               Lwt.return_unit
             in
             U.iter S.unroll  key_ret (cb this_key)) *)
          (* Method 1-b: slow in looping *)
          let choice_beta = (x', beta) in
          U.filter_map S.unroll key_ret this_key
            (return_phis_with_beta this_key [ phi_beta ] [ choice_beta ] rc))
    in

    U.iter S.unroll x' (cb this_key)

  (* Method 1 End *)

  (* Method 2 : lookup two bools together, each *)
  (* List.iter [ true; false ] ~f:(fun beta ->
      let key_ret = Lookup_key.return_key_of_cond this_key S.block_map beta in
      S.run_task term_c phis_top ;

      let cb key_ret ((key_c : Lookup_key.t), phis_c) =
        let phi_beta = Riddler.(eqz key_c (bool_ beta)) in
        (* match Riddler.check_phis (Phi_set.to_list phis_top') false with
           | Some _ -> *)
        S.run_task key_ret phis_top ;

        U.map S.unroll  key_ret this_key
          (return_with_phis this_key (Phi_set.add phis_c phi_beta)) ;
        Lwt.return_unit
        (* | None -> () *)
      in
      U.iter S.unroll term_c  (cb key_ret)) *)

  (* Method 2 End *)

  let fun_enter_local p (key : Lookup_key.t) phis_top =
    let ({ fb; fid; callsites_with_stk; _ } : Fun_enter_local_rule.t) = p in
    let sub_trees =
      List.fold callsites_with_stk
        ~f:(fun sub_trees (key_f, key_arg) ->
          S.run_task key_f phis_top ;
          let phi =
            Riddler.(and_ [ eqz key_f (z_of_fid fid); eq key key_arg ])
          in
          let choice_f = Decision.make key_f.r_stk key_f.block.id in
          let cb key (rf : Ddse_result.t) =
            S.run_task key_arg phis_top ;
            let phi_f = Riddler.eq key_f rf.v in
            (* This function contains `key = key_arg.v` in the phis *)
            U.filter_map S.unroll key_arg key
              (return_phis key [ phi; phi_f ] [ choice_f ] [] rf)
          in
          U.iter S.unroll key_f (cb key) ;

          sub_trees
          (* @ [ (node_f, node_arg) ] *))
        ~init:[]
    in
    ()

  let fun_enter_nonlocal p (this_key : Lookup_key.t) phis_top =
    let ({ fb; fid; callsites_with_stk; _ } : Fun_enter_nonlocal_rule.t) = p in

    let sub_trees =
      List.fold callsites_with_stk
        ~f:(fun sub_trees (key_f, _key_arg) ->
          S.run_task key_f phis_top ;
          let phi = Riddler.(eqz key_f (z_of_fid fid)) in
          (* let choice_this = Decision.make r_stk this_key.block.id in *)
          let choice_f = Decision.make key_f.r_stk key_f.block.id in

          let cb_f key (rf : Ddse_result.t) =
            let key_arg = Lookup_key.with_x rf.v this_key.x in
            let fv_block = Cfg.find_reachable_block rf.v.x S.block_map in
            S.run_task key_arg phis_top ;

            let phi_f = Riddler.eq key_f rf.v in
            U.filter_map S.unroll key_arg key
              (return_phis key [ phi_f; phi ] [ choice_f ] [] rf)
          in
          U.iter S.unroll key_f (cb_f this_key) ;

          sub_trees
          (* @ [ (node_f, node_f) ] *))
        ~init:[]
    in
    ()

  let fun_exit p (this_key : Lookup_key.t) phis_top =
    let ({ xf; fids } : Fun_exit_rule.t) = p in
    let b_id = this_key.block.id in

    S.run_task xf phis_top ;

    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_ret = Lookup_key.get_f_return S.block_map fid this_key in
          let phi =
            Riddler.(and_ [ eqz xf (z_of_fid fid); eq key_ret this_key ])
          in
          let cb (key : Lookup_key.t) (rf : Ddse_result.t) =
            let fid' = rf.v.x in
            if Id.equal fid fid' (* if List.mem fids fid ~equal:Id.equal *)
            then (
              let phis_top' = Phi_set.union phis_top rf.phis in
              S.run_task key_ret phis_top' ;
              let choice_this = Decision.make key.r_stk b_id in
              let choice_f = Decision.make key_ret.r_stk fblock.id in
              U.filter_map S.unroll key_ret this_key
                (return_with_phis_with_choices this_key [ phi ]
                   [ choice_this; choice_f ] rf))
          in
          U.iter S.unroll xf (cb this_key) ;

          sub_trees)
        ~init:[]
    in
    ()

  let pattern _p _key _phis_top = ()
  let assume _p _key _phis_top = ()

  let assert_ _p this_key phis_top =
    let _phis' = S.add_phi this_key Riddler.false_ phis_top in
    ()

  let abort _p _key _phis_top = ()

  let mismatch this_key phis =
    let _phis' = S.add_phi this_key Riddler.false_ phis in
    ()
end
