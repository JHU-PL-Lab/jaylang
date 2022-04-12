open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule

module Unroll_S :
  Unroll.S_sig
    with type message = Lookup_key.t * Phi.set
     and type result = Lookup_key.t * Phi.set = struct
  type message = Lookup_key.t * Phi.set
  type result = Lookup_key.t * Phi.set
  type key = Lookup_key.t

  let equal_message (k1, phis1) (k2, phis2) =
    let r1 = Lookup_key.compare k1 k2 in
    let r2 = if r1 = 0 then Set.compare_direct phis1 phis2 else r1 in
    r2 = 0
end

module U = Unroll.Make (Lookup_key) (Unroll_S)

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> Set.M(Phi).t -> Set.M(Phi).t

  val find_or_add_node :
    Lookup_key.t -> Tracelet.block -> Node.ref_t -> Node.ref_t

  val block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t
  val unroll : U.t
end

module Make (S : S) = struct
  let cb_add phi (key, phis) = (key, Set.add phis phi)

  let rule_main v (key : Lookup_key.t) this_node _phis =
    let target_stk = Rstack.concretize_top key.r_stk in
    Node.update_rule this_node (Node.done_ target_stk) ;
    let phi = Riddler.discover_main key v in
    let phis = S.add_phi key phi Phi.empty_set in
    let result_pusher = U.push_if_new S.unroll key in
    result_pusher (key, phis)

  let rule_nonmain v (key : Lookup_key.t) this_node block phis_top run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    let node_child = S.find_or_add_node key_first block this_node in
    Node.update_rule this_node (Node.to_first node_child) ;
    let phi = Riddler.discover_non_main key S.state.first v in
    let phis_top' = S.add_phi key phi phis_top in
    run_task key_first block phis_top' ;

    U.by_map_u S.unroll key key_first (cb_add phi)

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

  let discard p key this_node block phis_top run_task =
    let ({ v; _ } : Discard_rule.t) = p in
    let result_pusher = U.push_if_new S.unroll key in
    let key_drop_x = Lookup_key.drop_x key in
    let node_sub = S.find_or_add_node key_drop_x block this_node in
    Node.update_rule this_node (Node.discard node_sub) ;
    let phi = Riddler.discard key (Some v) in
    let phis_top' = S.add_phi key phi phis_top in
    run_task key_drop_x block phis_top' ;

    U.by_map_u S.unroll key key_drop_x (cb_add phi)

  let alias p (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key_rx = Lookup_key.replace_x key x' in
    let node_rx = S.find_or_add_node key_rx block this_node in
    Node.update_rule this_node (Node.alias node_rx) ;
    let phi = Riddler.alias key x' in
    let phis_top' = S.add_phi key phi phis_top in
    run_task key_rx block phis_top' ;
    U.by_map_u S.unroll key key_rx (cb_add phi)

  let binop b (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.of_parts x1 key.xs key.r_stk in
    let node_x1 = S.find_or_add_node key_x1 block this_node in
    let key_x2 = Lookup_key.of_parts x2 key.xs key.r_stk in
    let node_x2 = S.find_or_add_node key_x2 block this_node in
    Node.update_rule this_node (Node.binop node_x1 node_x2) ;

    let phi = Riddler.binop key bop x1 x2 in

    let phis_top' = S.add_phi key phi phis_top in

    run_task key_x1 block phis_top' ;
    run_task key_x2 block phis_top' ;

    let cb ((_, phis1), (_, phis2)) =
      let phis = Set.(add (union phis1 phis2) phi) in
      (key, phis)
    in
    U.by_map2_u S.unroll key key_x1 key_x2 cb

  let record_start p (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ x; r; lbl } : Record_start_rule.t) = p in
    let key_r = Lookup_key.replace_x key x in
    let node_r = S.find_or_add_node key_r block this_node in
    let key_r_l = Lookup_key.replace_x2 key (r, lbl) in
    let node_r_l = S.find_or_add_node key_r_l block this_node in

    Node.update_rule this_node (Node.project node_r node_r_l) ;
    let phi = Riddler.alias_key key key_r_l in
    let phis_top' = S.add_phi key phi phis_top in

    run_task key_r_l block phis_top' ;
    U.by_map_u S.unroll key key_r_l (cb_add phi)

  let record_end p (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ r; _ } : Record_end_rule.t) = p in
    let (Record_value rmap) = r in
    let _x, xs, r_stk = Lookup_key.to_parts key in
    let labal, xs' = (List.hd_exn xs, List.tl_exn xs) in
    match Ident_map.Exceptionless.find labal rmap with
    | Some (Var (vid, _)) ->
        let key' = Lookup_key.of_parts2 (vid :: xs') r_stk in
        let node_key = S.find_or_add_node key' block this_node in
        Node.update_rule this_node (Node.alias node_key) ;
        let phi = Riddler.alias_key key key' in
        let phis_top' = S.add_phi key phi phis_top in

        run_task key' block phis_top' ;
        U.by_map_u S.unroll key key' (cb_add phi)
    | None ->
        Node.update_rule this_node Node.mismatch ;
        ignore @@ S.add_phi key (Riddler.mismatch key) phis_top

  let cond_top (cb : Cond_top_rule.t) (key : Lookup_key.t) this_node block
      phis_top run_task =
    let condsite_block = Tracelet.outer_block block S.block_map in
    let x, xs, r_stk = Lookup_key.to_parts key in
    let choice = Option.value_exn cb.choice in
    let condsite_stack =
      match Rstack.pop r_stk (cb.point, Id.cond_fid choice) with
      | Some stk -> stk
      | None -> failwith "impossible in CondTop"
    in
    let x2 = cb.cond in

    let key_x2 = Lookup_key.of_parts x2 [] condsite_stack in
    let node_x2 = S.find_or_add_node key_x2 condsite_block this_node in
    let key_xxs = Lookup_key.of_parts x xs condsite_stack in
    let node_xxs = S.find_or_add_node key_xxs condsite_block this_node in

    Node.update_rule this_node (Node.cond_choice node_x2 node_xxs) ;
    let phi = Riddler.cond_top key cb condsite_stack in

    let phis_top' = S.add_phi key phi phis_top in
    run_task key_x2 condsite_block phis_top' ;

    let cb key rc =
      let _key_c, phis_c = rc in
      let phis_top' = Set.union phis_c phis_top in
      run_task key_xxs condsite_block phis_top' ;
      U.by_map_u S.unroll key key_xxs (cb_add phi) ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_x2 cb

  let cond_btm p (key : Lookup_key.t) this_node block phis_top run_task =
    let _x, xs, r_stk = Lookup_key.to_parts key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block =
      Ident_map.find tid S.block_map |> Tracelet.cast_to_cond_block
    in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let key_cond_var = Lookup_key.of_parts x' [] r_stk in
    let cond_var_tree = S.find_or_add_node key_cond_var block this_node in

    let sub_trees =
      List.fold [ true; false ]
        ~f:(fun sub_trees beta ->
          let ctracelet, key_x_ret =
            Lookup_key.get_cond_block_and_return cond_block beta r_stk x xs
          in
          let node_x_ret = S.find_or_add_node key_x_ret ctracelet this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees) ;

    let phi = Riddler.cond_bottom key cond_block x' in
    let phis_top' = S.add_phi key phi phis_top in

    run_task key_cond_var block phis_top' ;

    let cb this_key rc =
      let _, phis_c = rc in
      List.iter [ true; false ] ~f:(fun beta ->
          let phis_eager =
            Set.add phis_c (Riddler.bind_x_v [ x' ] r_stk (Value_bool beta))
          in
          match Riddler.check_phis (Set.to_list phis_eager) false with
          | Some _ ->
              let ctracelet, key_x_ret =
                Lookup_key.get_cond_block_and_return cond_block beta r_stk x xs
              in
              run_task key_x_ret ctracelet phis_eager ;
              U.by_map_u S.unroll this_key key_x_ret (cb_add phi)
          | None -> ()) ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_cond_var cb

  let fun_enter (fb : Tracelet.fun_block) is_local key this_node phis_top
      run_task =
    let x, xs, r_stk = Lookup_key.to_parts key in
    let fid = fb.point in
    let callsites =
      match Rstack.paired_callsite r_stk fid with
      | Some callsite -> [ callsite ]
      | None -> fb.callsites
    in
    let phi = Riddler.fun_enter key is_local fb callsites S.block_map in
    let phis' = S.add_phi key phi phis_top in

    let sub_trees, lookups =
      List.fold callsites
        ~f:(fun (sub_trees, lookups) callsite ->
          let callsite_block, x', x'', x''' =
            Tracelet.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of_parts x'' [] callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              let _lookup_f = run_task key_f callsite_block phis' in
              let key_arg =
                if is_local
                then Lookup_key.of_parts x''' xs callsite_stack
                else Lookup_key.of_parts x'' (x :: xs) callsite_stack
              in
              let node_arg =
                S.find_or_add_node key_arg callsite_block this_node
              in
              let _lookup_f_arg = run_task key_arg callsite_block phis' in
              (sub_trees @ [ (node_f, node_arg) ], lookups @ [ key_arg ])
          | None -> (sub_trees, lookups))
        ~init:([], [])
    in

    Node.update_rule this_node (Node.mk_para ~sub_trees) ;
    (* let lookups = [] in *)
    (* Lwt.dont_wait
       (fun () -> U.by_join_map S.unroll key lookups (cb_add phi))
       (fun _ -> LLog.app (fun m -> m "catched")) *)
    List.iter lookups ~f:(fun lookup ->
        U.by_map_u S.unroll key lookup (cb_add phi))

  let fun_enter_local p this_key this_node phis run_task =
    let ({ fb; is_local; _ } : Fun_enter_local_rule.t) = p in
    fun_enter fb is_local this_key this_node phis run_task

  let fun_enter_nonlocal p this_key this_node phis run_task =
    let ({ fb; is_local; _ } : Fun_enter_nonlocal_rule.t) = p in
    fun_enter fb is_local this_key this_node phis run_task

  let fun_exit p this_key this_node block phis_top run_task =
    let _x, xs, r_stk = Lookup_key.to_parts this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    LLog.debug (fun m -> m "FunExit: %a -> %a" Id.pp xf Id.pp_list fids) ;
    let key_fun = Lookup_key.of_parts xf [] r_stk in
    let node_fun = S.find_or_add_node key_fun block this_node in
    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_x_ret = Lookup_key.get_f_return S.block_map fid r_stk x xs in
          let node_x_ret = S.find_or_add_node key_x_ret fblock this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_callsite ~fun_tree:node_fun ~sub_trees) ;
    let phi = Riddler.fun_exit this_key xf fids S.block_map in
    let phis_top' = S.add_phi this_key phi phis_top in
    run_task key_fun block phis_top' ;

    let cb this_key rf =
      let (key_f : Lookup_key.t), phis_f = rf in
      let fid = key_f.x in
      if List.mem fids fid ~equal:Id.equal
      then (
        let fblock = Ident_map.find fid S.block_map in
        let key_x_ret = Lookup_key.get_f_return S.block_map fid r_stk x xs in
        let phis_top'' = Set.union phis_top' phis_f in
        run_task key_x_ret fblock phis_top'' ;
        U.by_map_u S.unroll this_key key_x_ret (cb_add phi))
      else () ;
      Lwt.return_unit
    in

    U.by_bind_u S.unroll this_key key_fun cb

  let mismatch this_key this_node phis =
    Node.update_rule this_node Node.mismatch ;
    let _phis' = S.add_phi this_key (Riddler.mismatch this_key) phis in
    ()
end
