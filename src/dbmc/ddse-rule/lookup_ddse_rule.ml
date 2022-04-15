open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule

module Unroll_S :
  Unroll.S_sig
    with type message = Lookup_key.t * Phi_set.t
     and type result = Lookup_key.t * Phi_set.t = struct
  type message = Lookup_key.t * Phi_set.t
  type result = Lookup_key.t * Phi_set.t
  type key = Lookup_key.t

  let equal_message (k1, phis1) (k2, phis2) =
    Lookup_key.compare k1 k2 = 0
    (* && (Ast.equal_value v1 v2) *)
    && Phi_set.compare phis1 phis2 = 0
end

module U = Unroll.Make (Lookup_key) (Unroll_S)

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> Phi_set.t -> Phi_set.t

  val find_or_add_node :
    Lookup_key.t -> Tracelet.block -> Node.ref_t -> Node.ref_t

  val block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t
  val unroll : U.t
end

module Make (S : S) = struct
  let cb_phi_from_key this_key (key, phis) =
    let phi = Riddler_ddse.alias_key this_key key in
    (key, Phi_set.add phis phi)

  let cb_phi_from_key_and_phis this_key this_phis (key, phis) =
    let phi = Riddler_ddse.alias_key this_key key in
    (key, Phi_set.(add (union this_phis phis) phi))

  let cb_add phi (key, phis) = (key, Phi_set.add phis phi)
  let cb_merge phis1 (key2, phis2) = (key2, Phi_set.(union phis1 phis2))
  let cb_key phi key1 (_, phis2) = (key1, Phi_set.(add phis2 phi))

  let cb_merge_key phi key1 phis1 (_, phis2) =
    (key1, Phi_set.(add (union phis1 phis2) phi))

  let rule_main v (key : Lookup_key.t) this_node _phis =
    let target_stk = Rstack.concretize_top key.r_stk in
    Node.update_rule this_node (Node.done_ target_stk) ;
    let phi = Riddler_ddse.discover_main key v in
    let phis = S.add_phi key phi Phi_set.empty in
    let result_pusher = U.push_if_new S.unroll key in
    result_pusher (key, phis)

  let rule_nonmain v (key : Lookup_key.t) this_node block phis_top run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    let node_child = S.find_or_add_node key_first block this_node in
    Node.update_rule this_node (Node.to_first node_child) ;
    run_task key_first block phis_top ;
    let phi = Riddler_ddse.discover_non_main key S.state.first v in
    let _ = S.add_phi key phi phis_top in
    U.by_map_u S.unroll key key_first (cb_key phi key)

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
    run_task key_drop_x block phis_top ;

    let phi = Riddler_ddse.discard key (Some v) in
    let _ = S.add_phi key phi phis_top in
    U.by_map_u S.unroll key key_drop_x (cb_add phi)

  let alias p (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key_rx = Lookup_key.replace_x key x' in
    let node_rx = S.find_or_add_node key_rx block this_node in
    Node.update_rule this_node (Node.alias node_rx) ;
    run_task key_rx block phis_top ;

    (* let _ = S.add_phi key phi phis_top in *)
    U.by_map_u S.unroll key key_rx (cb_phi_from_key key)

  let binop b (key : Lookup_key.t) this_node block phis_top run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.of_parts x1 key.xs key.r_stk in
    let node_x1 = S.find_or_add_node key_x1 block this_node in
    let key_x2 = Lookup_key.of_parts x2 key.xs key.r_stk in
    let node_x2 = S.find_or_add_node key_x2 block this_node in
    Node.update_rule this_node (Node.binop node_x1 node_x2) ;
    run_task key_x1 block phis_top ;
    run_task key_x2 block phis_top ;

    let phi = Riddler_ddse.binop key bop x1 x2 in
    let _ = S.add_phi key phi phis_top in
    let cb ((_, phis1), (_, phis2)) =
      let phis = Phi_set.(add (union phis1 phis2) phi) in
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
    (* let phi = Riddler_ddse.alias_key key key_r_l in
       let phis_top' = S.add_phi key phi phis_top in *)
    run_task key_r_l block phis_top ;
    U.by_map_u S.unroll key key_r_l (cb_phi_from_key key)

  (* x = {l=t}, X=[x,l,...] --> X=[t,...] *)
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
        (* let phi = Riddler_ddse.alias_key key key' in *)
        (* let phis_top' = S.add_phi key phi phis_top in *)
        run_task key' block phis_top ;
        U.by_map_u S.unroll key key' (cb_phi_from_key key)
    | None ->
        Node.update_rule this_node Node.mismatch ;
        ignore @@ S.add_phi key (Riddler_ddse.mismatch key) phis_top

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

    let beta = Option.value_exn cb.choice in
    (* let phi = Riddler_ddse.cond_top key cb.cond beta condsite_stack in *)
    (* let phis_top' = S.add_phi key phi phis_top in *)
    run_task key_x2 condsite_block phis_top ;

    let cb key rc =
      let (key_c : Lookup_key.t), phis_c = rc in
      let phi = Riddler_ddse.cond_top cb.cond beta key_c.r_stk in
      let phic' = Phi_set.(add phis_c phi) in
      run_task key_xxs condsite_block phis_top ;
      U.by_map_u S.unroll key key_xxs (cb_phi_from_key_and_phis key phic') ;
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
          run_task key_cond_var block phis_top ;

          let cb key_x_ret ((key_c : Lookup_key.t), phis_c) =
            let phi_beta =
              Riddler.bind_x_v [ key_c.x ] key_c.r_stk (Value_bool beta)
            in
            (* let phi_beta = Riddler_ddse.cond_bottom key_c beta cond_block x' in *)
            (* let phis_top' = Phi_set.add phis_c phi_beta in *)
            (* match Riddler.check_phis (Phi_set.to_list phis_top') false with
               | Some _ -> *)
            run_task key_x_ret ctracelet phis_top ;

            U.by_map_u S.unroll key key_x_ret
              (cb_phi_from_key_and_phis key (Phi_set.add phis_c phi_beta)) ;
            Lwt.return_unit
            (* | None -> () *)
          in
          U.by_bind_u S.unroll key_x_ret key_cond_var cb ;
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees)

  let fun_enter (fb : Tracelet.fun_block) is_local key this_node phis_top
      run_task =
    let x, xs, r_stk = Lookup_key.to_parts key in
    let fid = fb.point in
    let callsites =
      match Rstack.paired_callsite r_stk fid with
      | Some callsite -> [ callsite ]
      | None -> fb.callsites
    in

    let sub_trees, this_phis =
      List.fold callsites
        ~f:(fun (sub_trees, this_phis) callsite ->
          let callsite_block, x', x'', x''' =
            Tracelet.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let phi =
                Riddler_ddse.fun_enter key x' x'' x''' is_local fb
                  callsite_stack
              in
              let key_f = Lookup_key.of_parts x'' [] callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              run_task key_f callsite_block phis_top ;

              let key_arg =
                if is_local
                then Lookup_key.of_parts x''' xs callsite_stack
                else Lookup_key.of_parts x'' (x :: xs) callsite_stack
              in
              let node_arg =
                S.find_or_add_node key_arg callsite_block this_node
              in

              let cb key_arg ((key_f : Lookup_key.t), phis_f) =
                let phi_f =
                  Riddler.bind_x_y' [ x'' ] callsite_stack [ key_f.x ]
                    key_f.r_stk
                in
                run_task key_arg callsite_block phis_top ;
                U.by_map_u S.unroll key key_arg
                  (cb_merge Phi_set.(add (add phis_f phi) phi_f)) ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll key_arg key_f cb ;

              (sub_trees @ [ (node_f, node_arg) ], this_phis @ [ phi ])
          | None -> (sub_trees, this_phis))
        ~init:([], [])
    in

    ignore @@ S.add_phi key (Solver.SuduZ3.and_ this_phis) phis_top ;
    Node.update_rule this_node (Node.mk_para ~sub_trees)
  (* Lwt.async (fun () ->
      Lwt_list.iter_p
        (fun (lookup, phi) -> U.by_map S.unroll key lookup (cb_add phi))
        lookups) ; *)
  (* List.iter lookups ~f:(fun (lookup, phi) ->
      U.by_map_u S.unroll key lookup (cb_add phi)) ; *)
  (* let cb ((_key1, phis1), (key2, phis2)) =
       (* Phi_set.print phis1 ;
          Phi_set.print phis2 ; *)
       let phis = Phi_set.(union phis1 phis2) in
       (key2, phis)
     in
     List.iter lookups ~f:(fun (s1, s2) -> U.by_map2_u S.unroll key s1 s2 cb) *)

  let fun_enter_local p this_key this_node phis run_task =
    let ({ fb; is_local; _ } : Fun_enter_local_rule.t) = p in
    fun_enter fb is_local this_key this_node phis run_task

  let fun_enter_nonlocal p this_key this_node phis run_task =
    let ({ fb; is_local; _ } : Fun_enter_nonlocal_rule.t) = p in
    fun_enter fb is_local this_key this_node phis run_task

  let fun_exit p this_key this_node block phis_top run_task =
    let _x, xs, r_stk = Lookup_key.to_parts this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_fun = Lookup_key.of_parts xf [] r_stk in
    let node_fun = S.find_or_add_node key_fun block this_node in

    run_task key_fun block phis_top ;

    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_x_ret = Lookup_key.get_f_return S.block_map fid r_stk x xs in
          let node_x_ret = S.find_or_add_node key_x_ret fblock this_node in
          let phi = Riddler_ddse.fun_exit this_key xf fid S.block_map in

          let cb this_key ((key_f : Lookup_key.t), phis_f) =
            let fid = key_f.x in
            if List.mem fids fid ~equal:Id.equal
            then (
              let phis_top' = Phi_set.union phis_top phis_f in
              run_task key_x_ret fblock phis_top' ;
              U.by_map_u S.unroll this_key key_x_ret
                (cb_phi_from_key_and_phis this_key (Phi_set.add phis_f phi)))
            else () ;
            Lwt.return_unit
          in
          U.by_bind_u S.unroll this_key key_fun cb ;

          sub_trees @ [ node_x_ret ])
        ~init:[]
    in
    Node.update_rule this_node (Node.mk_callsite ~fun_tree:node_fun ~sub_trees)

  let mismatch this_key this_node phis =
    Node.update_rule this_node Node.mismatch ;
    let _phis' = S.add_phi this_key (Riddler_ddse.mismatch this_key) phis in
    ()
end
