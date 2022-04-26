open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule
module U = Unrolls.U_dbmc

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> unit

  val find_or_add_node :
    Lookup_key.t -> Tracelet.block -> Node.ref_t -> Node.ref_t

  val block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t
  val unroll : U.t
end

module Make (S : S) = struct
  let rule_main v (key : Lookup_key.t) this_node =
    let target_stk = Rstack.concretize_top key.r_stk in
    Node.update_rule this_node (Node.done_ target_stk) ;
    S.add_phi key (Riddler.discover_main_with_picked key v) ;
    U.by_return S.unroll key (Lookup_result.ok key)

  let rule_nonmain v (key : Lookup_key.t) this_node block run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    let node_child = S.find_or_add_node key_first block this_node in
    Node.update_rule this_node (Node.to_first node_child) ;
    S.add_phi key (Riddler.discover_non_main key key_first v) ;
    run_task key_first block ;
    U.by_map_u S.unroll key key_first (fun _ -> Lookup_result.ok key)

  let discovery_main p (key : Lookup_key.t) this_node =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) key this_node

  let discovery_nonmain p (key : Lookup_key.t) this_node block run_task =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) key this_node block run_task

  let input p this_key this_node block run_task =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes this_key ;
    if is_in_main
    then rule_main None this_key this_node
    else rule_nonmain None this_key this_node block run_task

  let alias p (key : Lookup_key.t) this_node block run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let node' = S.find_or_add_node key' block this_node in
    Node.update_rule this_node (Node.alias node') ;
    S.add_phi key (Riddler.eq_with_picked key key') ;
    run_task key' block ;
    U.by_id_u S.unroll key key'

  let binop b (key : Lookup_key.t) this_node block run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let node_x1 = S.find_or_add_node key_x1 block this_node in
    let key_x2 = Lookup_key.with_x key x2 in
    let node_x2 = S.find_or_add_node key_x2 block this_node in

    Node.update_rule this_node (Node.binop node_x1 node_x2) ;
    S.add_phi key (Riddler.binop_with_picked key bop key_x1 key_x2) ;

    run_task key_x1 block ;
    run_task key_x2 block ;
    U.by_map2_u S.unroll key key_x1 key_x2 (fun _ -> Lookup_result.ok key)

  let record_start p (key : Lookup_key.t) this_node block run_task =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in
    let node_r = S.find_or_add_node key_r block this_node in

    run_task key_r block ;

    let counter = ref 0 in
    (* S.add_phi key (Riddler.alias key r) ; *)
    S.add_phi key (Riddler.list_head key) ;
    Hashtbl.add_exn S.state.smt_lists ~key ~data:0 ;

    let cb this_key (r : Lookup_result.t) =
      let key_rv = r.from in
      let rv_block = Tracelet.find_by_id key_rv.x S.block_map in
      let node_rv = S.find_or_add_node key_rv rv_block this_node in
      let rv = Tracelet.record_of_id S.block_map key_rv.x in

      let i = !counter in
      Int.incr counter ;
      Hashtbl.update S.state.smt_lists this_key ~f:(function
        | Some _ -> !counter
        | None -> failwith "smt list key") ;

      (match Ident_map.Exceptionless.find lbl rv with
      | Some (Var (field, _)) ->
          let key_l = Lookup_key.with_x key_rv field in
          let node_l = S.find_or_add_node key_l rv_block this_node in
          Node.update_rule this_node (Node.project node_r node_l) ;

          let phi_i =
            Riddler.record_start_append this_key key_r key_rv key_l i
          in
          S.add_phi this_key phi_i ;

          run_task key_l rv_block ;

          U.by_id_u S.unroll this_key key_l
      | None ->
          Node.update_rule this_node Node.mismatch ;
          S.add_phi key (Riddler.list_append_mismatch this_key i)) ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_r cb

  let record_end p (this_key : Lookup_key.t) this_node block run_task =
    let ({ r; is_in_main; _ } : Record_end_rule.t) = p in
    let rv = Some (Value_record r) in
    if is_in_main
    then rule_main rv this_key this_node
    else rule_nonmain rv this_key this_node block run_task

  let cond_top (cb : Cond_top_rule.t) (key : Lookup_key.t)
      (this_node : Node.ref_t) block run_task =
    let condsite_block = Tracelet.outer_block block S.block_map in
    let x, r_stk = Lookup_key.to2 key in
    let choice = Option.value_exn cb.choice in
    let condsite_stack =
      match Rstack.pop r_stk (cb.point, Id.cond_fid choice) with
      | Some stk -> stk
      | None -> failwith "impossible in CondTop"
    in
    let x2 = cb.cond in

    let key_x2 = Lookup_key.of2 x2 condsite_stack in
    let node_x2 = S.find_or_add_node key_x2 condsite_block this_node in
    let key_x = Lookup_key.of2 x condsite_stack in
    let node_x = S.find_or_add_node key_x condsite_block this_node in

    Node.update_rule this_node (Node.cond_choice node_x2 node_x) ;
    S.add_phi key (Riddler.cond_top key key_x key_x2 choice) ;

    run_task key_x2 condsite_block ;

    let cb key (rc : Lookup_result.t) =
      let c = rc.from in
      if true
      then (
        run_task key_x condsite_block ;
        U.by_id_u S.unroll key key_x)
      else () ;
      Lwt.return_unit
    in
    U.by_bind_u S.unroll key key_x2 cb

  let cond_btm p (this_key : Lookup_key.t) (this_node : Node.ref_t) block
      run_task =
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block =
      Ident_map.find tid S.block_map |> Tracelet.cast_to_cond_block
    in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.of2 x' r_stk in
    let cond_var_tree = S.find_or_add_node term_c block this_node in

    let sub_trees =
      List.fold [ true; false ]
        ~f:(fun sub_trees beta ->
          let ctracelet, key_ret =
            Lookup_key.get_cond_block_and_return cond_block beta r_stk x
          in
          let node_x_ret = S.find_or_add_node key_ret ctracelet this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Hash_set.strict_add_exn S.state.lookup_created this_key ;
    Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees) ;

    let remove_once =
      lazy
        (Hash_set.strict_remove_exn S.state.lookup_created this_key ;
         S.add_phi this_key (Riddler.cond_bottom this_key term_c cond_block))
    in
    let remove_mutex = Nano_mutex.create () in

    run_task term_c block ;

    let cb this_key (c : Lookup_result.t) =
      if c.status
      then (
        Nano_mutex.critical_section remove_mutex ~f:(fun () ->
            ignore @@ Lazy.force remove_once) ;

        List.iter [ true; false ] ~f:(fun beta ->
            if Riddler.eager_check S.state S.config term_c
                 [ Riddler.eqv term_c (Value_bool beta) ]
            then (
              let ctracelet, key_ret =
                Lookup_key.get_cond_block_and_return cond_block beta r_stk x
              in

              run_task key_ret ctracelet ;
              U.by_id_u S.unroll this_key key_ret)
            else ()) ;
        Lwt.return_unit)
      else Lwt.return_unit
    in
    U.by_bind_u S.unroll this_key term_c cb

  let fun_enter_local p this_key this_node run_task =
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in

    let _x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in

    S.add_phi this_key
      (Riddler.fun_enter_local this_key fid callsites S.block_map) ;

    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', x''' =
            Tracelet.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              run_task key_f callsite_block ;

              let cb this_key (_r : Lookup_result.t) =
                let key_arg = Lookup_key.of2 x''' callsite_stack in
                let _node_arg =
                  S.find_or_add_node key_arg callsite_block this_node
                in
                run_task key_arg callsite_block ;
                U.by_id_u S.unroll this_key key_arg ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll this_key key_f cb ;
              sub_trees @ [ (node_f, node_f) ]
          | None -> sub_trees)
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_para ~sub_trees)

  let fun_enter_nonlocal p this_key this_node run_task =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in

    let x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in

    S.add_phi this_key (Riddler.list_head this_key) ;
    Hashtbl.add_exn S.state.smt_lists ~key:this_key ~data:0 ;

    let nonlocal_i = ref 0 in
    let sub_trees =
      List.fold callsites
        ~f:(fun sub_trees callsite ->
          let callsite_block, x', x'', _x''' =
            Tracelet.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              run_task key_f callsite_block ;

              let cb this_key (r : Lookup_result.t) =
                let i = !nonlocal_i in
                Int.incr nonlocal_i ;
                Hashtbl.update S.state.smt_lists this_key ~f:(function
                  | Some _ -> !nonlocal_i
                  | None -> failwith "smt list key") ;

                let key_arg = Lookup_key.of2 x r.from.r_stk in
                let phi_i =
                  Riddler.fun_enter_append this_key key_f r.from fid key_arg i
                in
                S.add_phi this_key phi_i ;
                let fv_block = Tracelet.find_by_id r.from.x S.block_map in
                let _node_arg = S.find_or_add_node key_arg fv_block this_node in
                run_task key_arg fv_block ;
                U.by_id_u S.unroll this_key key_arg ;
                Lwt.return_unit
              in
              U.by_bind_u S.unroll this_key key_f cb ;
              sub_trees @ [ (node_f, node_f) ]
          | None -> sub_trees)
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_para ~sub_trees)

  let fun_exit p this_key this_node block run_task =
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of2 xf r_stk in
    let node_fun = S.find_or_add_node key_f block this_node in
    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in
          let node_x_ret = S.find_or_add_node key_ret fblock this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_callsite ~fun_tree:node_fun ~sub_trees) ;
    S.add_phi this_key (Riddler.fun_exit this_key key_f fids S.block_map) ;

    run_task key_f block ;

    let cb this_key (rf : Lookup_result.t) =
      let fid = rf.from.x in
      if List.mem fids fid ~equal:Id.equal
      then (
        let fblock = Ident_map.find fid S.block_map in
        let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in

        run_task key_ret fblock ;
        U.by_id_u S.unroll this_key key_ret ;
        Lwt.return_unit)
      else Lwt.return_unit
    in

    U.by_bind_u S.unroll this_key key_f cb

  let mismatch this_key this_node =
    Node.update_rule this_node Node.mismatch ;
    S.add_phi this_key (Riddler.mismatch_with_picked this_key)
end
