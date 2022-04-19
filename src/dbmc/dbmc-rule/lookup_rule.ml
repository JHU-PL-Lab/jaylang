open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule

(*
   module type X_int = sig val x : int end;;

   utop # let rec mm = (module struct let x = y end : X_int) and y = 4;;
   val mm : (module X_int) = <module>
*)

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> unit
  val x_first : Id.t

  val find_or_add_node :
    Lookup_key.t -> Tracelet.block -> Node.ref_t -> Node.ref_t

  val block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t

  (* type eval_typ = Lookup_key.t -> Tracelet.block -> unit -> unit Lwt.t

     val run_eval : Lookup_key.t -> Tracelet.block -> eval_typ *)
end

module U = Global_state.Unroll

module Make (S : S) = struct
  let rule_main v (key : Lookup_key.t) this_node =
    let result_pusher = U.push_if_new S.state.unroll key in
    let target_stk = Rstack.concretize_top key.r_stk in
    Node.update_rule this_node (Node.done_ target_stk) ;
    S.add_phi key (Riddler.discover_main key v) ;
    result_pusher (Lookup_result.ok key.x) ;
    Lookup_result.ok_lwt key.x

  let rule_nonmain v (key : Lookup_key.t) this_node block run_task =
    let result_pusher = U.push_if_new S.state.unroll key in
    let key_first = Lookup_key.to_first key S.x_first in
    let node_child = S.find_or_add_node key_first block this_node in
    Node.update_rule this_node (Node.to_first node_child) ;
    S.add_phi key (Riddler.discover_non_main key S.x_first v) ;
    run_task key_first block ;
    U.by_map S.state.unroll key key_first (fun _ -> Lookup_result.ok key.x) ;%lwt
    Lookup_result.ok_lwt key.x

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

  (* let discard p (key : Lookup_key.t) _this_node _block _run_task =
       let ({ v; _ } : Discard_rule.t) = p in
       let result_pusher = U.push_if_new S.state.unroll key in
       failwith "how to encode"
     (* let key_drop_x = Lookup_key.drop_x key in
        let node_sub = S.find_or_add_node key_drop_x block this_node in
        Node.update_rule this_node (Node.discard node_sub) ;
        S.add_phi key (Riddler.discard key (Some v)) ;

        run_task key_drop_x block ;
        U.by_id S.state.unroll key key_drop_x ;%lwt
        Lookup_result.ok_lwt key.x *) *)

  let alias p (key : Lookup_key.t) this_node block run_task =
    let ({ x; x' } : Alias_rule.t) = p in
    let key_rx = Lookup_key.with_x key x' in
    let node_rx = S.find_or_add_node key_rx block this_node in
    Node.update_rule this_node (Node.alias node_rx) ;
    S.add_phi key (Riddler.alias key x') ;
    run_task key_rx block ;
    U.by_id S.state.unroll key key_rx ;%lwt
    Lookup_result.ok_lwt x

  let binop b (key : Lookup_key.t) this_node block run_task =
    let ({ x; bop; x1; x2 } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let node_x1 = S.find_or_add_node key_x1 block this_node in
    let key_x2 = Lookup_key.with_x key x2 in
    let node_x2 = S.find_or_add_node key_x2 block this_node in

    Node.update_rule this_node (Node.binop node_x1 node_x2) ;
    S.add_phi key (Riddler.binop key bop x1 x2) ;

    run_task key_x1 block ;
    run_task key_x2 block ;
    U.by_map2_u S.state.unroll key key_x1 key_x2 (fun _ -> Lookup_result.ok x) ;
    Lookup_result.ok_lwt x

  let record_start p (key : Lookup_key.t) this_node block _run_task =
    let ({ x; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key x in
    let node_r = S.find_or_add_node key_r block this_node in
    failwith "how to encode"
  (* let key_r_l = Lookup_key.with_x2 key (r, lbl) in
     let node_r_l = S.find_or_add_node key_r_l block this_node in

     Node.update_rule this_node (Node.project node_r node_r_l) ;
     S.add_phi key (Riddler.alias_key key key_r_l) ;

     run_task key_r_l block ;
     U.by_id S.state.unroll key key_r_l ;%lwt

     Lookup_result.ok_lwt x *)

  let record_end p (key : Lookup_key.t) _this_node _block _run_task =
    let ({ r; _ } : Record_end_rule.t) = p in
    let (Record_value _rmap) = r in
    let _x, r_stk = Lookup_key.to2 key in
    (* let labal, xs' = (List.hd_exn xs, List.tl_exn xs) in *)
    failwith "how to encode"
  (* match Ident_map.Exceptionless.find labal rmap with
     | Some (Var (vid, _)) ->
         let key' = Lookup_key.of2 (vid :: xs') r_stk in
         let node_key = S.find_or_add_node key' block this_node in
         Node.update_rule this_node (Node.alias node_key) ;
         S.add_phi key (Riddler.alias_key key key') ;

         run_task key' block ;
         U.by_id S.state.unroll key key' ;%lwt
         Lookup_result.ok_lwt key.x
     | None ->
         Node.update_rule this_node Node.mismatch ;
         S.add_phi key (Riddler.mismatch key) ;
         Lookup_result.fail_lwt key.x *)

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
    let key_xxs = Lookup_key.of2 x condsite_stack in
    let node_xxs = S.find_or_add_node key_xxs condsite_block this_node in

    Node.update_rule this_node (Node.cond_choice node_x2 node_xxs) ;
    S.add_phi key (Riddler.cond_top key cb condsite_stack) ;

    run_task key_x2 condsite_block ;

    let cb key (rc : Lookup_result.t) =
      let c = rc.from in
      if true
      then (
        run_task key_xxs condsite_block ;
        Lwt.async (fun () -> U.by_id S.state.unroll key key_xxs))
      else () ;
      Lwt.return_unit
    in
    U.by_bind S.state.unroll key key_x2 cb ;%lwt

    Lookup_result.ok_lwt x

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
    let key_cond_var = Lookup_key.of2 x' r_stk in
    let cond_var_tree = S.find_or_add_node key_cond_var block this_node in

    let sub_trees =
      List.fold [ true; false ]
        ~f:(fun sub_trees beta ->
          let ctracelet, key_x_ret =
            Lookup_key.get_cond_block_and_return cond_block beta r_stk x
          in
          let node_x_ret = S.find_or_add_node key_x_ret ctracelet this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Hash_set.strict_add_exn S.state.lookup_created this_key ;
    Node.update_rule this_node (Node.mk_condsite ~cond_var_tree ~sub_trees) ;

    let remove_once =
      lazy
        (Hash_set.strict_remove_exn S.state.lookup_created this_key ;
         S.add_phi this_key (Riddler.cond_bottom this_key cond_block x'))
    in
    let remove_mutex = Nano_mutex.create () in

    run_task key_cond_var block ;

    let cb this_key (c : Lookup_result.t) =
      if c.status
      then (
        Nano_mutex.critical_section remove_mutex ~f:(fun () ->
            ignore @@ Lazy.force remove_once) ;

        List.iter [ true; false ] ~f:(fun beta ->
            if Riddler.eager_check S.state S.config key_cond_var
                 [ Riddler.bind_x_v x' r_stk (Value_bool beta) ]
            then (
              let ctracelet, key_x_ret =
                Lookup_key.get_cond_block_and_return cond_block beta r_stk x
              in

              run_task key_x_ret ctracelet ;
              Lwt.async (fun () -> U.by_id S.state.unroll this_key key_x_ret))
            else ()) ;
        Lwt.return_unit)
      else Lwt.return_unit
    in
    U.by_bind S.state.unroll this_key key_cond_var cb ;%lwt
    Lookup_result.ok_lwt x

  let fun_enter (fb : Tracelet.fun_block) is_local this_key this_node run_task =
    let x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites =
      match Rstack.paired_callsite r_stk fid with
      | Some callsite -> [ callsite ]
      | None -> fb.callsites
    in
    LLog.debug (fun m ->
        m "FunEnter%s: %a -> %a"
          (if is_local then "" else "Nonlocal")
          Id.pp fid Id.pp_list callsites) ;
    let sub_trees, lookups =
      List.fold callsites
        ~f:(fun (sub_trees, lookups) callsite ->
          let callsite_block, x', x'', _x''' =
            Tracelet.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let node_f = S.find_or_add_node key_f callsite_block this_node in
              let _lookup_f = run_task key_f callsite_block in
              let key_arg =
                failwith "how to encode"
                (* if is_local
                   then Lookup_key.of2 x''' xs callsite_stack
                   else Lookup_key.of2 x'' (x :: xs) callsite_stack *)
              in
              let node_arg =
                S.find_or_add_node key_arg callsite_block this_node
              in
              let _lookup_f_arg = run_task key_arg callsite_block in
              (sub_trees @ [ (node_f, node_arg) ], lookups @ [ key_arg ])
          | None -> (sub_trees, lookups))
        ~init:([], [])
    in

    Node.update_rule this_node (Node.mk_para ~sub_trees) ;
    S.add_phi this_key
      (Riddler.fun_enter this_key is_local fb callsites S.block_map) ;

    U.by_join S.state.unroll this_key lookups ;%lwt
    Lookup_result.ok_lwt x

  let fun_enter_local p this_key this_node run_task =
    let ({ fb; is_local; _ } : Fun_enter_local_rule.t) = p in
    fun_enter fb is_local this_key this_node run_task

  let fun_enter_nonlocal p this_key this_node run_task =
    let ({ fb; is_local; _ } : Fun_enter_nonlocal_rule.t) = p in
    fun_enter fb is_local this_key this_node run_task

  let fun_exit p this_key this_node block run_task =
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    LLog.debug (fun m -> m "FunExit: %a -> %a" Id.pp xf Id.pp_list fids) ;
    let key_fun = Lookup_key.of2 xf r_stk in
    let node_fun = S.find_or_add_node key_fun block this_node in
    let sub_trees =
      List.fold fids
        ~f:(fun sub_trees fid ->
          let fblock = Ident_map.find fid S.block_map in
          let key_x_ret = Lookup_key.get_f_return S.block_map fid r_stk x in
          let node_x_ret = S.find_or_add_node key_x_ret fblock this_node in
          sub_trees @ [ node_x_ret ])
        ~init:[]
    in

    Node.update_rule this_node (Node.mk_callsite ~fun_tree:node_fun ~sub_trees) ;
    S.add_phi this_key (Riddler.fun_exit this_key xf fids S.block_map) ;

    run_task key_fun block ;

    let cb this_key (rf : Lookup_result.t) =
      let fid = rf.from in
      if List.mem fids fid ~equal:Id.equal
      then (
        let fblock = Ident_map.find fid S.block_map in
        let key_x_ret = Lookup_key.get_f_return S.block_map fid r_stk x in

        run_task key_x_ret fblock ;
        Lwt.async (fun () -> U.by_id S.state.unroll this_key key_x_ret) ;
        Lwt.return_unit)
      else Lwt.return_unit
    in

    U.by_bind S.state.unroll this_key key_fun cb ;%lwt
    Lookup_result.ok_lwt x

  let mismatch this_key this_node =
    Node.update_rule this_node Node.mismatch ;
    S.add_phi this_key (Riddler.mismatch this_key) ;
    Lookup_result.fail_lwt this_key.x
end
