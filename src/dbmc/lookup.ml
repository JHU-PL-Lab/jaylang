open Core
open Odefa_ast
open Odefa_ast.Ast
open Cfg
open Log.Export
module U_ddse = Lookup_ddse_rule.U

let[@landmark] run_ddse ~(config : Global_config.t) ~(state : Global_state.t)
    job_queue : unit Lwt.t =
  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;

  let unroll = U_ddse.create () in
  let term_target = Lookup_key.start state.target in

  let run_eval key block eval =
    let task () = Scheduler.push job_queue key (eval key block) in
    U_ddse.alloc_task unroll ~task key
  in

  let module LS = (val (module struct
                         let state = state
                         let config = config

                         let add_phi key phi phis =
                           Global_state.add_phi state key phi ;
                           Set.add phis phi

                         let find_or_add_node key block node_parent =
                           Global_state.find_or_add_node state key block
                             node_parent
                           |> snd

                         let block_map = state.block_map
                         let unroll = unroll
                       end) : Lookup_ddse_rule.S)
  in
  let module R = Lookup_ddse_rule.Make (LS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec run_task key block phis =
    (* LLog.app (fun m -> m "k = %a" Lookup_key.pp key) ; *)
    let task () = Scheduler.push job_queue key (lookup key block phis) in
    U_ddse.alloc_task unroll ~task key
    (* ----- *)
  and lookup (this_key : Lookup_key.t) block phis () : unit Lwt.t =
    let x, _r_stk = Lookup_key.to2 this_key in
    let this_node = Global_state.find_node_exn state this_key in

    let block_id = Cfg.id_of_block block in

    (* match Riddler.check_phis (Set.to_list phis) false with
       | None -> Lwt.return_unit
       | Some _ -> *)
    let rule = Rule.rule_of_runtime_status x block in
    LLog.app (fun m ->
        m "[Lookup][=>]: %a in block %a; Rule %a" Lookup_key.pp this_key Id.pp
          block_id Rule.pp_rule rule) ;

    (* let the_rule = Rule_node.from_rule_adapter rule this_key in
       (match rule with
       | Discovery_main _ -> Node.update_rule this_node the_rule
       | _ -> ()) ; *)
    let _apply_rule =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key this_node phis
      | Discovery_nonmain p ->
          R.discovery_nonmain p this_key this_node block phis run_task
      | Input p -> R.input p this_key this_node block phis run_task
      | Alias p -> R.alias p this_key this_node block phis run_task
      | Binop b -> R.binop b this_key this_node block phis run_task
      | Record_start p ->
          R.record_start p this_key this_node block phis run_task
      | Record_end p -> R.record_end p this_key this_node block phis run_task
      | Cond_top cb -> R.cond_top cb this_key this_node block phis run_task
      | Cond_btm p -> R.cond_btm p this_key this_node block phis run_task
      | Fun_enter_local p ->
          R.fun_enter_local p this_key this_node phis run_task
      | Fun_enter_nonlocal p ->
          R.fun_enter_nonlocal p this_key this_node phis run_task
      | Fun_exit p -> R.fun_exit p this_key this_node block phis run_task
      | Assume p -> R.assume p this_key this_node block phis run_task
      | Assert p -> R.assert_ p this_key this_node block phis run_task
      | Mismatch -> R.mismatch this_key this_node phis
    in

    (* LLog.app (fun m ->
        m "[Lookup][<=]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ; *)
    Lwt.return_unit
  in

  let _ = Global_state.init_node state term_target state.root_node in
  let block0 = Cfg.find_by_id state.target state.block_map in
  let phis = Phi_set.empty in
  run_task term_target block0 phis ;

  let wait_result =
    U_ddse.by_iter unroll term_target (fun (r : Ddse_result.t) ->
        LLog.debug (fun m -> m "HERE?") ;

        let phis_to_check = Set.to_list r.phis in
        match Riddler.check_phis phis_to_check config.debug_model with
        | None -> Lwt.return_unit
        | Some { model; c_stk } ->
            raise (Riddler.Found_solution { model; c_stk }))
  in

  let%lwt _ =
    Lwt.pick
      [
        (let%lwt _ = Scheduler.run job_queue in
         Lwt.return_unit);
        wait_result;
      ]
  in
  Lwt.return_unit

module U = Lookup_rule.U

let[@landmark] run_dbmc ~(config : Global_config.t) ~(state : Global_state.t)
    job_queue : unit Lwt.t =
  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;
  state.phis_z3 <- [ Riddler.picked (Lookup_key.start state.target) ] ;
  let stride = ref config.stride_init in

  let unroll = U.create () in

  let run_eval key block eval =
    let task () = Scheduler.push job_queue key (eval key block) in
    U.alloc_task unroll ~task key
  in

  let module LS = (val (module struct
                         let state = state
                         let config = config

                         let add_phi key phis =
                           Hashtbl.update state.phi_map key ~f:(function
                             | Some phi' -> Riddler.and_ [ phi'; phis ]
                             | None -> phis) ;
                           state.phis_z3 <- phis :: state.phis_z3

                         let find_or_add_node key block node_parent =
                           Global_state.find_or_add_node state key block
                             node_parent
                           |> snd

                         let block_map = state.block_map
                         let unroll = unroll
                         let stride = stride
                       end) : Lookup_rule.S)
  in
  let module R = Lookup_rule.Make (LS) in
  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let x, _r_stk = Lookup_key.to2 this_key in
    let this_node = Global_state.find_node_exn state this_key in
    let run_task key block = run_eval key block lookup in
    let block_id = Cfg.id_of_block block in

    Riddler.step_check ~state ~config stride ;%lwt

    Hash_set.strict_remove_exn state.lookup_created this_key ;

    let rule = Rule.rule_of_runtime_status x block in
    LLog.app (fun m ->
        m "[Lookup][=>]: %a in block %a; Rule %a" Lookup_key.pp this_key Id.pp
          block_id Rule.pp_rule rule) ;

    let _apply_rule =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key this_node
      | Discovery_nonmain p ->
          R.discovery_nonmain p this_key this_node block run_task
      | Input p -> R.input p this_key this_node block run_task
      | Alias p -> R.alias p this_key this_node block run_task
      | Binop b -> R.binop b this_key this_node block run_task
      | Record_start p -> R.record_start p this_key this_node block run_task
      | Record_end p -> R.record_end p this_key this_node block run_task
      | Cond_top cb -> R.cond_top cb this_key this_node block run_task
      | Cond_btm p -> R.cond_btm p this_key this_node block run_task
      | Fun_enter_local p -> R.fun_enter_local p this_key this_node run_task
      | Fun_enter_nonlocal p ->
          R.fun_enter_nonlocal p this_key this_node run_task
      | Fun_exit p -> R.fun_exit p this_key this_node block run_task
      | Assume p -> R.assume p this_key this_node block run_task
      | Assert p -> R.assert_ p this_key this_node block run_task
      | Mismatch -> R.mismatch this_key this_node
    in

    let previous_clauses = Cfg.clauses_before_x block x in
    List.iter previous_clauses ~f:(fun tc ->
        let term_prev = Lookup_key.with_x this_key tc.id in
        let _node_arg = LS.find_or_add_node term_prev block this_node in
        run_task term_prev block) ;

    (* LLog.app (fun m ->
        m "[Lookup]cs:%a\n%a" Cfg.pp_clause_list previous_clauses Id.pp x) ; *)

    (* LLog.app (fun m ->
        m "[Lookup][<=]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ; *)

    (* add for side-effect, run all previous lookups *)
    Lwt.return_unit
  in

  let key_target = Lookup_key.start state.target in
  let _ = Global_state.init_node state key_target state.root_node in
  let block0 = Cfg.find_by_id state.target state.block_map in
  run_eval key_target block0 lookup ;
  let%lwt _ = Scheduler.run job_queue in
  Lwt.return_unit
