open Core
open Dj_common
open Jayil
open Jayil.Ast
open Cfg
open Log.Export
module U_ddse = Lookup_ddse_rule.U

let[@landmark] run_ddse ~(config : Global_config.t) ~(state : Global_state.t) :
    unit Lwt.t =
  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;

  let unroll = U_ddse.create () in

  let module LS = (val (module struct
                         let state = state
                         let config = config

                         let add_phi key phi phis =
                           let term_detail =
                             Hashtbl.find_exn state.term_detail_map key
                           in
                           term_detail.phis <- phi :: term_detail.phis ;
                           Set.add phis phi

                         let block_map = state.block_map
                         let unroll = unroll
                       end) : Lookup_ddse_rule.S)
  in
  let module R = Lookup_ddse_rule.Make (LS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec run_task key phis =
    match Hashtbl.find state.term_detail_map key with
    | Some _ -> ()
    | None ->
        let term_detail : Term_detail.t =
          let rule = Rule.rule_of_runtime_status key state.block_map in
          Term_detail.mk_detail ~rule ~key
        in
        Hashtbl.add_exn state.term_detail_map ~key ~data:term_detail ;
        let task () = Scheduler.push state.job_queue key (lookup key phis) in
        U_ddse.alloc_task unroll ~task key
  and lookup (this_key : Lookup_key.t) phis () : unit Lwt.t =
    (* match Riddler.check_phis (Set.to_list phis) false with
       | None -> Lwt.return_unit
       | Some _ -> *)
    let rule = Rule.rule_of_runtime_status this_key state.block_map in
    LLog.app (fun m ->
        m "[Lookup][=>]: %a ; Rule %a" Lookup_key.pp this_key Rule.pp_rule rule) ;

    let _apply_rule =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key phis
      | Discovery_nonmain p -> R.discovery_nonmain p this_key phis run_task
      | Input p -> R.input p this_key phis run_task
      | Alias p -> R.alias p this_key phis run_task
      | Not b -> R.not_ b this_key phis run_task
      | Binop b -> R.binop b this_key phis run_task
      | Record_start p -> R.record_start p this_key phis run_task
      | Cond_top cb -> R.cond_top cb this_key phis run_task
      | Cond_btm p -> R.cond_btm p this_key phis run_task
      | Fun_enter_local p -> R.fun_enter_local p this_key phis run_task
      | Fun_enter_nonlocal p -> R.fun_enter_nonlocal p this_key phis run_task
      | Fun_exit p -> R.fun_exit p this_key phis run_task
      | Pattern p -> R.pattern p this_key phis run_task
      | Assume p -> R.assume p this_key phis run_task
      | Assert p -> R.assert_ p this_key phis run_task
      | Abort p -> R.abort p this_key phis run_task
      | Mismatch -> R.mismatch this_key phis
    in

    (* LLog.app (fun m ->
        m "[Lookup][<=]: %a" Lookup_key.pp this_key) ; *)
    Lwt.return_unit
  in

  let block0 = Cfg.find_block_by_id state.target state.block_map in
  let term_target = Lookup_key.start state.target block0 in
  let phis = Phi_set.empty in
  run_task term_target phis ;

  let wait_result =
    U_ddse.by_iter unroll term_target (fun (r : Ddse_result.t) ->
        let phis_to_check = Set.to_list r.phis in
        match Checker.check_phis phis_to_check config.debug_model with
        | None -> Lwt.return_unit
        | Some { model; c_stk } ->
            raise (Riddler.Found_solution { model; c_stk }))
  in

  let%lwt _ =
    Lwt.pick
      [
        (let%lwt _ = Scheduler.run state.job_queue in
         Lwt.return_unit);
        wait_result;
      ]
  in
  Lwt.return_unit

let[@landmark] run_dbmc ~(config : Global_config.t) ~(state : Global_state.t) :
    unit Lwt.t =
  let stride = ref config.stride_init in

  let unroll = Unrolls.U_dbmc.create () in

  let run_eval key eval =
    match Hashtbl.find state.term_detail_map key with
    | Some _ -> ()
    | None ->
        if Hash_set.mem state.lookup_created key
        then ()
        else (
          (* Fmt.pr "[Task] %a\n" Lookup_key.pp key ; *)
          Hash_set.strict_add_exn state.lookup_created key ;
          let task () = Scheduler.push state.job_queue key (eval key) in
          Unrolls.U_dbmc.alloc_task unroll ~task key)
  in

  let module LS = (val (module struct
                         let state = state
                         let config = config
                         let block_map = state.block_map
                       end) : Lookup_rule.S)
  in
  let module R = Lookup_rule.Make (LS) in
  let[@landmark] rec lookup (key : Lookup_key.t) () : unit Lwt.t =
    let rule = Rule.rule_of_runtime_status key state.block_map in
    let term_detail = Term_detail.mk_detail ~rule ~key in

    Hashtbl.add_exn state.term_detail_map ~key ~data:term_detail ;

    Checker.try_step_check ~state ~config key stride ;%lwt
    state.tree_size <- state.tree_size + 1 ;
    Observe.dump_block_stat config state ;

    Hash_set.strict_remove_exn state.lookup_created key ;

    LLog.app (fun m ->
        m "[Lookup][%d][=>]: %a; [Rule] %a; [Block] %a" state.tree_size
          Lookup_key.pp key Rule.pp_rule rule Id.pp key.block.id) ;

    let rule_action =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p key
      | Discovery_nonmain p -> R.discovery_nonmain p key
      | Input p -> R.input p key
      | Alias p -> R.alias p key
      | Not p -> R.not_ p key
      | Binop b -> R.binop b key
      | Record_start p -> R.record_start p key
      | Cond_top cb -> R.cond_top cb key
      | Cond_btm p -> R.cond_btm p key
      | Fun_enter_local p -> R.fun_enter_local p key
      | Fun_enter_nonlocal p -> R.fun_enter_nonlocal p key
      | Fun_exit p -> R.fun_exit p key
      | Pattern p -> R.pattern p key
      | Assume p -> R.assume p key
      | Assert p -> R.assert_ p key
      | Abort p -> R.abort p key
      | Mismatch -> R.mismatch key
    in
    let run_task key = run_eval key lookup in
    Run_rule_action.run run_task unroll state term_detail rule_action ;

    (* Fix for SATO. `abort` is a side-effect clause so it needs to be implied picked.
        run all previous lookups *)
    let previous_clauses = Cfg.clauses_before_x key.block key.x in
    List.iter previous_clauses ~f:(fun tc ->
        (* Fmt.pr "[Clause before %a] %a\n" Id.pp key.x Id.pp tc.id ; *)
        let term_prev = Lookup_key.with_x key tc.id in
        Global_state.add_phi state term_detail
          (Riddler.picked_imply key term_prev) ;
        run_task term_prev) ;

    LLog.app (fun m -> m "[Lookup][<=]: %a" Lookup_key.pp key) ;

    Lwt.return_unit
  in

  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;
  let block0 = Cfg.find_block_by_id state.target state.block_map in
  let key_target = Lookup_key.start state.target block0 in

  let lookup_main key_target () =
    lookup key_target () ;%lwt
    let td = Hashtbl.find_exn state.term_detail_map key_target in
    Global_state.add_phi state td (Riddler.picked key_target) ;
    Lwt.return_unit
  in
  run_eval key_target lookup_main ;
  let%lwt _ = Scheduler.run state.job_queue in
  Lwt.return_unit
