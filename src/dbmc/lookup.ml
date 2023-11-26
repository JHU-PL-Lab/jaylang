open Core
open Dj_common
open Jayil
open Jayil.Ast
open Cfg
open Log.Export
module U_ddse = Lookup_ddse_rule.U

let scheduler_run (state : Global_state.t) =
  let s = state.job.job_queue in
  LLog.app (fun m -> m "[Queue]size = %d" (Pairing_heap.length s.heap)) ;
  Scheduler.run s

let push_job (state : Global_state.t) (key : Lookup_key.t) task () =
  (* Scheduler.push state.job_queue key task *)
  let job_key : Job_key.t =
    { lookup = key; block_visits = Observe.get_block_visits state key }
  in
  Scheduler.push state.job.job_queue job_key task

let[@landmark] run_ddse ~(config : Global_config.t) ~(state : Global_state.t) :
    unit Lwt.t =
  (* reset and init *)
  Solver.reset state.solve.solver ;
  Riddler.reset () ;

  let unroll =
    match state.job.unroll with
    | S_ddse unroll -> unroll
    | _ -> failwith "unroll"
  in

  let module LS =
    (val (module struct
           let state = state
           let config = config

           let add_phi key phi phis =
             let detail = Hashtbl.find_exn state.search.lookup_detail_map key in
             detail.phis <- phi :: detail.phis ;
             Set.add phis phi

           let block_map = state.info.block_map
           let unroll = unroll
         end)
        : Lookup_ddse_rule.S)
  in
  let module R = Lookup_ddse_rule.Make (LS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec run_task key phis =
    match Hashtbl.find state.search.lookup_detail_map key with
    | Some _ -> ()
    | None ->
        let detail : Lookup_detail.t =
          let rule =
            Rule.rule_of_runtime_status key state.info.block_map config.target
          in
          Lookup_detail.mk_detail ~rule ~key
        in
        Hashtbl.add_exn state.search.lookup_detail_map ~key ~data:detail ;
        let task = push_job state key (lookup key phis) in
        U_ddse.alloc_task unroll ~task key
  and lookup (this_key : Lookup_key.t) phis () : unit Lwt.t =
    let rule =
      Rule.rule_of_runtime_status this_key state.info.block_map config.target
    in
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

  let block0 =
    Cfg.find_reachable_block state.info.target state.info.block_map
  in
  let term_target = Lookup_key.start state.info.target block0 in
  let phis = Phi_set.empty in
  run_task term_target phis ;

  let wait_result =
    U_ddse.by_iter unroll term_target (fun (r : Ddse_result.t) ->
        let phis_to_check = Set.to_list r.phis in
        match
          Checker.check_phis state.solve.solver phis_to_check config.debug_model
        with
        | None -> Lwt.return_unit
        | Some { model; c_stk } ->
            raise (Riddler.Found_solution { model; c_stk }))
  in

  let%lwt _ =
    Lwt.pick
      [
        (let%lwt _ = scheduler_run state in
         Lwt.return_unit);
        wait_result;
      ]
  in
  Lwt.return_unit

let[@landmark] run_dbmc ~(config : Global_config.t) ~(state : Global_state.t) :
    unit Lwt.t =
  let stride = ref config.stride_init in

  let unroll =
    match state.job.unroll with
    | S_dbmc unroll -> unroll
    | _ -> failwith "unroll"
  in
  let run_eval key eval =
    match Hashtbl.find state.search.lookup_detail_map key with
    | Some _ -> ()
    | None ->
        if Hash_set.mem state.search.lookup_created key
        then ()
        else (
          Hash_set.strict_add_exn state.search.lookup_created key ;
          let task = push_job state key (eval key) in
          Unrolls.U_dbmc.alloc_task unroll ~task key)
  in

  let module LS =
    (val (module struct
           let state = state
           let config = config
           let block_map = state.info.block_map
         end)
        : Lookup_rule.S)
  in
  let module R = Lookup_rule.Make (LS) in
  let[@landmark] rec lookup (key : Lookup_key.t) () : unit Lwt.t =
    let rule =
      Rule.rule_of_runtime_status key state.info.block_map config.target
    in
    LLog.app (fun m ->
        m "[Lookup][%d][=>]: %a; [Rule] %a; [Block] %a" state.search.tree_size
          Lookup_key.pp key Rule.pp_rule rule Id.pp key.block.id) ;

    let detail = Lookup_detail.mk_detail ~rule ~key in

    Option.iter !Log.saved_oc ~f:Out_channel.flush ;

    Hashtbl.add_exn state.search.lookup_detail_map ~key ~data:detail ;

    Checker.try_step_check ~state ~config key stride ;%lwt
    state.search.tree_size <- state.search.tree_size + 1 ;
    Observe.dump_block_stat config state ;

    Hash_set.strict_remove_exn state.search.lookup_created key ;

    let phi, action = R.get_initial_phi_action key rule in
    Global_state.add_phi state detail phi ;

    Lookup_rule.run_action
      (fun key -> run_eval key lookup)
      unroll state detail key action ;

    (* Fix for SATO. `abort` is a side-effect clause so it needs to be implied picked.
        run all previous lookups *)
    let previous_clauses = Cfg.clauses_before_x key.block key.x in
    List.iter previous_clauses ~f:(fun tc ->
        let term_prev = Lookup_key.with_x key tc.id in
        Global_state.add_phi ~is_external:true state detail
          (Riddler.implies key term_prev) ;
        run_eval term_prev lookup) ;
    Lwt.return_unit
  in

  Solver.reset state.solve.solver ;
  Riddler.reset () ;
  let lookup_main key_target () =
    lookup key_target () ;%lwt
    let td = Hashtbl.find_exn state.search.lookup_detail_map key_target in
    Lwt.return_unit
  in
  run_eval state.info.key_target lookup_main ;
  let%lwt _ = scheduler_run state in
  Lwt.return_unit
