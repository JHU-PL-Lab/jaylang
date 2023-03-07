open Core
open Dj_common
open Jayil
open Jayil.Ast
open Cfg
open Log.Export
module U_ddse = Lookup_ddse_rule.U

let push_job (state : Global_state.t) (key : Lookup_key.t) task () =
  (* Scheduler.push state.job_queue key task *)
  let job_key : Job_key.t =
    { lookup = key; block_visits = Observe.get_block_visits state key }
  in
  Scheduler.push state.job_queue job_key task

let[@landmark] run_ddse ~(config : Global_config.t) ~(state : Global_state.t) :
    unit Lwt.t =
  (* reset and init *)
  Solver.reset state.solver ;
  Riddler.reset () ;

  let unroll =
    match state.unroll with S_ddse unroll -> unroll | _ -> failwith "unroll"
  in

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
        let task = push_job state key (lookup key phis) in
        U_ddse.alloc_task unroll ~task key
  and lookup (this_key : Lookup_key.t) phis () : unit Lwt.t =
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
        match
          Checker.check_phis state.solver phis_to_check config.debug_model
        with
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

  let unroll =
    match state.unroll with S_dbmc unroll -> unroll | _ -> failwith "unroll"
  in
  let run_eval key eval =
    match Hashtbl.find state.term_detail_map key with
    | Some _ -> ()
    | None ->
        if Hash_set.mem state.lookup_created key
        then ()
        else (
          (* Fmt.pr "[Task] %a\n" Lookup_key.pp key ; *)
          Hash_set.strict_add_exn state.lookup_created key ;
          let task = push_job state key (eval key) in
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

    Option.iter !Log.saved_oc ~f:Out_channel.flush ;

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
    Run_rule_action.register
      (fun key -> run_eval key lookup)
      unroll state term_detail key rule_action ;

    let phi =
      let key_first = Lookup_key.to_first key state.first in
      let open Rule in
      match rule with
      | Discovery_main p -> Riddler.discover_main_with_picked key (Some p.v)
      | Discovery_nonmain p ->
          Riddler.discover_non_main key key_first (Some p.v)
      | Input p ->
          if p.is_in_main
          then Riddler.discover_main_with_picked key None
          else Riddler.discover_non_main key key_first None
      | Alias p ->
          let key' = Lookup_key.with_x key p.x' in
          Riddler.eq_with_picked key key'
      | Not p ->
          let key' = Lookup_key.with_x key p.x' in
          Riddler.not_with_picked key key'
      | Binop p ->
          let key_x1 = Lookup_key.with_x key p.x1 in
          let key_x2 = Lookup_key.with_x key p.x2 in
          Riddler.binop_with_picked key p.bop key_x1 key_x2
      | Cond_top p ->
          let ({ cond_case_info = cb; condsite_block } : Cond_top_rule.t) = p in
          let beta = cb.choice in
          let _paired, condsite_stack =
            Rstack.pop_at_condtop key.r_stk (cb.condsite, Id.cond_fid beta)
          in
          let x2 = cb.cond in
          let key_x2 = Lookup_key.of3 x2 condsite_stack condsite_block in
          let key_x = Lookup_key.of3 key.x condsite_stack condsite_block in
          Riddler.cond_top key key_x key_x2 beta
      | Cond_btm p ->
          let term_c = Lookup_key.with_x key p.x' in
          Riddler.cond_bottom key term_c p.cond_both
      | Fun_enter_local p ->
          let callsites = Lookup_key.get_callsites key.r_stk key.block in
          Riddler.fun_enter_local key key.block.id callsites state.block_map
      | Fun_enter_nonlocal p -> Riddler.true_
      | Fun_exit p ->
          let key_f = Lookup_key.of3 p.xf key.r_stk key.block in
          Riddler.fun_exit key key_f p.fids state.block_map
      | Pattern p -> Riddler.true_
      | Record_start p -> Riddler.true_
      | Assume p -> Riddler.mismatch_with_picked key
      | Assert p -> Riddler.mismatch_with_picked key
      | Abort p ->
          if Lookup_key.equal key (Lookup_key.start config.target key.block)
          then Riddler.discover_non_main key key_first None
          else Riddler.mismatch_with_picked key
      | Mismatch -> Riddler.mismatch_with_picked key
    in
    Global_state.add_phi state term_detail phi ;

    (* Fix for SATO. `abort` is a side-effect clause so it needs to be implied picked.
        run all previous lookups *)
    let previous_clauses = Cfg.clauses_before_x key.block key.x in
    List.iter previous_clauses ~f:(fun tc ->
        (* Fmt.pr "[Clause before %a] %a\n" Id.pp key.x Id.pp tc.id ; *)
        let term_prev = Lookup_key.with_x key tc.id in
        Global_state.add_phi state term_detail
          (Riddler.picked_imply key term_prev) ;
        run_eval term_prev lookup) ;
    Lwt.return_unit
  in

  (* reset and init *)
  Solver.reset state.solver ;
  Riddler.reset () ;
  let lookup_main key_target () =
    lookup key_target () ;%lwt
    let td = Hashtbl.find_exn state.term_detail_map key_target in
    Lwt.return_unit
  in
  run_eval state.key_target lookup_main ;
  let%lwt _ = Scheduler.run state.job_queue in
  Lwt.return_unit
