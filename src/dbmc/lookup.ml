open Core
open Odefa_ast
open Odefa_ast.Ast
open Tracelet
open Log.Export
module U = Global_state.Unroll

let[@landmark] run_ddse ~(config : Global_config.t) ~(state : Global_state.t)
    job_queue : unit =
  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;
  state.phis_z3 <- [ Riddler.pick_at_key (Lookup_key.start state.target) ] ;

  (* async handler *)
  (Lwt.async_exception_hook :=
     fun exn ->
       match exn with
       | Riddler.Found_solution _ -> raise exn
       | _ -> failwith "unknown exception") ;

  let run_eval key block eval =
    let task () = Scheduler.push job_queue key (eval key block) in
    U.alloc_task state.unroll ~task key
  in

  let module LS = (val (module struct
                         let state = state
                         let config = config
                         let add_phi = Global_state.add_phi state

                         let find_or_add_node key block node_parent =
                           Global_state.find_or_add_node state key block
                             node_parent
                           |> snd

                         let block_map = state.block_map
                       end) : Lookup_ddse_rule.S)
  in
  let module R = Lookup_ddse_rule.Make (LS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let x, xs, _r_stk = Lookup_key.to_parts this_key in
    let this_node = Global_state.find_node_exn state this_key block in
    let run_task key block = run_eval key block lookup in
    let block_id = Tracelet.id_of_block block in

    Riddler.step_check ~state ~config ;%lwt
    Hash_set.strict_remove_exn state.lookup_created this_key ;

    LLog.app (fun m ->
        m "[Lookup][=>]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ;

    let rule = Rule.rule_of_runtime_status x xs block in
    let _apply_rule =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key this_node
      | Discovery_nonmain p ->
          R.discovery_nonmain p this_key this_node block run_task
      | Input p -> R.input p this_key this_node block run_task
      | Discard p -> R.discard p this_key this_node block run_task
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
      | Mismatch -> R.mismatch this_key this_node
    in

    LLog.debug (fun m ->
        m "[Lookup][<=]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ;
    Lwt.return_unit
  in

  let key_target = Lookup_key.of_parts state.target [] Rstack.empty in
  let _ = Global_state.init_node state key_target state.root_node in
  let block0 = Tracelet.find_by_id state.target state.block_map in
  run_eval key_target block0 lookup

let[@landmark] run ~(config : Global_config.t) ~(state : Global_state.t)
    job_queue : unit =
  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;
  state.phis_z3 <- [ Riddler.pick_at_key (Lookup_key.start state.target) ] ;

  (* async handler *)
  (Lwt.async_exception_hook :=
     fun exn ->
       match exn with
       | Riddler.Found_solution _ -> raise exn
       | _ -> failwith "unknown exception") ;

  let run_eval key block eval =
    let task () = Scheduler.push job_queue key (eval key block) in
    U.alloc_task state.unroll ~task key
  in

  let module LS = (val (module struct
                         let state = state
                         let config = config
                         let add_phi = Global_state.add_phi state
                         let x_first = state.first

                         let find_or_add_node key block node_parent =
                           Global_state.find_or_add_node state key block
                             node_parent
                           |> snd

                         let block_map = state.block_map
                       end) : Lookup_rule.S)
  in
  let module R = Lookup_rule.Make (LS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let x, xs, _r_stk = Lookup_key.to_parts this_key in
    let this_node = Global_state.find_node_exn state this_key block in
    let run_task key block = run_eval key block lookup in
    let block_id = Tracelet.id_of_block block in

    Riddler.step_check ~state ~config ;%lwt
    Hash_set.strict_remove_exn state.lookup_created this_key ;

    LLog.app (fun m ->
        m "[Lookup][=>]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ;

    let rule = Rule.rule_of_runtime_status x xs block in
    let%lwt _apply_rule =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key this_node
      | Discovery_nonmain p ->
          R.discovery_nonmain p this_key this_node block run_task
      | Input p -> R.input p this_key this_node block run_task
      | Discard p -> R.discard p this_key this_node block run_task
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
      | Mismatch -> R.mismatch this_key this_node
    in

    LLog.debug (fun m ->
        m "[Lookup][<=]: %a in block %a" Lookup_key.pp this_key Id.pp block_id) ;
    Lwt.return_unit
  in

  let key_target = Lookup_key.of_parts state.target [] Rstack.empty in
  let _ = Global_state.init_node state key_target state.root_node in
  let block0 = Tracelet.find_by_id state.target state.block_map in
  run_eval key_target block0 lookup