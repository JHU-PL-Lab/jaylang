open Core
open Odefa_ast
open Odefa_ast.Ast
open Tracelet
open Log.Export

type result_info = Riddler.result_info

exception Found_solution of result_info

module U = Global_state.Unroll

let[@landmark] run ~(config : Global_config.t) ~(state : Global_state.t)
    job_queue : unit =
  let target = state.target in
  let map = state.block_map in
  let x_first = state.first in
  (* let block0 = Tracelet.cut_before true target block in *)
  let block0 = Tracelet.find_by_id target map in

  let add_phi = Global_state.add_phi state in

  let find_or_add_node key block node_parent =
    Global_state.find_or_add_node state key block node_parent |> snd
  in

  (* reset and init *)
  Solver.reset () ;
  Riddler.reset () ;
  state.phis_z3 <- [ Riddler.pick_at_key (Lookup_key.start target) ] ;

  (* async handler *)
  (Lwt.async_exception_hook :=
     fun exn ->
       match exn with
       | Found_solution _ -> raise exn
       | _ -> failwith "unknown exception") ;

  let run_eval key block eval =
    let task () = Scheduler.push job_queue key (eval key block) in
    U.alloc_task state.unroll ~task key
  in

  let module RS = (val (module struct
                         let state = state
                         let config = config
                         let add_phi = add_phi
                         let x_first = x_first
                         let find_or_add_node = find_or_add_node
                         let block_map = state.block_map
                       end) : Ruler.S)
  in
  let module R = Ruler.Make (RS) in
  (* block works similar to env in a common interpreter *)
  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let x, xs, _r_stk = Lookup_key.to_parts this_key in
    let gate_tree = Global_state.find_node_exn state this_key block in

    let run_task key block = run_eval key block lookup in

    (* update global state *)
    state.tree_size <- state.tree_size + 1 ;
    Hash_set.strict_remove_exn state.lookup_created this_key ;
    let result_pusher = U.push_if_new state.unroll this_key in

    (* LLog.app (fun m ->
       m "[Lookup][=>]: %a in block %a" Lookup_key.pp this_key Id.pp
         (Tracelet.id_of_block block)) ; *)
    let p = Riddler.pick_at_key this_key in
    let rule = Rule.rule_of_runtime_status x xs block in
    let apply_rule () =
      let open Rule in
      match rule with
      | Discovery_main p -> R.discovery_main p this_key gate_tree
      | Discovery_nonmain p ->
          R.discovery_nonmain p this_key gate_tree block run_task
      | Input p -> R.input p this_key gate_tree block run_task
      | Discard p -> R.discard p this_key gate_tree block run_task
      | Alias p -> R.alias p this_key gate_tree block run_task
      | Binop b -> R.binop b this_key gate_tree block run_task
      | Record_start p -> R.record_start p this_key gate_tree block run_task
      | Record_end p -> R.record_end p this_key gate_tree block run_task
      | Cond_top cb -> R.cond_top cb this_key gate_tree block run_task
      | Cond_btm p -> R.cond_btm p this_key gate_tree block run_task
      | Fun_enter_local p -> R.fun_enter_local p this_key gate_tree run_task
      | Fun_enter_nonlocal p ->
          R.fun_enter_nonlocal p this_key gate_tree run_task
      | Fun_exit p -> R.fun_exit p this_key gate_tree block run_task
      | Mismatch ->
          Node.update_rule gate_tree Node.mismatch ;
          add_phi this_key (Riddler.mismatch this_key) ;
          Lookup_result.fail_lwt x
    in
    let%lwt _rule_result = apply_rule () in
    let%lwt _ =
      if state.tree_size mod config.steps = 0
      then (
        LLog.app (fun m ->
            m "Step %d\t%a\n" state.tree_size Lookup_key.pp this_key) ;
        match Riddler.check state config with
        | Some { model; c_stk } -> Lwt.fail (Found_solution { model; c_stk })
        | None -> Lwt.return_unit)
      else Lwt.return_unit
    in
    LLog.debug (fun m ->
        m "[Lookup][<=]: %a in block %a\n" Lookup_key.pp this_key Id.pp
          (Tracelet.id_of_block block)) ;
    Lwt.return_unit
  in

  let key_target = Lookup_key.of_parts target [] Rstack.empty in
  let _ = Global_state.init_node state key_target state.root_node in
  run_eval key_target block0 lookup