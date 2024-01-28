open Core
include Types.State
open Dj_common
open Log.Export

let compute_info (config : Global_config.t) program : info =
  let first = Jayil.Ast_tools.first_id program in
  let target = config.target in
  let block_map =
    match config.analyzer with
    | K_ddpa k -> Ddpa_for_dj.Cfg_of_ddpa.block_map_of_expr program k target
    | K_cfa k -> Jil_analysis.Main.block_map_of_expr k program
  in
  (* Cfg.dump_block_map block_map ; *)
  let block0, reachable = Cfg.find_block_with_reachable target block_map in
  let key_target = Lookup_key.start target block0 in
  let root_node_info = Search_graph.root_node block0 target in
  let source_map = lazy (Jayil.Ast_tools.clause_mapping program) in
  {
    first;
    reachable;
    target;
    key_target;
    program;
    block_map;
    source_map;
    root_node_info;
  }

let job_key_compare (jk1 : job_key) (jk2 : job_key) =
  let visits_compare = Int.compare jk1.block_visits jk2.block_visits in
  if visits_compare <> 0
  then visits_compare
  else Int.compare (Lookup_key.length jk1.lookup) (Lookup_key.length jk2.lookup)

let create_job_state (config : Global_config.t) : job_state =
  {
    unroll =
      (match config.engine with
      | Global_config.E_dbmc -> S_dbmc (Unrolls.U_dbmc.create ())
      | Global_config.E_ddse -> S_ddse (Unrolls.U_ddse.create ()));
    job_queue = Scheduler.create ~cmp:job_key_compare ();
  }

let reset_job_state job_state =
  (match job_state.unroll with
  | S_dbmc unroll -> Unrolls.U_dbmc.reset unroll
  | S_ddse unroll -> Unrolls.U_ddse.reset unroll) ;
  Scheduler.reset job_state.job_queue

let create_solve_state () : solve_state =
  {
    phis_staging = [];
    phis_added = [];
    smt_lists = Hashtbl.create (module Lookup_key);
    solver = Z3.Solver.mk_solver Solver.ctx None;
  }

let reset_solve_state solve_state =
  solve_state.phis_staging <- [] ;
  solve_state.phis_added <- [] ;
  Hashtbl.clear solve_state.smt_lists ;
  Z3.Solver.reset solve_state.solver

let create_stat_state () : stat_state =
  {
    lookup_alert = Hash_set.create (module Lookup_key);
    rstk_picked = Hashtbl.create (module Rstack);
    rstk_stat_map = Hashtbl.create (module Rstack);
    block_stat_map = Hashtbl.create (module Cfg.Block);
    check_infos = [];
  }

let reset_stat_state (stat_state : stat_state) =
  Hash_set.clear stat_state.lookup_alert ;
  Hashtbl.clear stat_state.rstk_picked ;
  Hashtbl.clear stat_state.rstk_stat_map ;
  Hashtbl.clear stat_state.block_stat_map ;
  stat_state.check_infos <- []

let create_search_state (root_node : Search_graph.node) : search_state =
  {
    root_node = ref root_node;
    tree_size = 1;
    lookup_detail_map = Hashtbl.create (module Lookup_key);
    lookup_created = Hash_set.create (module Lookup_key);
    input_nodes = Hash_set.create (module Lookup_key);
  }

let reset_search_state (info : info) (search_state : search_state) =
  search_state.root_node := info.root_node_info ;
  search_state.tree_size <- 1 ;
  Hashtbl.clear search_state.lookup_detail_map ;
  Hash_set.clear search_state.lookup_created ;
  Hash_set.clear search_state.input_nodes

let reset_mutable_state (config : Global_config.t) (info : info) (state : t) =
  reset_job_state state.job ;
  reset_solve_state state.solve ;
  reset_search_state info state.search ;
  reset_stat_state state.stat

let create (config : Global_config.t) program =
  Solver.set_timeout_sec Solver.ctx config.timeout ;
  let info = compute_info config program in
  (* Global_state.lookup_alert state key_target state.root_node; *)
  Riddler.reset () ;
  {
    info;
    job = create_job_state config;
    solve = create_solve_state ();
    search = create_search_state info.root_node_info;
    stat = create_stat_state ();
  }

let clear_phis state =
  state.solve.phis_added <- state.solve.phis_added @ state.solve.phis_staging ;
  state.solve.phis_staging <- []

let add_phi ?(is_external = false) (state : t) (lookup_detail : Lookup_detail.t)
    phi =
  if is_external
  then lookup_detail.phis_external <- phi :: lookup_detail.phis_external ;
  lookup_detail.phis <- phi :: lookup_detail.phis ;
  state.solve.phis_staging <- phi :: state.solve.phis_staging

let detail_alist (state : t) =
  let sorted_list_of_hashtbl table =
    Hashtbl.to_alist table
    |> List.sort ~compare:(fun (k1, _) (k2, _) ->
           Int.compare (Lookup_key.length k1) (Lookup_key.length k2))
  in
  sorted_list_of_hashtbl state.search.lookup_detail_map

let create_counter state detail key =
  Hashtbl.update state.solve.smt_lists key ~f:(function
    | Some i -> i
    | None ->
        add_phi state detail (Riddler.list_head key) ;
        0)

let fetch_counter state key =
  let new_i =
    Hashtbl.update_and_return state.solve.smt_lists key ~f:(function
      | Some i -> i + 1
      | None -> failwith (Fmt.str "why not inited : %a" Lookup_key.pp key))
  in
  new_i - 1

let run_if_fresh state key job =
  match Hashtbl.find state.search.lookup_detail_map key with
  | Some _ -> ()
  | None ->
      if not (Hash_set.mem state.search.lookup_created key)
      then (
        Hash_set.strict_add_exn state.search.lookup_created key ;
        job ())

let add_detail_if_fresh state target key =
  (* TODO: this is obvious buggy. This function should have a `job` to run  *)
  match Hashtbl.find state.search.lookup_detail_map key with
  | Some _ -> ()
  | None ->
      let detail : Lookup_detail.t =
        let rule =
          Rule.rule_of_runtime_status key state.info.block_map target
        in
        Lookup_detail.mk_detail ~rule ~key
      in
      Hashtbl.add_exn state.search.lookup_detail_map ~key ~data:detail

let scheduler_run state =
  let s = state.job.job_queue in
  LLog.app (fun m -> m "[Queue]size = %d" (Pairing_heap.length s.heap)) ;
  let%lwt _ = Scheduler.run s in
  Lwt.return_unit
