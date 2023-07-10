open Core
include Types.State
open Dj_common

type immutable_state = {
  first : Id.t;
  target : Id.t;
  key_target : Lookup_key.t;
  program : Jayil.Ast.expr;
  block_map : Cfg.block Jayil.Ast.Ident_map.t;
  source_map : Jayil.Ast.clause Jayil.Ast.Ident_map.t Lazy.t;
  root_node : Search_graph.node;
}

let compute_immutable_state (config : Global_config.t) program : immutable_state
    =
  let first = Jayil.Ast_tools.first_id program in
  let target = config.target in
  let block_map = Cfg.annotate program target in
  let block0 = Cfg.find_block_by_id target block_map in
  let key_target = Lookup_key.start target block0 in
  let source_map = lazy (Jayil.Ast_tools.clause_mapping program) in
  let root_node = Search_graph.root_node block0 target in
  { first; target; key_target; program; block_map; source_map; root_node }

let create_job_state (config : Global_config.t) : job_state =
  {
    unroll =
      (match config.engine with
      | Global_config.E_dbmc -> S_dbmc (Unrolls.U_dbmc.create ())
      | Global_config.E_ddse -> S_ddse (Unrolls.U_ddse.create ()));
    job_queue = Schedule.create ();
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

let create_search_state (istate : immutable_state) : search_state =
  {
    root_node = ref istate.root_node;
    tree_size = 1;
    lookup_detail_map = Hashtbl.create (module Lookup_key);
    lookup_created = Hash_set.create (module Lookup_key);
    input_nodes = Hash_set.create (module Lookup_key);
  }

let reset_search_state (istate : immutable_state) (search_state : search_state)
    =
  search_state.root_node := istate.root_node ;
  search_state.tree_size <- 1 ;
  Hashtbl.clear search_state.lookup_detail_map ;
  Hash_set.clear search_state.lookup_created ;
  Hash_set.clear search_state.input_nodes

let reset_mutable_state (config : Global_config.t) (istate : immutable_state)
    (state : t) =
  reset_job_state state.job ;
  reset_solve_state state.solve ;
  reset_search_state istate state.search ;
  reset_stat_state state.stat ;
  ()

let create (config : Global_config.t) program =
  let istate = compute_immutable_state config program in

  Solver.set_timeout_sec Solver.ctx config.timeout ;
  let state =
    {
      first = istate.first;
      target = istate.target;
      key_target = istate.key_target;
      program = istate.program;
      block_map = istate.block_map;
      source_map = istate.source_map;
      job = create_job_state config;
      solve = create_solve_state ();
      search = create_search_state istate;
      stat = create_stat_state ();
    }
  in
  (* Global_state.lookup_alert state key_target state.root_node; *)
  state

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

(* let picked_from model key =
     Option.value
       (Solver.SuduZ3.get_bool model (Riddler.picked key))
       ~default:true

   let collect_picked_input state model =
     let node_picked (node : Node.t) =
       let picked = picked_from model node.key in
       picked
     in
     let sum_path acc_path node = acc_path && node_picked node in
     let sum acc acc_path (node : Node.t) =
       if acc_path && Hash_set.mem state.input_nodes node.key
       then
         let i = Solver.SuduZ3.get_int_s model (Lookup_key.to_string node.key) in
         (node.key, i) :: acc
       else acc
     in
     Node.fold_tree ~init:[] ~init_path:true ~sum ~sum_path !(state.root_node) *)
