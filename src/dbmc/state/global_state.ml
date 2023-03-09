open Core
include Types.State
open Dj_common

let create (config : Global_config.t) program =
  let target = config.target in
  let block_map = Cfg.annotate program target in
  let block0 = Cfg.find_block_by_id target block_map in
  let unroll =
    match config.engine with
    | Global_config.E_dbmc -> S_dbmc (Unrolls.U_dbmc.create ())
    | Global_config.E_ddse -> S_ddse (Unrolls.U_ddse.create ())
  in
  let state =
    {
      first = Jayil.Ast_tools.first_id program;
      target;
      key_target = Lookup_key.start target block0;
      program;
      block_map;
      source_map = lazy (Ddpa.Ddpa_helper.clause_mapping program);
      unroll;
      job_queue = Schedule.create ();
      root_node = ref (Search_graph.root_node block0 target);
      tree_size = 1;
      lookup_detail_map = Hashtbl.create (module Lookup_key);
      lookup_created = Hash_set.create (module Lookup_key);
      input_nodes = Hash_set.create (module Lookup_key);
      phis_staging = [];
      phis_added = [];
      smt_lists = Hashtbl.create (module Lookup_key);
      solver = Z3.Solver.mk_solver Solver.ctx None;
      lookup_alert = Hash_set.create (module Lookup_key);
      rstk_picked = Hashtbl.create (module Rstack);
      rstk_stat_map = Hashtbl.create (module Rstack);
      block_stat_map = Hashtbl.create (module Cfg.Block);
      check_infos = [] (* unroll = Unrolls.U_dbmc.create (); *);
    }
  in
  (* Global_state.lookup_alert state key_target state.root_node; *)
  state

let clear_phis state =
  state.phis_added <- state.phis_added @ state.phis_staging ;
  state.phis_staging <- []

let add_phi (state : t) (lookup_detail : Lookup_detail.t) phi =
  lookup_detail.phis <- phi :: lookup_detail.phis ;
  state.phis_staging <- phi :: state.phis_staging

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
