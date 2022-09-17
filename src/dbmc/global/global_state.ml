open Core
include Types.State
open Dj_common

let create (config : Global_config.t) program =
  let target = config.target in
  let block_map = Cfg.annotate program target in

  let block0 = Cfg.block_of_id target block_map in
  let state =
    {
      first = Jayil.Ast_tools.first_id program;
      target;
      program;
      block_map;
      source_map = lazy (Ddpa.Ddpa_helper.clause_mapping program);
      root_node =
        ref (Search_graph.root_node (block0 |> Cfg.id_of_block) target);
      tree_size = 1;
      term_detail_map = Hashtbl.create (module Lookup_key);
      phis = [];
      (* phi_map = Hashtbl.create (module Lookup_key); *)
      input_nodes = Hash_set.create (module Lookup_key);
      lookup_created = Hash_set.create (module Lookup_key);
      smt_lists = Hashtbl.create (module Lookup_key);
      lookup_alert = Hash_set.create (module Lookup_key);
      (* unroll = Unrolls.U_dbmc.create (); *)
      (* noted_phi_map = Hashtbl.create (module Lookup_key); *)
      rstk_picked = Hashtbl.create (module Rstack);
      solver = Solver.solver;
    }
  in
  state

let clear_phis state = state.phis <- []
let pvar_picked state key = not (Hash_set.mem state.lookup_created key)

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
