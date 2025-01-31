open Core
include Types.State
open Dj_common
open Log.Export

let compute_info (config : Global_config.t) program : info =
  let first = Jayil.Ast_tools.first_id program in
  let target = config.target in
  let block_map =
    match config.analyzer with
    | K_ddpa k -> failwith "unimplemented"
    | K_cfa k -> Jil_analysis.Main.block_map_of_expr k program
  in
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
    job_queue = Scheduler.create ~cmp:job_key_compare ();
  }

let create_stat_state () : stat_state =
  {
    lookup_alert = Hash_set.create (module Lookup_key);
    rstk_picked = Hashtbl.create (module Rstack)
  }

let reset_stat_state (stat_state : stat_state) =
  Hash_set.clear stat_state.lookup_alert ;
  Hashtbl.clear stat_state.rstk_picked 

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
  (* reset_job_state state.job ; *)
  reset_search_state info state.search ;
  reset_stat_state state.stat

let create (config : Global_config.t) program =
  let info = compute_info config program in
  {
    info;
    job = create_job_state config;
    search = create_search_state info.root_node_info;
    stat = create_stat_state ();
  }


let add_phi ?(is_external = false) (state : t) (lookup_detail : Lookup_detail.t)
    phi =
  if is_external
  then lookup_detail.phis_external <- phi :: lookup_detail.phis_external ;
  lookup_detail.phis <- phi :: lookup_detail.phis

let detail_alist (state : t) =
  let sorted_list_of_hashtbl table =
    Hashtbl.to_alist table
    |> List.sort ~compare:(fun (k1, _) (k2, _) ->
           Int.compare (Lookup_key.length k1) (Lookup_key.length k2))
  in
  sorted_list_of_hashtbl state.search.lookup_detail_map

let run_if_fresh state key job =
  match Hashtbl.find state.search.lookup_detail_map key with
  | Some _ -> ()
  | None ->
      if not (Hash_set.mem state.search.lookup_created key)
      then (
        Hash_set.strict_add_exn state.search.lookup_created key ;
        job ())

let add_detail_if_fresh state target key =
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
