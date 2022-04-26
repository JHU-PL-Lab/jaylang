open Core
include Types.State

let create (config : Global_config.t) program =
  let target = config.target in
  let block_map = Tracelet.annotate program target in

  let block0 = Tracelet.find_by_id target block_map in
  let state =
    {
      first = Odefa_ast.Ast_tools.first_id program;
      target;
      program;
      block_map;
      source_map = lazy (Odefa_ddpa.Ddpa_helper.clause_mapping program);
      root_node = ref (Node.root_node (block0 |> Tracelet.id_of_block) target);
      tree_size = 1;
      node_map = Hashtbl.create (module Lookup_key);
      phis_z3 = [];
      phi_map = Hashtbl.create (module Lookup_key);
      input_nodes = Hash_set.create (module Lookup_key);
      lookup_created = Hash_set.create (module Lookup_key);
      smt_lists = Hashtbl.create (module Lookup_key);
      lookup_alert = Hash_set.create (module Lookup_key);
      (* unroll = Unrolls.U_dbmc.create (); *)
      noted_phi_map = Hashtbl.create (module Lookup_key);
      node_set = Hashtbl.create (module Lookup_key);
      node_get = Hashtbl.create (module Lookup_key);
      rstk_picked = Hashtbl.create (module Rstack);
    }
  in
  state

let clear_phis state = state.phis_z3 <- []

let add_phi state key phis =
  Hashtbl.add_exn state.phi_map ~key ~data:phis ;
  state.phis_z3 <- phis :: state.phis_z3

let find_node_exn state key = Hashtbl.find_exn state.node_map key

let init_node state key node =
  Hash_set.strict_add_exn state.lookup_created key ;
  Hashtbl.add_exn state.node_map ~key ~data:node ;
  node

let find_or_add_node state key block node_parent =
  let exist, node_child =
    match Hashtbl.find state.node_map key with
    | Some node_child -> (true, node_child)
    | None ->
        let node_child =
          init_node state key
            (ref
               (Node.mk_node
                  ~block_id:(Tracelet.id_of_block block)
                  ~key ~rule:Node.pending_node))
        in
        (false, node_child)
  in
  let edge = Node.mk_edge node_parent node_child in
  Node.add_pred node_child edge ;
  (exist, node_child)

let pvar_picked state key = not (Hash_set.mem state.lookup_created key)

(* let refresh_picked state model =
   Hashtbl.clear state.rstk_picked;
   Hashtbl.iter_keys state.node_map ~f:(fun key ->
       if Riddler.is_picked (Some model) key
       then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
       else ()) *)

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
