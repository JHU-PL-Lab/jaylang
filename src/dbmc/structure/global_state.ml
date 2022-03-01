open Core
include Types.Info
include Types.State

let create_state block x_target =
  let state =
    {
      root_node = ref (Node.root_node (block |> Tracelet.id_of_block) x_target);
      tree_size = 1;
      node_map = Hashtbl.create (module Lookup_key);
      phis_z3 = [];
      phi_map = Hashtbl.create (module Lookup_key);
      input_nodes = Hash_set.create (module Lookup_key);
      lookup_created = Hash_set.create (module Lookup_key);
      lookup_alert = Hash_set.create (module Lookup_key);
      noted_phi_map = Hashtbl.create (module Lookup_key);
      node_set = Hashtbl.create (module Lookup_key);
      node_get = Hashtbl.create (module Lookup_key);
      rstk_picked = Hashtbl.create (module Rstack);
    }
  in
  Hashtbl.add_exn state.node_map
    ~key:(Lookup_key.start x_target)
    ~data:state.root_node;
  Hash_set.strict_add_exn state.lookup_created (Lookup_key.start x_target);
  state

let clear_phis state = state.phis_z3 <- []

let add_phi state key phis =
  Hashtbl.add_exn state.phi_map ~key ~data:phis;
  state.phis_z3 <- phis :: state.phis_z3

let find_or_add state key block parent_node =
  let block_id = Tracelet.id_of_block block in
  match Hashtbl.find state.node_map key with
  | Some child_node ->
      let edge = Node.mk_edge parent_node child_node in
      Node.add_pred child_node edge;
      (true, child_node)
  | None ->
      Hash_set.strict_add_exn state.lookup_created key;
      let child_node =
        ref (Node.mk_node ~block_id ~key ~rule:Node.pending_node)
      in
      let edge = Node.mk_edge parent_node child_node in
      Node.add_pred child_node edge;
      Hashtbl.add_exn state.node_map ~key ~data:child_node;
      (false, child_node)

let pvar_picked state key = not (Hash_set.mem state.lookup_created key)

(* let refresh_picked state model =
   Hashtbl.clear state.rstk_picked;
   Hashtbl.iter_keys state.node_map ~f:(fun key ->
       if Riddler.is_picked (Some model) key
       then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
       else ()) *)

(* let picked_from model key =
     Option.value
       (Solver.SuduZ3.get_bool model (Riddler.pick_at_key key))
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
