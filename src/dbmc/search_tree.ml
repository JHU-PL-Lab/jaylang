open Core

(* Hashtbl.t is mutable by default.
   Using explicit *mutable* is for replacing a new one easier.
*)
type info = {
  first : Id.t;
  target : Id.t;
  program : Odefa_ast.Ast.expr;
  block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t;
}

type state = {
  (* graph attr *)
  root_node : Gate.Node.t ref;
  mutable tree_size : int;
  (* node attr *)
  node_map : (Lookup_key.t, Gate.Node.t ref) Hashtbl.t;
  (* constraints *)
  mutable phis_z3 : Z3.Expr.expr list;
  phi_map : (Lookup_key.t, Z3.Expr.expr) Hashtbl.t;
  input_nodes : Lookup_key.t Hash_set.t;
  (* pvar *)
  pvar_reach_top : bool ref;
  lookup_created : Lookup_key.t Hash_set.t;
  (* debug *)
  noted_phi_map : (Lookup_key.t, (string * Z3.Expr.expr) list) Hashtbl.t;
}

let create_state block x_target =
  let state =
    {
      root_node = ref (Gate.root_node (block |> Tracelet.id_of_block) x_target);
      tree_size = 1;
      node_map = Hashtbl.create (module Lookup_key);
      phis_z3 = [];
      phi_map = Hashtbl.create (module Lookup_key);
      input_nodes = Hash_set.create (module Lookup_key);
      pvar_reach_top = ref false;
      lookup_created = Hash_set.create (module Lookup_key);
      noted_phi_map = Hashtbl.create (module Lookup_key);
    }
  in
  Hash_set.strict_add_exn state.lookup_created (Lookup_key.start x_target);
  state

let clear_phis state = state.phis_z3 <- []

let picked_from model key =
  Option.value
    (Solver.SuduZ3.get_bool model (Riddler.pick_at_key key))
    ~default:true

let collect_picked_input state model =
  let node_picked (node : Gate.Node.t) =
    let picked = picked_from model node.key in
    picked
  in
  let sum_path acc_path node = acc_path && node_picked node in
  let sum acc acc_path (node : Gate.Node.t) =
    if acc_path && Hash_set.mem state.input_nodes node.key then
      let i = Solver.SuduZ3.get_int_s model (Lookup_key.to_str node.key) in
      (node.key, i) :: acc
    else
      acc
  in
  Gate.fold_tree ~init:[] ~init_path:true ~sum ~sum_path !(state.root_node)
