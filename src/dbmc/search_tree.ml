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
  phi_map : (Lookup_key.t, Constraint.t list) Hashtbl.t;
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
      pvar_reach_top = ref false;
      lookup_created = Hash_set.create (module Lookup_key);
      noted_phi_map = Hashtbl.create (module Lookup_key);
    }
  in
  Hash_set.strict_add_exn state.lookup_created (Lookup_key.start x_target);
  state

let clear_phis state = state.phis_z3 <- []
