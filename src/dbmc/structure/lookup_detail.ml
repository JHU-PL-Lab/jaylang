open Core
open Dj_common

type t = {
  node : Search_graph.node_ref;
  rule : Rule.t;
  (* constraints *)
  mutable phis : Z3.Expr.expr list;
  (* state *)
  mutable domain : Lookup_key.t list;
  mutable status : Lookup_status.t;
  mutable status_gen_phi : Lookup_status.t;
  (* sublookup *)
  mutable sub_preconds : bool ref list;
  mutable sub_lookups : Lookup_key.t list;
  (* debug *)
  mutable is_set : bool;
  mutable get_count : int;
}

let mk_detail ~rule ~key =
  let block_id = key.Lookup_key.block.id in
  {
    node = ref (Search_graph.mk_node ~block_id ~key);
    rule;
    domain = [];
    phis = [];
    status = Lookup_status.Good;
    status_gen_phi = Lookup_status.Good;
    sub_preconds = [];
    sub_lookups = [];
    is_set = false;
    get_count = 0;
  }
