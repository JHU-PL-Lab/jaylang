open Dj_common

type t = {
  node : Search_graph.node_ref;
  rule : Rule.t;
  mutable phis : Z3.Expr.expr list;
  (* debug *)
  mutable is_set : bool;
  mutable get_count : int;
}

let mk_detail ~rule ~block ~key =
  let block_id = Cfg.id_of_block block in
  {
    node = ref (Search_graph.mk_node ~block_id ~key);
    rule;
    phis = [];
    is_set = false;
    get_count = 0;
  }