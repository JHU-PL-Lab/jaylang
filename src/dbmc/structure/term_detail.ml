open Core
open Dj_common

type t = {
  node : Search_graph.node_ref;
  rule : Rule.t;
  mutable phis : Z3.Expr.expr list;
  mutable is_complete_or_fail : bool;
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
    phis = [];
    is_complete_or_fail = false;
    sub_lookups = [];
    is_set = false;
    get_count = 0;
  }
