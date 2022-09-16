type t = {
  node : Search_graph.node_ref;
  rule : Rule.t;
  mutable phis : Z3.Expr.expr list;
}