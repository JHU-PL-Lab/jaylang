type t = {
  node : Search_graph.node_ref;
  rule : Rule.t;
  mutable phi : Z3.Expr.expr option;
}