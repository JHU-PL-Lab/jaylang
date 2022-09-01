open! Core
open! Types

include module type of struct
  include State
end

val create : Global_config.t -> Jayil.Ast.expr -> State.t

(* val init_node :
   State.t -> Lookup_key.t -> Search_graph.node_ref -> Search_graph.node_ref *)

val clear_phis : State.t -> unit

(* val refresh_picked : State.t -> Z3.Model.model -> unit *)
(* val add_phi : State.t -> Lookup_key.t -> Z3.Expr.expr -> unit *)

(* val find_or_add_node :
   State.t -> Lookup_key.t -> Cfg.t -> Node.ref_t -> bool * Node.ref_t *)

(* val find_node_exn : State.t -> Lookup_key.t -> Search_graph.node_ref *)
val pvar_picked : State.t -> Lookup_key.t -> bool
