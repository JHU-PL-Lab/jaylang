open! Core
open Dj_common

(* Note on maps:
   The `State.t` should maintain any necessary datas.

   Scheduler should only hold a queue, not a map. It's not scheduler's responsibility to
   check any duplicate tasks. On the contrary, tasks with duplicate keys, or even duplicate
   tasks should do no harm.

   Unroll may also has an internal map from key to task.

   Interpreter can also update the node map.
   It's not a good idea that interpreter directly motate the searching state.
   Using another map in interpreter session may be a better choice.
*)

(* Note on a general node map and special node maps
   The key difference is whether
   we need to traverse all node and check the property
   or we only traverse the nodes with this property.

   e.g. input_nodes is a collection of all nodes that is for input clause.
   We only need to traverse these nodes to determine their order.
*)

(* Note on graph
   What do we want to put in the graph, or into a node map?
   Now the question becomes what property is better to be a value,
   what is better to be relations between nodes?
*)
module State = struct
  (* type unroll_t = S_dbmc of Unrolls.U_dbmc.t | S_ddse of Unrolls.U_ddse.t *)

  type info = {
    first : Id.t;
    target : Id.t;
    reachable : bool;
    key_target : Lookup_key.t;
    program : Jayil.Ast.expr;
    block_map : Cfg.block Jayil.Ast.Ident_map.t;
    source_map : Jayil.Ast.clause Jayil.Ast.Ident_map.t Lazy.t;
    root_node_info : Search_graph.node;
  }

  type job_key = { lookup : Lookup_key.t; block_visits : int }

  type job_state = {
    job_queue : (job_key, unit) Scheduler.t;
  }


  type search_state = {
    root_node : Search_graph.node_ref;
    mutable tree_size : int;
    lookup_detail_map : (Lookup_key.t, Lookup_detail.t) Hashtbl.t;
    lookup_created : Lookup_key.t Hash_set.t;
    input_nodes : Lookup_key.t Hash_set.t;
  }

  type stat_state = {
    lookup_alert : Lookup_key.t Hash_set.t;
    rstk_picked : (Rstack.t, bool) Hashtbl.t;
  }

  type t = {
    (* immutable *)
    info : info;
    (* mutable *)
    job : job_state;
    search : search_state;
    stat : stat_state;
  }
end
