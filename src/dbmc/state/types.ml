open! Core
open Dj_common

(* Hashtbl.t is mutable by default.
   Using explicit *mutable* is for replacing a new one easier.
*)

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
  type t = {
    (*
        lookup basic
    *)
    first : Id.t;
    target : Id.t;
    program : Jayil.Ast.expr;
    block_map : Cfg.block Jayil.Ast.Ident_map.t;
    source_map : Jayil.Ast.clause Jayil.Ast.Ident_map.t Lazy.t;
    (*
       scheduling
    *)
    job_queue : (Job_key.t, unit) Scheduler.t;
    (*
        search graph
    *)
    root_node : Search_graph.node_ref;
    mutable tree_size : int;
    term_detail_map : (Lookup_key.t, Term_detail.t) Hashtbl.t;
    (*
        search graph for solving
    *)
    lookup_created : Lookup_key.t Hash_set.t;
    input_nodes : Lookup_key.t Hash_set.t;
    (*
       constraint solving
    *)
    mutable phis_staging : Z3.Expr.expr list;
    mutable phis_added : Z3.Expr.expr list;
    smt_lists : (Lookup_key.t, int) Hashtbl.t;
    solver : Z3.Solver.solver;
    (*
        debug and stats
    *)
    lookup_alert : Lookup_key.t Hash_set.t;
    rstk_picked : (Rstack.t, bool) Hashtbl.t;
    rstk_stat_map : (Rstack.t, Rstk_stat.t) Hashtbl.t;
    block_stat_map : (Cfg.block, Block_stat.t) Hashtbl.t;
    mutable check_infos : Check_info.t list;
  }
end
