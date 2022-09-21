module Node = struct
  type t = Lookup_key.t

  let compare = Lookup_key.compare
  let equal = Lookup_key.equal
  let hash = Lookup_key.hash
end

module Edge = struct
  type t = string

  let compare = String.compare
  let equal = String.equal
  let default = ""
end

module G = Graph.Imperative.Digraph.ConcreteLabeled (Node) (Edge)
include G

(*
  let rule_name = function
       | Pending -> "Pending"
       | Done _ -> "Done"
       | Mismatch -> "Mismatch"
       | Discard _ -> "Discard"
       | Alias _ -> "Alias"
       | To_first _ -> "To_first"
       | Project _ -> "Project"
       | Binop _ -> "Binop"
       | Cond_choice _ -> "Cond_choice"
       | Callsite _ -> "Callsite"
       | Condsite _ -> "Condsite"
       | Para_local _ -> "Para_local"
       | Para_nonlocal _ -> "Para_nonlocal"

     let pp_rule_name oc rule = Fmt.pf oc "%s" (rule_name rule) *)

(*
   let mk_callsite ~fun_tree ~sub_trees = Callsite (fun_tree, sub_trees)
   let mk_condsite ~cond_var_tree ~sub_trees = Condsite (cond_var_tree, sub_trees)
   let mk_para ~sub_trees = Para_local sub_trees
   let pending_node = Pending
   let done_ cstk = Done cstk
   let discard node = Discard node
   let mismatch = Mismatch
   let alias node = Alias node
   let project n1 n2 = Project (n1, n2)
   let to_first node = To_first node
   let binop n1 n2 = Binop (n1, n2)
   let cond_choice nc nr = Cond_choice (nc, nr) *)

(*
   one-to-one stack mapping between lookup and running
*)

(*
   apply = fun f -> fun x ->
     r = f x

   z1 = apply f1 0
   z2 = apply f2 0
*)

(* forward
   we visit one nodes then its children
   when a node is visited, its parents may not be visited. we only know at least
   one of its parents are visited.

   What we need is a topological sorting.

   Before visiting a node (the node is candicate as a child of a visited node),
   we check if its parents are all visited.

   It will be similar to Clang::PostOrderCFGView.

   Forward & Backward? One-run & Multi-runs? Circles? Pre-order & Post-order?

   In `lookup`, it's an incremental processing to search children, therefore it's
   forward in the perspective of graph (through the lookup itself is a backward).

   A circle is not a problem in pre-order traversing since after a node is visited,
   it's still visited if coming back to itself again. We need a `visited` map to record it.

   `Lookup` is pre-order since we need to visit a node to create its children.
   **At present** generating constraints of a node has nothing to do with its children.
   (This can be optimized later to shrink many unnecessary infeasible cases)

   There is one use-case for post-order traversing `picked_from_root`:
      1) a node is picked from the solver result.
      2) and there is a path from this node to the root that all nodes are picked
   They are used together to exclude a node only picked by the SMT solver.

   A `visited` map is not suffient to implement a topological sorting on post-order visiting.

   Pre-order and post-order are more central to the traverse target.
   Forward and backward are of implementation. The choices on the same layer includes
   DFS, BFS, priority-queue. The main task is to reduce the complexity and prevent starvation.
*)

(* let visited = Hash_set.create (module Lookup_key) in
   let rec loop ~acc node =
     let is_stop = stop node in
     if is_stop
     then ()
     else
       (* visit this node *)
       let duplicate =
         match Hash_set.strict_add visited !node.key with
         | Ok () ->
             at_node node ;
             false
         | Error _ -> true
       in
       let acc = acc_f acc node in
       (* traverse its children *)
       if not duplicate
       then
         match !node.rule with
         | Pending | Done _ | Mismatch -> ()
         | Discard child | Alias child | To_first child -> loop ~acc child
         | Project (n1, n2) | Binop (n1, n2) | Cond_choice (n1, n2) ->
             List.iter ~f:(loop ~acc) [ n1; n2 ]
         | Callsite (node, child_edges) | Condsite (node, child_edges) ->
             loop ~acc node ;
             List.iter ~f:(fun n -> loop ~acc n) child_edges
         | Para_local ncs | Para_nonlocal ncs ->
             List.iter
               ~f:(fun (n1, n2) -> List.iter ~f:(loop ~acc) [ n1; n2 ])
               ncs
       else ()
   in
   loop ~acc:init node *)

(* let visited = Hash_set.create (module Lookup_key) in
   let rec loop ~acc ~acc_path node =
     let is_stop = stop node in
     if is_stop
     then acc
     else
       (* fold this node *)
       let duplicate =
         match Hash_set.strict_add visited node.key with
         | Ok () -> false
         | Error _ -> true
       in
       if not duplicate
       then
         let acc_path = sum_path acc_path node in
         let acc = sum acc acc_path node in
         (* fold its children *)
         match node.rule with
         | Pending | Done _ | Mismatch -> acc
         | Discard child | Alias child | To_first child ->
             loop ~acc ~acc_path !child
         | Project (n1, n2) | Binop (n1, n2) | Cond_choice (n1, n2) ->
             List.fold ~init:acc
               ~f:(fun acc n -> loop ~acc ~acc_path !n)
               [ n1; n2 ]
         | Callsite (node, child_edges) | Condsite (node, child_edges) ->
             let acc = loop ~acc ~acc_path !node in
             List.fold ~init:acc
               ~f:(fun acc n -> loop ~acc ~acc_path !n)
               child_edges
         | Para_local ncs | Para_nonlocal ncs ->
             List.fold ~init:acc
               ~f:(fun acc (n1, n2) ->
                 loop ~acc:(loop ~acc ~acc_path !n1) ~acc_path !n2)
               ncs
       else acc
   in
   loop ~acc:init ~acc_path:init_path node *)

(* let empty = Node *)

(* for ddse,
   the node tree is a state it updates as the evaluating.
   it's also functional since in each non-deterministic run it updates differently.
   it needs to be compositional.

   phi_set is compositional with respect to non-determinism.
   It works because:
   1. non-determinism join on sets is straightforward.
   2. it composes bottom-up
*)
(* let set_node_rule key_map key block node_parent rule =
     let node =
       match Hashtbl.find key_map key with
       | Some node -> node
       | None ->
           init_node key_map key
             (ref
                (Node.mk_node ~block_id:(Cfg.id_of_block block) ~key
                   ~rule:Node.pending_node))
     in
     let edge = Node.mk_edge node_parent node in
     Node.add_pred node edge ;
     !node.rule <- rule

   let set_node_rule_exn key_map key rule =
     let node : Node.ref_t = Hashtbl.find_exn key_map key in
     !node.rule <- rule *)
