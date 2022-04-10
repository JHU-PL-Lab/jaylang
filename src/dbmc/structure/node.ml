open Core

module T = struct
  type t = {
    key : Lookup_key.t;
    block_id : Id.t;
    rule : (rule[@ignore]);
    mutable preds : (edge list[@ignore]);
  }

  and edge = { pred : t ref; succ : t ref }

  and rule =
    (* special rule *)
    | Pending
    (* value rule *)
    | Done of Concrete_stack.t
    | Mismatch
    | Discard of t ref
    | Alias of t ref
    | To_first of t ref
    | Project of t ref * t ref
    | Binop of t ref * t ref
    | Cond_choice of t ref * t ref
    | Condsite of t ref * t ref list
    | Callsite of t ref * t ref list
    | Para_local of (t ref * t ref) list
    | Para_nonlocal of (t ref * t ref) list
  [@@deriving sexp_of, compare, equal]

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

  let pp_rule_name oc rule = Fmt.pf oc "%s" (rule_name rule)
end

module Node = struct
  include T
  include Comparator.Make (T)
end

module Node_ref = struct
  module T = struct
    type t = Node.t ref [@@deriving sexp_of, compare, equal]

    let hash = Hashtbl.hash
  end

  include T
  include Comparator.Make (T)
end

type t = Node.t
type ref_t = Node_ref.t

let equal_ref = Node_ref.equal

open Node

let mk_edge pred succ = { pred; succ }

let root_node block_id x =
  { block_id; key = Lookup_key.start x; rule = Pending; preds = [] }

let mk_node ~block_id ~key ~rule = { block_id; key; rule; preds = [] }

let add_pred node pred =
  if List.mem !node.preds pred ~equal:(fun eg1 eg2 ->
         phys_equal eg1.pred eg2.pred)
  then () (* failwith "why duplicate cvars on edge" *)
  else !node.preds <- pred :: !node.preds

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
let cond_choice nc nr = Cond_choice (nc, nr)

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
let traverse_node ?(stop = fun _ -> false) ~at_node ~init ~acc_f node =
  let visited = Hash_set.create (module Lookup_key) in
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
  loop ~acc:init node

let fold_tree ?(stop = fun _ -> false) ~init ~init_path ~sum ~sum_path node =
  let visited = Hash_set.create (module Lookup_key) in
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
  loop ~acc:init ~acc_path:init_path node

(* TODO: try making the rule a mutable field *)
let update_rule tree rule = tree := { !tree with rule }
