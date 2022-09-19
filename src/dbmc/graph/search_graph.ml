open Core
open Dj_common

type ('node, 'leaf) gate =
  | Open
  | Wrong
  | End of 'leaf
  | One of 'node
  | And of 'node * 'node
  | One_and_orlist of 'node * 'node list
  | Orlist_of_pair of ('node * 'node) list
[@@deriving sexp_of, compare, equal]

let fmap_gate node leaf gate =
  let loop = function
    | Open -> Open
    | Wrong -> Wrong
    | End l -> End (leaf l)
    | One n -> One (node n)
    | And (n1, n2) -> And (node n1, node n2)
    | One_and_orlist (n, ns) -> One_and_orlist (node n, List.map ns ~f:node)
    | Orlist_of_pair ns ->
        Orlist_of_pair (List.map ns ~f:(fun (a, b) -> (node a, node b)))
  in
  loop gate

module T = struct
  type t = {
    key : Lookup_key.t;
    block_id : Id.t;
    mutable gate : ((t ref, Concrete_stack.t) gate[@ignore]);
    mutable preds : (edge list[@ignore]);
  }

  and edge = { pred : t ref; succ : t ref }
  (* and gate =
     | Open
     | Mismatch
     | Done of Concrete_stack.t
     (* | Discard of t ref
        | Alias of t ref
        | To_first of t ref *)
     | One of t ref
     (* | Binop of t ref * t ref
        | Cond_choice of t ref * t ref
        | Project of t ref * t ref
     *)
     | And of t ref * t ref
     (* | Condsite of t ref * t ref list
        | Callsite of t ref * t ref list *)
     | One_and_orlist of t ref * t ref list
     (* | Para_local of (t ref * t ref) list
        | Para_nonlocal of (t ref * t ref) list *)
     | Orlist_of_pair of (t ref * t ref) list *)
  [@@deriving sexp_of, compare, equal]
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

type node = Node.t
type node_ref = Node_ref.t

let equal_ref = Node_ref.equal

open Node

let mk_edge pred succ = { pred; succ }

let root_node block_id x =
  { block_id; key = Lookup_key.start x block_id; gate = Open; preds = [] }

let mk_node ~block_id ~key = { block_id; key; gate = Open; preds = [] }

let add_pred node pred =
  if List.mem !node.preds pred ~equal:(fun eg1 eg2 ->
         phys_equal eg1.pred eg2.pred)
  then () (* failwith "why duplicate cvars on edge" *)
  else !node.preds <- pred :: !node.preds

let traverse_node ?(stop = fun _ -> false) ~at_node ~init ~acc_f _node = ()

let fold_tree ?(stop = fun _ -> false) ~init ~init_path ~sum ~sum_path _node =
  init

(* let init_node key_map key node =
   Hashtbl.add_exn key_map ~key ~data:node ;
   node *)