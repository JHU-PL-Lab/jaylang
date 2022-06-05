open Core

type t = Node

let empty = Node

(* let from_rule_adapter rule (term : Lookup_key.t) =
   let open Rule in
   match rule with
   | Discovery_main _ ->
       let target_stk = Rstack.concretize_top term.r_stk in
       Node.done_ target_stk
   | Discovery_nonmain _ -> Node.to_first node_child
   | _ -> failwith "not yet" *)

let init_node node_map key node =
  Hashtbl.add_exn node_map ~key ~data:node ;
  node

(* for ddse,
   the node tree is a state it updates as the evaluating.
   it's also functional since in each non-deterministic run it updates differently.
   it needs to be compositional.

   phi_set is compositional with respect to non-determinism.
   It works because:
   1. non-determinism join on sets is straightforward.
   2. it composes bottom-up
*)
let set_node_rule node_map key block node_parent rule =
  let node =
    match Hashtbl.find node_map key with
    | Some node -> node
    | None ->
        init_node node_map key
          (ref
             (Node.mk_node ~block_id:(Cfg.id_of_block block) ~key
                ~rule:Node.pending_node))
  in
  let edge = Node.mk_edge node_parent node in
  Node.add_pred node edge ;
  !node.rule <- rule

let set_node_rule_exn node_map key rule =
  let node : Node.ref_t = Hashtbl.find_exn node_map key in
  !node.rule <- rule