open Core

type t = {
  node_map : (Lookup_key.t, Gate.Node.t ref) Hashtbl.t;
  phi_map : (Lookup_key.t, Constraint.t list) Hashtbl.t;
  acc_phi_map : (Lookup_key.t, Constraint.t list) Hashtbl.t;
  current_pendings : Gate.Node.t ref Hash_set.t;
  cvar_complete_map : (Cvar.t, bool) Hashtbl.t;
  cvar_complete : (Cvar.t, bool) Hashtbl.t;
  mutable cvar_picked_map : (Cvar.t, bool) Hashtbl.t;
  root_node : Gate.Node.t ref;
}

let create block x_target =
  let root_node =
    ref
      (Gate.root_node (block |> Tracelet.id_of_block |> Id.of_ast_id) x_target)
  in
  let state =
    {
      node_map = Hashtbl.create (module Lookup_key);
      phi_map = Hashtbl.create (module Lookup_key);
      acc_phi_map = Hashtbl.create (module Lookup_key);
      current_pendings = Hash_set.create (module Gate.Node_ref);
      cvar_complete_map = Hashtbl.create (module Cvar);
      cvar_complete = Hashtbl.create (module Cvar);
      cvar_picked_map = Hashtbl.create (module Cvar);
      root_node;
    }
  in
  Hash_set.add state.current_pendings root_node;
  (* Hash_set.add state.visited_map !root_node.key; *)
  state

let add_phi ?debug_info state key data =
  ignore debug_info;
  Hashtbl.add_multi state.phi_map ~key ~data
(* match Core.Hashtbl.add phi_map ~key ~data with
    | `Ok -> ()
    | `Duplicate ->
        let old_v = Core.Hashtbl.find_exn phi_map key in
        if Constraint.equal old_v data then
          ()
        else (
          (match debug_info with
          | Some l_key ->
              Fmt.pr "key: %a\nv_old: %a\nv_new: %a\n" Lookup_key.pp l_key
                Constraint.pp old_v Constraint.pp data
          | None -> ());
          failwith "add_phi key duplication") *)

let merge_to_acc_phi_map state () =
  Hashtbl.merge_into ~src:state.phi_map ~dst:state.acc_phi_map
    ~f:(fun ~key a ob ->
      let _ = key in
      match ob with
      | Some _b -> failwith "acc_phi_map should not have it"
      | None -> Set_to a);
  Hashtbl.clear state.phi_map

(* Logs.app (fun m ->
    m "Visiting: Node(%a) = %B, preds[%d]\n" Lookup_key.pp !node.key picked
      (List.length !node.preds)); *)

let get_singleton_c_stk_exn state =
  let stop (node : Gate.Node_ref.t) =
    let picked =
      if List.is_empty !node.preds then
        true
      else
        List.fold !node.preds ~init:false ~f:(fun acc edge ->
            let this =
              Option.value_map edge.label_cvar ~default:true ~f:(fun cvar ->
                  Option.value
                    (Hashtbl.find state.cvar_picked_map cvar)
                    ~default:false)
            in
            acc || this)
    in
    not picked
  in
  let done_c_stk_set = Hash_set.create (module Concrete_stack) in
  let at_node (node : Gate.Node_ref.t) =
    match !node.rule with
    | Done c_stk -> Hash_set.add done_c_stk_set c_stk
    | _ -> ()
  in
  Gate.traverse_graph ~stop ~at_node ~init:()
    ~acc_f:(fun _ _ -> ())
    state.root_node;
  Logs.app (fun m ->
      m "C_stk: %a"
        (Fmt.Dump.list Concrete_stack.pp)
        (Hash_set.to_list done_c_stk_set));
  if Hash_set.length done_c_stk_set = 1 then
    List.hd_exn (Hash_set.to_list done_c_stk_set)
  else if Hash_set.length done_c_stk_set = 0 then
    Concrete_stack.empty
  else
    failwith "Incorrect c_stk set."

(* Frontiers are the line (a collection of nodes) on the DAG to seperated
   the visited nodes and unvisited nodes.
   The initial fronter is the root.

   Each invocation of this function is to march all frontiers towards the leaf.

   If the any frontier reaches the leaf thus making a complete path, it's
   the time to quit this march with true and check for a solution.

   Any frontiers reaching the leaf will be removed, to avoid duplicate checking.

   If no frontiers reaches the leaf in this mark, it returns a false.

   The map for visited_node is used to ensure each node won't be visited more
   than once. We may directly use a field in the node, instead.
*)

let march_frontiers state =
  let new_pendings = Hash_set.create (module Gate.Node_ref) in
  let new_dones = Hash_set.create (module Gate.Node_ref) in
  Hash_set.filter_inplace state.current_pendings ~f:(fun node_ref ->
      let node = !node_ref in
      match node.rule with
      | Pending -> true
      | Done _c_stk ->
          if not node.all_path_searched then (
            node.all_path_searched <- true;
            Hash_set.add new_dones node_ref)
          else
            ();
          false
      | Discard next | Alias next ->
          Hash_set.add new_pendings next;
          false
      | Mismatch -> false
      (* | Binop (nr1, nr2) ->
             List.iter [ nr1; nr2 ] ~f:(fun nr -> Hash_set.add new_pendings nr);
             false
         | Cond_choice (nc, nr) ->
             List.iter [ nc; nr ] ~f:(fun nr -> Hash_set.add new_pendings nr);
             false
         | Callsite (nf, ncs, _) ->
             List.iter (nf :: ncs) ~f:(fun nr -> Hash_set.add new_pendings nr);
             false *)
      | _ -> true);
  true
