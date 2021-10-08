open Core

module T = struct
  type t = {
    key : Lookup_key.t;
    block_id : Id.t;
    rule : (rule[@ignore]);
    mutable preds : (edge list[@ignore]);
    mutable has_complete_path : bool;
    mutable all_path_searched : bool;
  }

  and edge = { pred : t ref; succ : t ref; label_cvar : Cvar.t option }

  and edge_with_cvar = Cvar.t * t ref

  and edges_with_cvar = Cvar.t * t ref * t ref

  and rule =
    (* special rule *)
    | Pending
    (* value rule *)
    | Done of Concrete_stack.t
    | Mismatch
    | Discard of t ref
    | Alias of t ref
    | To_first of t ref
    | Binop of t ref * t ref
    | Cond_choice of t ref * t ref
    | Condsite of t ref * edge_with_cvar list
    | Callsite of t ref * edge_with_cvar list * (Cvar.cf[@ignore])
    | Para_local of edges_with_cvar list * (Cvar.fc[@ignore])
    | Para_nonlocal of edges_with_cvar list * (Cvar.fc[@ignore])
  [@@deriving sexp, compare, equal, show { with_path = false }]

  let rule_name = function
    | Pending -> "Pending"
    | Done _ -> "Done"
    | Mismatch -> "Mismatch"
    | Discard _ -> "Discard"
    | Alias _ -> "Alias"
    | To_first _ -> "To_first"
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
    type t = Node.t ref
    [@@deriving sexp, compare, equal, show { with_path = false }]

    let hash = Hashtbl.hash
  end

  include T
  include Comparator.Make (T)
end

open Node

let mk_edge ?cvar pred succ = { pred; succ; label_cvar = cvar }

let root_node block_id x =
  {
    block_id;
    key = Lookup_key.start x;
    rule = Pending;
    preds = [];
    has_complete_path = false;
    all_path_searched = false;
  }

let mk_node ~block_id ~key ~rule =
  {
    block_id;
    key;
    rule;
    preds = [];
    has_complete_path = false;
    all_path_searched = false;
  }

let add_pred node pred =
  if
    List.mem !node.preds pred ~equal:(fun eg1 eg2 ->
        phys_equal eg1.pred eg2.pred)
  then
    () (* failwith "why duplicate cvars on edge" *)
  else
    !node.preds <- pred :: !node.preds

let mk_callsite ~fun_tree ~sub_trees ~cf = Callsite (fun_tree, sub_trees, cf)

let mk_condsite ~cond_var_tree ~sub_trees = Condsite (cond_var_tree, sub_trees)

let mk_para ~sub_trees ~fc = Para_local (sub_trees, fc)

let pending_node = Pending

let done_ cstk = Done cstk

let discard node = Discard node

let mismatch = Mismatch

let alias node = Alias node

let to_first node = To_first node

let binop n1 n2 = Binop (n1, n2)

let cond_choice nc nr = Cond_choice (nc, nr)

(*
   cvars is actually some real or virtual out-edges of a node.
   In node-based-recursive function, it's OK to set the cvar for
   the node associated with that edge
*)

let bubble_up_complete cvar_map coming_edge node =
  let changed_cvars = ref [] in
  let change_cvar cvar =
    Logs.info (fun m -> m "collect %s" (Cvar.print cvar));
    Hashtbl.set cvar_map ~key:cvar ~data:true;
    changed_cvars := cvar :: !changed_cvars
  in
  let collect_in_cvar_edges edges =
    List.iter edges ~f:(fun (cvar, tree) ->
        if !tree.has_complete_path then
          change_cvar cvar
        else
          ());
    List.exists edges ~f:(fun (_, tree) -> !tree.has_complete_path)
  in
  let rec bubble_up coming_edge (node : Node_ref.t) =
    let coming_node = coming_edge.succ in
    let coming_cvar = coming_edge.label_cvar in
    Logs.info (fun m ->
        m "B: %a [%a->%a]%a" Lookup_key.pp !node.key Lookup_key.pp
          !(coming_edge.succ).key Lookup_key.pp !(coming_edge.pred).key
          (Fmt.Dump.option Cvar.pp_print)
          coming_cvar);
    (* bubble_up *)
    let can_mark_complete =
      match !node.rule with
      | Pending -> false
      | Mismatch -> failwith "should not be in bubble up"
      | Done _ | Discard _ | Alias _ | To_first _ -> true
      | Binop (t1, t2) -> !t1.has_complete_path && !t2.has_complete_path
      | Cond_choice (t1, t2) -> !t1.has_complete_path && !t2.has_complete_path
      | Condsite (nc, nbs) ->
          if phys_equal coming_node nc then
            collect_in_cvar_edges nbs
          else if !nc.has_complete_path then (
            change_cvar (Option.value_exn coming_cvar);
            true)
          else
            false
      | Callsite (nf, nts, _) ->
          if phys_equal coming_node nf then
            collect_in_cvar_edges nts
          else if !nf.has_complete_path then (
            change_cvar (Option.value_exn coming_cvar);
            true)
          else
            false
      | Para_local (nts, _) | Para_nonlocal (nts, _) ->
          let coming_cvar = Option.value_exn coming_cvar in
          let collect_this_cvar =
            List.exists nts ~f:(fun (cvar, t1, t2) ->
                Cvar.equal coming_cvar cvar
                && !t1.has_complete_path && !t2.has_complete_path)
          in
          if collect_this_cvar then (
            change_cvar coming_cvar;
            true)
          else
            false
    in
    Logs.info (fun m ->
        m "B: %B,%B,%d" !node.has_complete_path can_mark_complete
          (List.length !node.preds));
    if !node.has_complete_path then
      ()
    else if can_mark_complete then (
      !node.has_complete_path <- true;
      List.iter !node.preds ~f:(fun edge -> bubble_up edge edge.pred))
    else
      ()
  in
  bubble_up coming_edge node;
  !changed_cvars

let traverse_node ?(stop = fun _ -> false) ~at_node ~init ~acc_f node =
  let rec loop ~acc node =
    let is_stop = stop node in
    if is_stop then
      ()
    else (
      (* visit this node *)
      at_node node;
      let acc = acc_f acc node in
      (* traverse its children *)
      match !node.rule with
      | Pending | Done _ | Mismatch -> ()
      | Discard child | Alias child | To_first child -> loop ~acc child
      | Binop (n1, n2) | Cond_choice (n1, n2) ->
          List.iter ~f:(loop ~acc) [ n1; n2 ]
      | Callsite (node, child_edges, _) | Condsite (node, child_edges) ->
          loop ~acc node;
          List.iter ~f:(fun (_, n) -> loop ~acc n) child_edges
      | Para_local (ncs, _) | Para_nonlocal (ncs, _) ->
          List.iter
            ~f:(fun (_, n1, n2) -> List.iter ~f:(loop ~acc) [ n1; n2 ])
            ncs)
  in
  loop ~acc:init node

let fold_tree ?(stop = fun _ -> false) ~init ~sum node =
  let rec loop ~acc node =
    let is_stop = stop node in
    if is_stop then
      ()
    else (* fold this node *)
      let acc = sum acc node in
      (* fold its children *)
      match !node.rule with
      | Pending | Done _ | Mismatch -> acc
      | Discard child | Alias child | To_first child -> loop ~acc child
      | Binop (n1, n2) | Cond_choice (n1, n2) ->
          List.fold ~init:acc ~f:sum [ n1; n2 ]
      | Callsite (node, child_edges, _) | Condsite (node, child_edges) ->
          let acc = sum acc node in
          List.fold ~init:acc ~f:(fun acc (_, n) -> sum acc n) child_edges
      | Para_local (ncs, _) | Para_nonlocal (ncs, _) ->
          List.fold ~init:acc
            ~f:(fun acc (_, n1, n2) -> sum (sum acc n1) n2)
            ncs
  in
  loop ~acc:init node
