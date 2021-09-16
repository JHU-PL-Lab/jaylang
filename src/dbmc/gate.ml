open Core

module T = struct
  type t = {
    key : Lookup_key.t;
    block_id : Id.t;
    rule : rule;
    mutable preds : edge list;
    mutable has_complete_path : bool;
    mutable all_path_searched : bool;
  }

  and edge = { pred : t ref; succ : t ref; label_cvar : Cvar.t option }

  and edge_with_cvar = Cvar.t * t ref

  and edges_with_cvar = Cvar.t * t ref * t ref

  and rule =
    (* special rule *)
    | Pending
    | To_visited of t ref
    (* value rule *)
    | Done of Concrete_stack.t
    | Mismatch
    | Discard of t ref
    | Alias of t ref
    | To_first of t ref
    | Binop of t ref * t ref
    | Cond_choice of t ref * t ref
    | Condsite of t ref * edge_with_cvar list
    | Callsite of t ref * edge_with_cvar list * Helper.cf
    | Para_local of edges_with_cvar list * Helper.fc
    | Para_nonlocal of edges_with_cvar list * Helper.fc
  [@@deriving sexp, compare, equal, show { with_path = false }]

  let rule_name = function
    | Pending -> "Pending"
    | To_visited _ -> "To_visited"
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

(* let direct pred succ = { pred; succ; label_cvar = None }

   let with_cvar cvar pred succ = { pred; succ; label_cvar = Some cvar } *)

let mk_edge ?cvar pred succ = { pred; succ; label_cvar = cvar }

let root_node block_id x =
  {
    block_id;
    key = (x, [], Relative_stack.empty);
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
    (* match (eg1, eg2) with
       | With_cvar (cvar1, _p1), With_cvar (cvar2, _p2) ->
           Cvar.equal cvar1 cvar2
       | Direct n1, Direct n2 -> phys_equal n1 n2
       | _, _ -> false) *)
  then
    () (* failwith "why duplicate cvars on edge" *)
  else
    !node.preds <- pred :: !node.preds

let mk_callsite ~fun_tree ~sub_trees ~cf = Callsite (fun_tree, sub_trees, cf)

let mk_condsite ~cond_var_tree ~sub_trees = Condsite (cond_var_tree, sub_trees)

let mk_para ~sub_trees ~fc = Para_local (sub_trees, fc)

let pending_node = Pending

(* let to_visited node = To_visited node *)

let done_ cstk = Done cstk

let discard node = Discard node

let mismatch = Mismatch

let alias node = Alias node

let to_first node = To_first node

let binop n1 n2 = Binop (n1, n2)

let cond_choice nc nr = Cond_choice (nc, nr)

let deref_list nr = List.map nr ~f:Ref.( ! )

let deref_pair_list nr = List.map nr ~f:(fun (x, y) -> (!x, !y))

(*
   cvars is actually some real or virtual out-edges of a node.
   In node-based-recursive function, it's OK to set the cvar for
   the node associated with that edge

   visited_map works as memo for node via node.key
   cvar_map remembers all cvar_core

   Noting visited_map works for all nodes, while
   cvar_map just works for some edges.
   it's a workaround to put cvar as an optional argument,
   and that's why we first check visited_map and then check cvar_map
*)

(* let cvar_cores_of_node node =
   let x, xs, r_stk = node.key in
   let lookups = x :: xs in
   match node.rule with
   | Callsite (_, _, cf) -> Cvar.mk_callsite_to_fun lookups cf
   | Condsite (_, _) -> Cvar.mk_condsite lookups x r_stk
   | Para_local (_, fc) | Para_nonlocal (_, fc) ->
       Cvar.mk_fun_to_callsite lookups fc
   | _ -> [] *)

(*
     visited_map:
     true -> exist one done path
     false -> no done path

     A -> B -> C -> A
        \        -> D ([])
          E -> F -> G (..)
*)

let get_c_vars_and_complete cvar_map node (* visited_list *) =
  Hashtbl.clear cvar_map;
  (* let local_cvar_map = Hashtbl.create (module String) in *)
  let post_and x y = x && y in
  let post_or x y = x || y in
  (* let post_and x y =
       match (x, y) with
       | x, Std.Unknown -> x
       | Std.Unknown, y -> y
       | Std.True, Std.True -> Std.True
       | _, _ -> Std.False
     in
     let post_or x y =
       match (x, y) with
       | x, Std.Unknown -> x
       | Std.Unknown, y -> y
       | Std.False, Std.False -> Std.False
       | _, _ -> Std.True
     in *)
  let visited_map = Hashtbl.create (module Lookup_key) in
  (* let visited_map_debug = Hashtbl.create (module Lookup_key) in *)
  let rec loop ?cvar node =
    let done_ =
      match Hashtbl.find visited_map node.key with
      | Some done_ -> done_
      | None ->
          (* ignore @@ Hashtbl.add_exn visited_map ~key:node.key ~data:false; *)
          (* let cvars = cvar_cores_of_node node in *)
          let done_ =
            match node.rule with
            | Pending -> false
            | Done _ -> true
            | Mismatch -> false
            | To_visited next -> loop !next
            | Discard next | Alias next | To_first next -> loop !next
            | Binop (nr1, nr2) ->
                List.fold [ nr1; nr2 ] ~init:true ~f:(fun acc nr ->
                    post_and (loop !nr) acc)
            | Cond_choice (nc, nr) ->
                List.fold [ nc; nr ] ~init:true ~f:(fun acc nr ->
                    post_and (loop !nr) acc)
            | Callsite (nf, ncs, _) ->
                let sf = loop !nf in
                post_and sf
                  (List.fold ncs ~init:false ~f:(fun acc (cvar, nr) ->
                       post_or (loop ~cvar !nr) acc))
            | Condsite (nc, ncs) ->
                post_and (loop !nc)
                  (List.fold ncs ~init:false ~f:(fun acc (cvar, nr) ->
                       (* if not (Cvar.equal cvar0 cvar) then
                            Logs.app (fun m ->
                                m "[%a][%a]\n" Cvar.pp cvar0 Cvar.pp cvar)
                          else
                            (); *)
                       post_or (loop ~cvar !nr) acc))
            | Para_local (ncs, _) ->
                List.fold ncs ~init:false ~f:(fun acc (cvar, nf, na) ->
                    let this_done = post_and (loop !nf) (loop !na) in
                    (* special case for cvar since this cvar is not a real edge *)
                    ignore @@ Hashtbl.add cvar_map ~key:cvar ~data:this_done;
                    post_or this_done acc)
            | Para_nonlocal (ncs, _) ->
                List.fold ncs ~init:false ~f:(fun acc (cvar, nf, na) ->
                    let this_done = post_and (loop !nf) (loop !na) in
                    (* special case for cvar since this cvar is not a real edge *)
                    ignore @@ Hashtbl.add cvar_map ~key:cvar ~data:this_done;
                    post_or this_done acc)
          in
          (* Hashtbl.change visited_map node.key ~f:(function
             | Some Std.Unknown -> Some done_
             | Some _ -> failwith "why not Unknown"
             | None -> failwith "why None"); *)
          (* (match this_key with
             | Some key -> Logs.app (fun m -> m "(%B)%a" done_ Lookup_key.pp key)
             | None -> ()); *)
          Hashtbl.set visited_map ~key:node.key ~data:done_;
          (* (match Hashtbl.add visited_map ~key:node.key ~data:done_ with
             | `Ok -> ()
             | `Duplicate ->
                 Logs.app (fun m ->
                     m
                       "Search tree node_map circular dependency at\n\
                        \tkey:%a\told_val:%B\tnew_val:%B\t"
                       Lookup_key.pp node.key
                       (Hashtbl.find_exn visited_map node.key)
                       done_)); *)
          done_
    in
    (match cvar with
    | Some cvar -> (
        (* Hashtbl.set cvar_map ~key:cvar ~data:done_ *)
        match Hashtbl.add cvar_map ~key:cvar ~data:done_ with
        | `Ok -> ()
        | `Duplicate ->
            ()
            (* Logs.warn (fun m ->
                m
                  "Search tree cvar_map duplication at\n\
                   \tkey(cvar):%s\told_val:%a\tnew_val:%a\t"
                  cvar Std.pp_ternary
                  (Hashtbl.find_exn cvar_map cvar)
                  Std.pp_ternary done_ *)
            (* Hashtbl.change cvar_map cvar ~f:(function
               | Some Std.Unknown | None -> Some done_
               | Some v when Std.equal_ternary v done_ -> Some v
               | _ -> failwith "must be either unknown or consistency") *))
    | None -> ());
    (* List.iter node.cvars_coming ~f:(fun cvar ->
        Hashtbl.update cvar_map cvar ~f:(fun old_v ->
            match old_v with None -> done_ | Some ov -> ov || done_)); *)
    done_
  in
  let tdone_ = loop node in
  (* Hashtbl.iteri local_cvar_map ~f:(fun ~key ~data ->
      Hashtbl.add_exn cvar_map ~key ~data:(Std.bool_of_ternary_exn data)); *)
  (* Std.bool_of_ternary_exn tdone_ *)
  tdone_

let sum f childs = List.sum (module Int) childs ~f:(fun child -> f !child)

let rec size node =
  match node.rule with
  | Pending -> 1
  | To_visited _ -> 1
  | Done _ | Mismatch -> 1
  | Discard child | Alias child | To_first child -> 1 + size !child
  | Binop (n1, n2) | Cond_choice (n1, n2) -> 1 + size !n1 + size !n2
  | Callsite (node, child_edges, _) | Condsite (node, child_edges) ->
      let childs = List.map child_edges ~f:snd in
      1 + size !node + sum size childs
  | Para_local (ncs, _) ->
      1 + List.sum (module Int) ncs ~f:(fun (_, n1, n2) -> size !n1 + size !n2)
  | Para_nonlocal (ncs, _) ->
      1 + List.sum (module Int) ncs ~f:(fun (_, n1, n2) -> size !n1 + size !n2)

let bubble_up_complete cvar_map coming_edge node =
  (* let _visited = Hash_set.create (module Node_ref) in *)
  let rec bubble_up coming_edge (node : Node_ref.t) =
    let coming_node = coming_edge.succ in
    let coming_cvar = coming_edge.label_cvar in
    Logs.app (fun m ->
        m "B: %a [%a->%a]%a" Lookup_key.pp !node.key Lookup_key.pp
          !(coming_edge.succ).key Lookup_key.pp !(coming_edge.pred).key
          (Fmt.Dump.option Cvar.pp_print)
          coming_cvar);
    (* bubble_up *)
    let collect_in_cvar_edges edges =
      List.iter edges ~f:(fun (cvar, tree) ->
          if !tree.has_complete_path then
            Hashtbl.set cvar_map ~key:cvar ~data:true
          else
            ());
      List.exists edges ~f:(fun (_, tree) -> !tree.has_complete_path)
    in
    let collect_cvar cvar =
      Logs.app (fun m -> m "collect %s" (Cvar.print cvar));
      Hashtbl.set cvar_map ~key:cvar ~data:true
    in
    let can_mark_complete =
      match !node.rule with
      | Pending -> false
      | To_visited _ | Mismatch -> failwith "should not be in bubble up"
      | Done _ | Discard _ | Alias _ | To_first _ -> true
      | Binop (t1, t2) -> !t1.has_complete_path && !t2.has_complete_path
      | Cond_choice (t1, t2) ->
          Logs.app (fun m ->
              m "[CC]%a:%B , %a:%B" Lookup_key.pp !t1.key !t1.has_complete_path
                Lookup_key.pp !t2.key !t2.has_complete_path);
          !t1.has_complete_path && !t2.has_complete_path
      | Condsite (nc, nbs) ->
          if phys_equal coming_node nc then
            collect_in_cvar_edges nbs
          else if !nc.has_complete_path then (
            collect_cvar (Option.value_exn coming_cvar);
            true)
          else
            false
      | Callsite (nf, nts, _) ->
          if phys_equal coming_node nf then
            collect_in_cvar_edges nts
          else if !nf.has_complete_path then (
            collect_cvar (Option.value_exn coming_cvar);
            true)
          else
            false
      | Para_local (nts, _) | Para_nonlocal (nts, _) ->
          let coming_cvar = Option.value_exn coming_cvar in
          let collect_this_cvar =
            List.exists nts ~f:(fun (cvar, t1, t2) ->
                Logs.app (fun m ->
                    m "%B , %a:%B , %a:%B"
                      (Cvar.equal coming_cvar cvar)
                      Lookup_key.pp !t1.key !t1.has_complete_path Lookup_key.pp
                      !t2.key !t2.has_complete_path);
                Cvar.equal coming_cvar cvar
                && !t1.has_complete_path && !t2.has_complete_path)
          in
          if collect_this_cvar then (
            collect_cvar coming_cvar;
            true)
          else
            false
    in
    Logs.app (fun m ->
        m "B: %B,%B,%d" !node.has_complete_path can_mark_complete
          (List.length !node.preds));
    if !node.has_complete_path then
      ()
    else if can_mark_complete then (
      !node.has_complete_path <- true;
      List.iter !node.preds ~f:(fun edge -> bubble_up edge edge.pred)
      (* let pred_edge = { pred }
         | Direct pred -> bubble_up (Direct node) pred
         | With_cvar (cvar, pred) -> bubble_up (With_cvar (cvar, node)) pred)) *))
    else
      ()
  in
  bubble_up coming_edge node

let traverse_graph ?(stop = fun _ -> false) ~at_node ~init ~acc_f node =
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
      | Pending | To_visited _ | Done _ | Mismatch -> ()
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
