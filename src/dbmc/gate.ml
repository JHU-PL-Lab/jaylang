open Core

module T = struct
  type t = { block_id : Id.t; key : Lookup_key.t; rule : rule }

  and rule =
    (* special rule *)
    | Pending
    | Proxy of t ref
    (* value rule *)
    | Done of Concrete_stack.t
    | Mismatch
    | Discard of t ref
    | Alias of t ref
    | To_first of t ref
    | Binop of t ref * t ref
    | Cond_choice of t ref * t ref
    | Callsite of t ref * t ref list * Constraint.cf
    | Condsite of t ref * t ref list
    | Para_local of (t ref * t ref) list * Constraint.fc
    | Para_nonlocal of t ref list * Constraint.fc
  [@@deriving sexp, compare, equal, show { with_path = false }]
end

include T
include Comparator.Make (T)

type node = t

type choice_switch = Pre of bool | Post of bool option
[@@deriving show { with_path = false }]

let rule_name = function
  | Pending -> "Pending"
  | Proxy _ -> "Proxy"
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

let dummy_start =
  let x = Id.(Ident "dummy") in
  { block_id = x; key = (x, [], Relative_stack.empty); rule = Pending }

let pending_node = Pending

let proxy node = Proxy node

let done_ cstk = Done cstk

let discard node = Discard node

let mismatch = Mismatch

let alias node = Alias node

let to_first node = To_first node

let binop n1 n2 = Binop (n1, n2)

let callsite nf nrs cf = Callsite (nf, nrs, cf)

let condsite nc nrs = Condsite (nc, nrs)

let para_local ncs fc = Para_local (ncs, fc)

let para_nonlocal ncs fc = Para_nonlocal (ncs, fc)

let cond_choice nc nr = Cond_choice (nc, nr)

let deref_list nr = List.map nr ~f:Ref.( ! )

let deref_pair_list nr = List.map nr ~f:(fun (x, y) -> (!x, !y))

let cvar_name_cores node =
  let x, _xs, r_stk = node.key in
  match node.rule with
  | Callsite (_, _, cf) ->
      List.map cf.ins ~f:(fun in_ ->
          Constraint.mk_cvar_core ~from_id:cf.f_out ~to_id:in_.fun_in
            ~site:(Some cf.site) cf.stk_out)
  | Condsite (_, _) ->
      List.map [ true; false ] ~f:(fun beta ->
          Constraint.mk_cvar_cond_core ~site:x ~beta r_stk)
  | Para_local (_, fc) | Para_nonlocal (_, fc) ->
      List.map fc.outs ~f:(fun out ->
          Constraint.mk_cvar_core ~from_id:fc.fun_in ~to_id:out.f_out ~site:None
            fc.stk_in)
  | _ -> []

(* 
apparently, we have these two approaches to encode gates.
   if we model a gate as a 1-in-n-out xor switch, then we have two kinds of control
   1. close the in-port
   2. close the out-port

   when mapping the gate with the constraint, 
   1. the in-port maps to the whole constraint
   2. the out-ports map to the control variables
*)

(* the out-port approach *)

let rec gate_state node =
  match node.rule with
  | Pending -> (false, [])
  | Proxy node -> gate_state !node
  | Done _ -> (true, [])
  | Mismatch -> (false, [])
  | Discard nr | Alias nr | To_first nr -> gate_state !nr
  | Binop (nr1, nr2) -> fold true ( && ) [ nr1; nr2 ]
  | Cond_choice (nc, nr) -> fold true ( && ) [ nc; nr ]
  | Callsite (nfun, ncs, _) ->
      let (fun_done : bool), fun_gates = gate_state !nfun in
      let cvars = cvar_name_cores node in
      let sub_done, sub_gates =
        List.fold2_exn ncs cvars ~init:(false, [])
          ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
            let sub_done, sub_xs = gate_state !sub_tree in
            (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs))
      in
      (fun_done && sub_done, fun_gates @ sub_gates)
  | Condsite (nc, ncs) ->
      let cv_done, cv_gates = gate_state !nc in
      let cvars = cvar_name_cores node in
      let cb_done, cb_gates =
        List.fold2_exn ncs cvars ~init:(false, [])
          ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
            let sub_done, sub_xs = gate_state !sub_tree in
            (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs))
      in
      (cv_done && cb_done, cv_gates @ cb_gates)
  | Para_local (ncs, _) ->
      let cvars = cvar_name_cores node in
      List.fold2_exn ncs cvars ~init:(false, [])
        ~f:(fun (acc_done, acc_xs) (fun_node, arg_node) cvar ->
          let fun_done, fun_gates = gate_state !fun_node in
          let arg_done, arg_gates = gate_state !arg_node in
          let this_done = fun_done && arg_done in
          ( acc_done || this_done,
            (cvar, this_done) :: fun_gates @ arg_gates @ acc_xs ))
  | Para_nonlocal (ncs, _) ->
      let cvars = cvar_name_cores node in
      List.fold2_exn ncs cvars ~init:(false, [])
        ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
          let sub_done, sub_xs = gate_state !sub_tree in
          (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs))

and fold linit lop ns =
  List.fold ns ~init:(linit, []) ~f:(fun (acc_done, acc_xs) child ->
      let is_done, xs = gate_state !child in
      (lop acc_done is_done, xs @ acc_xs))

let gate_state tree =
  let t, ccs = gate_state tree in
  let ccs_unique =
    List.dedup_and_sort
      ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
      ccs
  in
  (t, ccs_unique)

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
let get_c_vars_and_complete node =
  let post_and x y = x && y in
  let post_or x y = x || y in
  let visited_map = Hashtbl.create (module Lookup_key) in
  let cvar_map = Hashtbl.create (module String) in
  let rec loop ?cvar node =
    let done_ =
      match Hashtbl.find visited_map node.key with
      | Some done_ -> done_
      | None ->
          let cvars = cvar_name_cores node in
          let done_ =
            match node.rule with
            | Pending -> false
            | Done _ -> true
            | Mismatch -> false
            | Proxy next -> loop !next
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
                  (List.fold2_exn ncs cvars ~init:false ~f:(fun acc nr cvar ->
                       post_or (loop ~cvar !nr) acc))
            | Condsite (nc, ncs) ->
                post_and (loop !nc)
                  (List.fold2_exn ncs cvars ~init:false ~f:(fun acc nr cvar ->
                       post_or (loop ~cvar !nr) acc))
            | Para_local (ncs, _) ->
                List.fold2_exn ncs cvars ~init:false
                  ~f:(fun acc (nf, na) cvar ->
                    let this_done = post_and (loop !nf) (loop !na) in
                    (* special case for cvar since this cvar is not a real edge *)
                    ignore @@ Hashtbl.add cvar_map ~key:cvar ~data:this_done;
                    post_or this_done acc)
            | Para_nonlocal (ncs, _) ->
                List.fold2_exn ncs cvars ~init:false ~f:(fun acc nr cvar ->
                    post_or (loop ~cvar !nr) acc)
          in
          (match Hashtbl.add visited_map ~key:node.key ~data:done_ with
          | `Ok -> ()
          | `Duplicate ->
              Logs.app (fun m ->
                  m
                    "Search tree node_map circular dependency at\n\
                     \tkey:%a\told_val:%B\tnew_val:%B\t"
                    Lookup_key.pp node.key
                    (Hashtbl.find_exn visited_map node.key)
                    done_));
          done_
    in
    (match cvar with
    | Some cvar -> (
        match Hashtbl.add cvar_map ~key:cvar ~data:done_ with
        | `Ok -> ()
        | `Duplicate ->
            Logs.app (fun m ->
                m
                  "Search tree cvar_map duplication at\n\
                   \tkey(cvar):%s\told_val:%B\tnew_val:%B\t"
                  cvar
                  (Hashtbl.find_exn cvar_map cvar)
                  done_))
    | None -> ());
    done_
  in
  let top_done = loop node in
  (top_done, cvar_map)

let sum f childs = List.sum (module Int) childs ~f:(fun child -> f !child)

let rec size node =
  match node.rule with
  | Pending -> 1
  | Proxy _ -> 1
  | Done _ | Mismatch -> 1
  | Discard child | Alias child | To_first child -> 1 + size !child
  | Binop (n1, n2) | Cond_choice (n1, n2) -> 1 + size !n1 + size !n2
  | Callsite (node, childs, _) | Condsite (node, childs) ->
      1 + size !node + sum size childs
  | Para_local (ncs, _) ->
      1 + List.sum (module Int) ncs ~f:(fun (n1, n2) -> size !n1 + size !n2)
  | Para_nonlocal (ncs, _) -> 1 + sum size ncs
