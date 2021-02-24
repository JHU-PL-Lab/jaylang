open Core

type t

type gate_id = int
[@@deriving show {with_path = false}]

type node = 
  | Pending
  | Rule of search_info * rule_info
and search_info = 
  Id.t * Lookup_stack.t * Relative_stack.t
and rule_info =
  | Done of Concrete_stack.t
  | Mismatch

  | Discard of node ref
  | Alias of node ref
  | To_first of node ref

  | Binop of node ref * node ref
  | Cond_choice of node ref * node ref

  | Callsite of node ref * gate_id * node ref list
  | Condsite of node ref * gate_id * node ref list

  | Para_local of gate_id * (node ref * node ref) list
  | Para_nonlocal of gate_id * node ref list
[@@deriving show {with_path = false}]

let pending_node = Pending
let done_ si cstk = Rule(si, Done cstk)
let discard si node = Rule(si, Discard node)
let mismatch si = Rule(si, Mismatch)
let alias si node = Rule(si, Alias node)
let to_first si node = Rule(si, To_first node)
let binop si n1 n2 = Rule(si, Binop(n1,n2))
let callsite si nf gid nrs = Rule(si, Callsite(nf,gid,nrs))
let condsite si nc gid nrs = Rule(si, Condsite(nc,gid,nrs))
let para_local si gid ncs = Rule(si, Para_local(gid,ncs))
let para_nonlocal si gid ncs = Rule(si, Para_nonlocal(gid,ncs))
let cond_choice si nc nr = Rule(si, Cond_choice(nc,nr))

let deref_list nr = List.map nr ~f:Ref.(!)
let deref_pair_list nr = List.map nr ~f:(fun (x,y) -> !x,!y)

let pp_compact ?choice_answers () fmter node =
  let open Fmt in 
  let pp_choice oc i = 
    match choice_answers with
    | Some choice_answers -> 
      let state = if List.nth_exn choice_answers i then " = on" else "" in
      pf oc "{$c%d%s}" i state
    | None -> pf oc "{$c%d}" i
  in
  let rec pp_this fmter node =
    let pp_nodes = Std.pp_with_seq ~pp_int:pp_choice pp_this in
    let pp_node_pairs = Std.pp_with_seq ~pp_int:pp_choice (pair ~sep:(cut ++ (any "& ")) pp_this pp_this) in
    match node with
    | Pending -> pf fmter "Pending"
    | Rule ((x,xs,rstk), nd) ->
      pf fmter "@[<v 2>";
      begin
        let x0 = x :: xs in
        match nd with
        | Done cstk -> pf fmter "%a (done %a); %a" Id.pp_list x0 Concrete_stack.pp cstk Relative_stack.pp rstk
        | Mismatch -> pf fmter "%a (mismatch); %a" Id.pp_list x0 Relative_stack.pp rstk

        | Discard child -> pf fmter "%a (discard); %a@;%a" Id.pp_list x0 Relative_stack.pp rstk pp_this !child
        | Alias child -> pf fmter "%a (alias); %a@,%a" Id.pp_list x0 Relative_stack.pp rstk pp_this !child
        | To_first child -> pf fmter "%a (to_first); %a@,%a" Id.pp_list x0 Relative_stack.pp rstk pp_this !child

        | Binop(n1,n2) -> pf fmter "%a (binop); %a@ #b1:%a@ #b2:%a" Id.pp_list x0 Relative_stack.pp rstk pp_this !n1 pp_this !n2
        | Cond_choice(nc,nr) -> pf fmter "%a (condtop); %a@,#c:%a@,#r:%a" Id.pp_list x0 Relative_stack.pp rstk pp_this !nc pp_this !nr

        | Callsite(nf,gid,ncs) -> 
          let ncs_with_seq = Std.with_seq ~start:gid (deref_list ncs) in
          pf fmter "%a (callsite %d); %a@,%a@,%a" Id.pp_list x0 gid Relative_stack.pp rstk pp_this !nf pp_nodes ncs_with_seq
        | Condsite(nc,gid,ncs) -> 
          let ncs_with_seq = Std.with_seq ~start:gid (deref_list ncs) in
          pf fmter "%a (condsite %d); %a@,%a@,%a" Id.pp_list x0 gid Relative_stack.pp rstk pp_this !nc pp_nodes ncs_with_seq

        | Para_local(gid,ncs) -> 
          let ncps = Std.with_seq ~start:gid (deref_pair_list ncs) in
          pf fmter "%a (para_local %d); %a@,%a" Id.pp_list x0 gid Relative_stack.pp rstk pp_node_pairs ncps

        | Para_nonlocal(gid,ncs) -> 
          let ncs_with_seq = Std.with_seq ~start:gid (deref_list ncs) in
          pf fmter "%a (para_nonlocal %d); %a@,%a" Id.pp_list x0 gid Relative_stack.pp rstk pp_nodes ncs_with_seq
      end;
      pf fmter "@]"
  in
  pp_this fmter node

(* apparently, we have these two approaches to encode gates.
   if we model a gate as a 1-in-n-out xor switch, then we have two kinds of control
   1. close the in-port
   2. close the out-port

   when mapping the gate with the constraint, 
   1. the in-port maps to the whole constraint
   2. the out-ports map to the control variables
*)

(* the in-port approach *)
let rec gate_state = function
  | Pending -> false, []
  | Rule (_si, rule_info) -> begin
      match rule_info with
      | Done _ -> true, []
      | Mismatch -> false, []

      | Discard nr
      | Alias nr
      | To_first nr -> gate_state !nr

      | Binop (nr1, nr2) -> fold true (&&) [nr1;nr2]
      | Cond_choice(nc,nr) -> fold true (&&) [nc;nr]

      | Callsite(nfun, gid, ncs) ->
        let fun_done, fun_gates = gate_state !nfun in
        let sub_done, sub_gates = fold_gid false (||) gid ncs in
        fun_done && sub_done, fun_gates @ sub_gates
      | Condsite(nc, gid, ncs) ->
        let cv_done, cv_gates = gate_state !nc in
        let cb_done, cb_gates = fold_gid false (||) gid ncs in
        cv_done && cb_done, cv_gates @ cb_gates

      | Para_local(gid, ncs) ->
        List.foldi ncs ~init:(false, []) ~f:(fun i (acc_done, acc_xs) (fun_node, arg_node) ->
            let fun_done, fun_gates = gate_state !fun_node in
            let arg_done, arg_gates = gate_state !arg_node in
            let this_done = fun_done && arg_done in
            acc_done || this_done, (gid+i, this_done)::fun_gates @ arg_gates @ acc_xs
          )
      | Para_nonlocal(gid, ncs) ->
        fold_gid false (||) gid ncs
    end

and fold linit lop ns = 
  List.fold ns ~init:(linit, []) ~f:(fun (acc_done, acc_xs) child ->
      let is_done, xs = gate_state !child in
      lop acc_done is_done, xs @ acc_xs
    )
and fold_gid linit lop gid ns = 
  List.foldi ns ~init:(linit, []) ~f:(fun i (acc_done, acc_xs) child ->
      let is_done, xs = gate_state !child in
      lop acc_done is_done, (gid+i, is_done)::xs @ acc_xs
    )

let sum f childs = List.sum (module Int) childs ~f:(fun child -> f !child)

let rec size = function
  | Pending -> 1
  | Rule(_, ri) -> 
    begin
      match ri with
      | Done _ 
      | Mismatch -> 1

      | Discard child
      | Alias child
      | To_first child -> 1 + size !child

      | Binop (n1,n2)
      | Cond_choice (n1,n2) -> 1 + size !n1 + size !n2

      | Callsite (node,_,childs)
      | Condsite (node,_,childs) -> 1 + size !node + sum size childs

      | Para_local (_,ncs) -> 1 + List.sum (module Int) ncs ~f:(fun (n1,n2) -> size !n1 + size !n2)
      | Para_nonlocal (_,ncs) -> 1 + sum size ncs
    end


(* the out-port approach *)
(* let rec choice_states (_,_, nd) = 
   match nd with
   | Pending -> false, []
   | Mismatch -> false, []
   | Done _ -> true, []
   | Pass child -> choice_states !child
   | And childs -> 
    List.fold childs ~init:(true, []) ~f:(fun (dones,xss) child ->
        let is_done, xs = choice_states !child in
        dones && is_done, xss @ xs
      )
   | GuardedChoice (gid, guard, choices) ->
    let guard_done, guard_xs = choice_states !guard in
    let choice_done, xss = List.foldi choices ~init:(false, guard_xs) ~f:(fun i (any_done,xss) child ->
        let is_done, xs = choice_states !child in
        any_done || is_done, (gid+i, is_done)::xs @ xss
      ) in
    guard_done && choice_done, xss
   | Choice (gid, choices) ->
    let choice_done, xss = List.foldi choices ~init:(false, []) ~f:(fun i (any_done,xss) child ->
        let is_done, xs = choice_states !child in
        any_done || is_done, (gid+i, is_done)::xs @ xss
      ) in
    choice_done, xss *)

(* 
let for_all_refs f tree_refs = 
  List.for_all tree_refs ~f:(fun c ->
      f !c
    )

let exists_refs f tree_refs = 
  List.exists tree_refs ~f:(fun c ->
      f !c
    )

let rec check_valid_tree (_,_, nd) = 
   match nd with
   | Pending -> false
   | Mismatch -> false
   | Done _ -> true
   | Pass child -> check_valid_tree !child
   | And childs ->  for_all_refs check_valid_tree childs
   | GuardedChoice (_, guard, choices) ->
    (check_valid_tree !guard)
    && (exists_refs check_valid_tree choices)
   | Choice (_, choices) ->
    exists_refs check_valid_tree choices *)

