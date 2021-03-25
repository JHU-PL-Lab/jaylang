open Core

type t

type gate_id = int [@@deriving show { with_path = false }]

type node = Pending | Proxy of node ref | Rule of search_info * rule_info

and search_info = Id.t * Lookup_stack.t * Relative_stack.t

and rule_info =
  | Done of Concrete_stack.t
  | Mismatch
  | Discard of node ref
  | Alias of node ref
  | To_first of node ref
  | Binop of node ref * node ref
  | Cond_choice of node ref * node ref
  | Callsite of node ref * gate_id * node ref list * Constraint.cf
  | Condsite of node ref * gate_id * node ref list
  | Para_local of gate_id * (node ref * node ref) list * Constraint.fc
  | Para_nonlocal of gate_id * node ref list * Constraint.fc
[@@deriving show { with_path = false }]

type choice_switch = Pre of bool | Post of bool option
[@@deriving show { with_path = false }]

let pending_node = Pending

let done_ si cstk = Rule (si, Done cstk)

let discard si node = Rule (si, Discard node)

let mismatch si = Rule (si, Mismatch)

let alias si node = Rule (si, Alias node)

let to_first si node = Rule (si, To_first node)

let binop si n1 n2 = Rule (si, Binop (n1, n2))

let callsite si nf gid nrs cf = Rule (si, Callsite (nf, gid, nrs, cf))

let condsite si nc gid nrs = Rule (si, Condsite (nc, gid, nrs))

let para_local si gid ncs fc = Rule (si, Para_local (gid, ncs, fc))

let para_nonlocal si gid ncs fc = Rule (si, Para_nonlocal (gid, ncs, fc))

let cond_choice si nc nr = Rule (si, Cond_choice (nc, nr))

let deref_list nr = List.map nr ~f:Ref.( ! )

let deref_pair_list nr = List.map nr ~f:(fun (x, y) -> (!x, !y))

let cvar_name_cores = function
  | Rule ((x, _xs, rel_stk), rule_info) -> (
      match rule_info with
      | Callsite (_, _, _, cf) ->
          List.map cf.ins ~f:(fun in_ ->
              Constraint.mk_cvar_core ~from_id:cf.f_out ~to_id:in_.fun_in
                ~site:(Some cf.site) cf.stk_out)
      | Condsite (_, _, _) ->
          List.map [ true; false ] ~f:(fun beta ->
              Constraint.mk_cvar_cond_core ~site:x ~beta rel_stk)
      | Para_local (_, _, fc) | Para_nonlocal (_, _, fc) ->
          List.map fc.outs ~f:(fun out ->
              Constraint.mk_cvar_core ~from_id:fc.fun_in ~to_id:out.f_out
                ~site:None fc.stk_in)
      | _ -> failwith "n/a")
  | _ -> failwith "n/a"

let pp_compact ?cc ?ccpp () fmter node =
  let open Fmt in
  let pp_choice oc cname =
    pf oc "{ %s : " cname;
    (match cc with
    | Some cc ->
        let b1 = List.Assoc.find_exn ~equal:String.equal cc cname in
        pf oc "%B" b1
    | None -> ());
    (match ccpp with
    | Some ccpp ->
        let b1, b2 = List.Assoc.find_exn ~equal:String.equal ccpp cname in
        let s2 =
          match b2 with
          | Some true -> " - picked"
          | Some false -> " - not picked"
          | None -> " - "
        in
        pf oc "%B,%s" b1 s2
    | None -> ());
    pf oc "}"
  in
  let pp_with_key ~pp_key pp_x oc xps =
    Fmt.(pf oc "%a" (list ~sep:(any "@,") (pair ~sep:sp pp_key pp_x)) xps)
  in
  let rec pp_this fmter node =
    let pp_nodes = pp_with_key ~pp_key:pp_choice pp_this in
    let pp_node_pairs =
      pp_with_key ~pp_key:pp_choice
        (pair ~sep:(cut ++ any "& ") pp_this pp_this)
    in
    match node with
    | Pending -> pf fmter "Pending"
    | Proxy _ -> pf fmter "Proxy"
    | Rule ((x, xs, rstk), nd) ->
        pf fmter "@[<v 2>";
        (let x0 = x :: xs in
         match nd with
         | Done cstk ->
             pf fmter "%a (done %a); %a" Id.pp_list x0 Concrete_stack.pp cstk
               Relative_stack.pp rstk
         | Mismatch ->
             pf fmter "%a (mismatch); %a" Id.pp_list x0 Relative_stack.pp rstk
         | Discard child ->
             pf fmter "%a (discard); %a@;%a" Id.pp_list x0 Relative_stack.pp
               rstk pp_this !child
         | Alias child ->
             pf fmter "%a (alias); %a@,%a" Id.pp_list x0 Relative_stack.pp rstk
               pp_this !child
         | To_first child ->
             pf fmter "%a (to_first); %a@,%a" Id.pp_list x0 Relative_stack.pp
               rstk pp_this !child
         | Binop (n1, n2) ->
             pf fmter "%a (binop); %a@ #b1:%a@ #b2:%a" Id.pp_list x0
               Relative_stack.pp rstk pp_this !n1 pp_this !n2
         | Cond_choice (nc, nr) ->
             pf fmter "%a (condtop); %a@,#c:%a@,#r:%a" Id.pp_list x0
               Relative_stack.pp rstk pp_this !nc pp_this !nr
         | Callsite (nf, gid, ncs, _cf) ->
             let cvars = cvar_name_cores node in
             let nodes = deref_list ncs in
             pf fmter "%a (callsite %d); %a@,%a@,%a" Id.pp_list x0 gid
               Relative_stack.pp rstk pp_this !nf pp_nodes
               (List.zip_exn cvars nodes)
         | Condsite (nc, gid, ncs) ->
             let cvars = cvar_name_cores node in
             let nodes = deref_list ncs in
             pf fmter "%a (condsite %d); %a@,%a@,%a" Id.pp_list x0 gid
               Relative_stack.pp rstk pp_this !nc pp_nodes
               (List.zip_exn cvars nodes)
         | Para_local (gid, ncs, _fc) ->
             let cvars = cvar_name_cores node in
             let nodes = deref_pair_list ncs in
             pf fmter "%a (para_local %d); %a@,%a" Id.pp_list x0 gid
               Relative_stack.pp rstk pp_node_pairs (List.zip_exn cvars nodes)
         | Para_nonlocal (gid, ncs, _fc) ->
             let cvars = cvar_name_cores node in
             let nodes = deref_list ncs in
             pf fmter "%a (para_nonlocal %d); %a@,%a" Id.pp_list x0 gid
               Relative_stack.pp rstk pp_nodes (List.zip_exn cvars nodes));
        pf fmter "@]"
  in
  pp_this fmter node

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
  match node with
  | Pending -> (false, [])
  | Proxy tree -> gate_state !tree
  | Rule (_, rule_info) -> (
      match rule_info with
      | Done _ -> (true, [])
      | Mismatch -> (false, [])
      | Discard nr | Alias nr | To_first nr -> gate_state !nr
      | Binop (nr1, nr2) -> fold true ( && ) [ nr1; nr2 ]
      | Cond_choice (nc, nr) -> fold true ( && ) [ nc; nr ]
      | Callsite (nfun, _, ncs, _) ->
          let (fun_done : bool), fun_gates = gate_state !nfun in
          let cvars = cvar_name_cores node in
          let sub_done, sub_gates =
            List.fold2_exn ncs cvars ~init:(false, [])
              ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
                let sub_done, sub_xs = gate_state !sub_tree in
                (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs))
          in
          (fun_done && sub_done, fun_gates @ sub_gates)
      | Condsite (nc, _, ncs) ->
          let cv_done, cv_gates = gate_state !nc in
          let cvars = cvar_name_cores node in
          let cb_done, cb_gates =
            List.fold2_exn ncs cvars ~init:(false, [])
              ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
                let sub_done, sub_xs = gate_state !sub_tree in
                (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs))
          in
          (cv_done && cb_done, cv_gates @ cb_gates)
      | Para_local (_, ncs, _) ->
          let cvars = cvar_name_cores node in
          List.fold2_exn ncs cvars ~init:(false, [])
            ~f:(fun (acc_done, acc_xs) (fun_node, arg_node) cvar ->
              let fun_done, fun_gates = gate_state !fun_node in
              let arg_done, arg_gates = gate_state !arg_node in
              let this_done = fun_done && arg_done in
              ( acc_done || this_done,
                (cvar, this_done) :: fun_gates @ arg_gates @ acc_xs ))
      | Para_nonlocal (_, ncs, _) ->
          let cvars = cvar_name_cores node in
          List.fold2_exn ncs cvars ~init:(false, [])
            ~f:(fun (acc_done, acc_xs) sub_tree cvar ->
              let sub_done, sub_xs = gate_state !sub_tree in
              (acc_done || sub_done, (cvar, sub_done) :: sub_xs @ acc_xs)))

and fold linit lop ns =
  List.fold ns ~init:(linit, []) ~f:(fun (acc_done, acc_xs) child ->
      let is_done, xs = gate_state !child in
      (lop acc_done is_done, xs @ acc_xs))

(* and fold_gid linit lop gid ns =
  List.foldi ns ~init:(linit, []) ~f:(fun i (acc_done, acc_xs) child ->
      let is_done, xs = gate_state !child in
      (lop acc_done is_done, (gid + i, is_done) :: xs @ acc_xs)) *)

let gate_state tree =
  let t, ccs = gate_state tree in
  let ccs_unique =
    List.dedup_and_sort
      ~compare:(fun (n1, _) (n2, _) -> String.compare n1 n2)
      ccs
  in
  (t, ccs_unique)

(* let chosen_path gates tree = 
   let rec loop = function
    | Pending -> None
    | Rule(_, ri) -> begin
        match ri with
        | Done _ ->  Some []
        | Mismatch -> failwith "chosen_path: should not reach Mismatch"

        | Discard nr
        | Alias nr
        | To_first nr -> loop !nr

        | Binop (n1,n2)
        | Cond_choice(n1,n2) -> 
          Option.map 
            (Option.all [loop !n1; loop !n2])
            ~f:List.join

        | Callsite(g, gid, choices)
        | Condsite(g, gid, choices) ->
          Option.all [ 
            (loop !g);
            (List.find_mapi choices ~f:(fun i arg ->
                 if List.nth_exn gates (gid+i) then
                   loop !arg
                   |> Option.map ~f:(List.cons (gid+i))
                 else
                   None
               ))]
          |> Option.map ~f:List.join

        | Para_local(gid, choices) ->
          Option.all [
            List.find_mapi choices ~f:(fun i (f,arg) ->
                if List.nth_exn gates (gid+i) then
                  Option.all [
                    loop !f;
                    loop !arg;                  
                    Some [gid+i]
                  ] |> Option.map ~f:List.join
                else
                  None)
          ]
          |> Option.map ~f:List.join

        | Para_nonlocal(gid, choices) ->
          List.find_mapi choices ~f:(fun i arg ->
              if List.nth_exn gates (gid+i) then
                loop !arg
                |> Option.map ~f:(List.cons (gid+i))
              else
                None
            )
      end
   in
   loop tree |> Option.value_exn *)

let sum f childs = List.sum (module Int) childs ~f:(fun child -> f !child)

let rec size = function
  | Pending -> 1
  | Proxy _ -> 1
  | Rule (_, ri) -> (
      match ri with
      | Done _ | Mismatch -> 1
      | Discard child | Alias child | To_first child -> 1 + size !child
      | Binop (n1, n2) | Cond_choice (n1, n2) -> 1 + size !n1 + size !n2
      | Callsite (node, _, childs, _) | Condsite (node, _, childs) ->
          1 + size !node + sum size childs
      | Para_local (_, ncs, _) ->
          1 + List.sum (module Int) ncs ~f:(fun (n1, n2) -> size !n1 + size !n2)
      (* | Para_nonlocal (_,ncs) -> 1 + List.sum (module Int) ncs ~f:(fun (n1,n2) -> size !n1 + size !n2) *)
      | Para_nonlocal (_, ncs, _) -> 1 + sum size ncs)
