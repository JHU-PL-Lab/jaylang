(* input *)
| At_clause { clause = Clause (_, Input_body); _ } ->
  let%lwt _ = Logs_lwt.info (fun m -> m "Rule Value (Input)") in
  Hash_set.add state.input_nodes this_key;
  deal_with_value None this_key block gate_tree

(* alias *)


(* Record Start *)
| At_clause
({ clause = Clause (_, Projection_body (Var (xr, _), lbl)); _ } as
tc) ->
Logs.info (fun m ->
    m "Rule ProjectBody : %a = %a.%a" Id.pp x Id.pp xr Id.pp lbl);
let key_r = Lookup_key.replace_x this_key x in
let node_r, lookup_r = create_task key_r block gate_tree in

let key_r_l = Lookup_key.replace_x2 this_key (xr, lbl) in
let node_r_l, lookup_r_l = create_task key_r_l block gate_tree in
Node.update_rule gate_tree (Node.project node_r node_r_l);
add_phi this_key (Riddler.alias_key this_key key_r_l);

let%lwt _ =
  Lwt_list.map_p
    (fun task -> filter_and_sequence task)
    [ lookup_r; lookup_r_l ]
in
Lookup_result.ok_lwt x
(* Binop *)
| At_clause
{
  clause =
    Clause
      (_, Binary_operation_body (Var (x1, _), bop, Var (x2, _)));
  _;
} ->
let key_x1 = Lookup_key.of_parts x1 xs r_stk in
let node_x1, lookup_x1 = create_task key_x1 block gate_tree in
let key_x2 = Lookup_key.of_parts x2 xs r_stk in
let node_x2, lookup_x2 = create_task key_x2 block gate_tree in

Node.update_rule gate_tree (Node.binop node_x1 node_x2);
add_phi this_key (Riddler.binop this_key bop x1 x2);

let%lwt _ =
  Lwt_list.map_p
    (fun task -> filter_and_sequence task)
    [ lookup_x1; lookup_x2 ]
in
Lookup_result.ok_lwt x
(* Cond Top *)
| At_chosen cb ->
let condsite_block = Tracelet.outer_block block map in
let choice = Option.value_exn cb.choice in
let condsite_stack =
  match Rstack.pop r_stk (cb.point, Id.cond_fid choice) with
  | Some stk -> stk
  | None -> failwith "impossible in CondTop"
in
let x2 = cb.cond in

let key_x2 = Lookup_key.of_parts x2 [] condsite_stack in
let node_x2, lookup_x2 =
  create_task key_x2 condsite_block gate_tree
in
let key_xxs = Lookup_key.of_parts x xs condsite_stack in
let node_xxs, lookup_xxs =
  create_task key_xxs condsite_block gate_tree
in

Node.update_rule gate_tree (Node.cond_choice node_x2 node_xxs);
add_phi this_key (Riddler.cond_top this_key cb condsite_stack);
let%lwt _ =
  Lwt_list.map_p
    (fun task -> filter_and_sequence task)
    [ lookup_x2; lookup_xxs ]
in
Lookup_result.ok_lwt x
(* Cond Bottom *)
| At_clause
{
  clause = Clause (_, Conditional_body (Var (x', _), _, _));
  id = tid;
  _;
} ->
let cond_block =
  Ident_map.find tid map |> Tracelet.cast_to_cond_block
in
if Option.is_some cond_block.choice
then failwith "conditional_body: not both"
else ();
let key_cond_var = Lookup_key.of_parts x' [] r_stk in
let cond_var_tree, lookup_cond_var =
  create_task key_cond_var block gate_tree
in

let sub_trees, tasks =
  List.fold [ true; false ]
    ~f:(fun (sub_trees, tasks) beta ->
      let ctracelet =
        Cond { cond_block with choice = Some beta }
      in
      let x_ret = Tracelet.ret_of ctracelet in
      let cbody_stack = Rstack.push r_stk (x, Id.cond_fid beta) in
      let key_x_ret = Lookup_key.of_parts x_ret xs cbody_stack in
      let node_x_ret, lookup_x_ret =
        create_task key_x_ret ctracelet gate_tree
      in
      (sub_trees @ [ node_x_ret ], tasks @ [ lookup_x_ret ]))
    ~init:([], [])
in
Node.update_rule gate_tree
  (Node.mk_condsite ~cond_var_tree ~sub_trees);
add_phi this_key (Riddler.cond_bottom this_key cond_block x');

let%lwt _ = filter_and_sequence lookup_cond_var in
let%lwt _ =
  Lwt_list.exists_p
    (fun task ->
      let%lwt _ = filter_and_sequence task in
      Lwt.return_true)
    tasks
in
Lookup_result.ok_lwt x
(* Fun Enter / Fun Enter Non-Local *)
| At_fun_para (is_local, fb) ->
let fid = fb.point in
let callsites =
  match Rstack.paired_callsite r_stk fid with
  | Some callsite -> [ callsite ]
  | None -> fb.callsites
in
Logs.info (fun m ->
    m "FunEnter%s: %a -> %a"
      (if is_local then "" else "Nonlocal")
      Id.pp fid Id.pp_list callsites);
let sub_trees, _taskss =
  List.fold callsites
    ~f:(fun (sub_trees, tasks) callsite ->
      let callsite_block, x', x'', x''' =
        Tracelet.fun_info_of_callsite callsite map
      in
      match Rstack.pop r_stk (x', fid) with
      | Some callsite_stack ->
          let key_f = Lookup_key.of_parts x'' [] callsite_stack in
          let node_f, lookup_f =
            create_task key_f callsite_block gate_tree
          in
          let key_arg =
            if is_local
            then Lookup_key.of_parts x''' xs callsite_stack
            else Lookup_key.of_parts x'' (x :: xs) callsite_stack
          in
          let node_arg, lookup_arg =
            create_task key_arg callsite_block gate_tree
          in
          ( sub_trees @ [ (node_f, node_arg) ],
            tasks @ [ [ lookup_f; lookup_arg ] ] )
      | None -> (sub_trees, tasks))
    ~init:([], [])
in

Node.update_rule gate_tree (Node.mk_para ~sub_trees);
add_phi this_key
  (Riddler.fun_enter this_key is_local fb callsites map);

(* let%lwt _ =
     Lwt_list.exists_p
       (fun tasks ->
         let%lwt _ = filter_and_sequence tasks in
         Lwt.return_true)
       taskss
   in *)
Lookup_result.ok_lwt x
(* Fun Exit *)
| At_clause
{
  clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
  cat = App fids;
  _;
} ->
let%lwt _ =
  Logs_lwt.app (fun m ->
      m "FunExit[%a] : %a" Id.pp xf Id.pp_list fids)
in
let key_fun = Lookup_key.of_parts xf [] r_stk in
let node_fun, _lookup_fun = create_task key_fun block gate_tree in
let sub_trees, _tasks =
  List.fold fids
    ~f:(fun (sub_trees, tasks) fid ->
      let fblock = Ident_map.find fid map in
      let key_x_ret =
        Lookup_key.get_f_return map fid r_stk x xs
      in
      let node_x_ret, lookup_x_ret =
        create_task key_x_ret fblock gate_tree
      in
      (sub_trees @ [ node_x_ret ], tasks @ [ lookup_x_ret ]))
    ~init:([], [])
in

Node.update_rule gate_tree
  (Node.mk_callsite ~fun_tree:node_fun ~sub_trees);
add_phi this_key (Riddler.fun_exit this_key xf fids map);

(* let%lwt f_seq = find_or_add_lookup key_fun block gate_tree in *)
(* let f_seq =
     Lwt_seq.unfold_lwt
       (fun acc ->
         if acc = 0
         then Lwt.return_none
         else
           (* let task _ = Lwt.return in *)
           let%lwt r = filter_and_sequence [ lookup_fun ] in
           Lwt.return_some (r, acc - 1))
       (List.length fids)
   in *)
(* let handle (xf : Lookup_result.t) =
     let fid = xf.from in
     let%lwt _ =
       Logs_lwt.app (fun m ->
           m "Handle[%a] : %a\n" Id.pp x Id.pp xf.from)
     in
     let key_x_ret = Lookup_key.get_f_return map fid r_stk x xs in
     let fblock = Ident_map.find fid map in
     let _node_x_ret, lookup_x_ret =
       create_task key_x_ret fblock gate_tree
     in
     let%lwt _ = filter_and_sequence [ lookup_x_ret ] in
     Lwt.return_unit
   in
   let%lwt _ = Lwt_seq.iter_p handle f_seq in *)
(* let%lwt _ = filter_and_sequence [ lookup_fun ] in
   let%lwt _ =
     Lwt_list.exists_p
       (fun task ->
         let%lwt _ = filter_and_sequence [ task ] in
         Lwt.return_true)
       tasks
   in *)
Lookup_result.ok_lwt x


(* let input_from_model =
          let input_collected = Global_state.collect_picked_input state model in
          input_collected
        in
        let input_from_model_ordered =
          List.sort input_from_model ~compare:(fun (k1, _v1) (k2, _v2) ->
              -Lookup_key.chrono_compare info.block_map k1 k2)
        in
       Logs.app (fun m ->
               m "M: %a\nI: %a\n"
                 Fmt.Dump.(
                   list
                     (Fmt.pair ~sep:(Fmt.any ", ") Lookup_key.pp_id (option Fmt.int)))
                 input_from_model_ordered
                 Fmt.Dump.(list (option Fmt.int))
                 inputs_from_interpreter);
           assert (
             List.length input_from_model_ordered
             = List.count inputs_from_interpreter ~f:Option.is_some);
           assert (
             List.equal [%equal: int option]
               (List.map input_from_model_ordered ~f:snd)
               (List.filter inputs_from_interpreter ~f:Option.is_some)); *)