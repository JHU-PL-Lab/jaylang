open Core
open Lwt.Infix
open Odefa_ast
open Odefa_ast.Ast
open Tracelet
open Odefa_ddpa

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

let print_dot_graph ~model ~program ~testname (state : Global_state.t) =
  let module GI = (val (module struct
                         let state = state
                         let testname = Some testname
                         let model = model
                         let source_map = Ddpa_helper.clause_mapping program
                       end) : Out_graph.Graph_state)
  in
  let module Graph_dot_printer = Out_graph.DotPrinter_Make (GI) in
  Graph_dot_printer.output_graph ()

let check (state : Global_state.t) (config : Top_config.t) =
  Logs.info (fun m -> m "Search Tree Size:\t%d" state.tree_size);
  let unfinish_lookup =
    Hash_set.to_list state.lookup_created
    |> List.map ~f:(fun key -> Solver.SuduZ3.not_ (Riddler.pick_at_key key))
  in
  let check_result = Solver.check state.phis_z3 unfinish_lookup in
  Global_state.clear_phis state;
  match check_result with
  | Result.Ok model ->
      if config.debug_model
      then (
        Logs.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ()));
        Logs.debug (fun m -> m "Model: %s" (Z3.Model.to_string model)))
      else ();
      let c_stk_mach =
        Solver.SuduZ3.(get_unbox_fun_exn model Riddler.top_stack)
      in
      let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
      print_endline @@ Concrete_stack.show c_stk;
      Some { model; c_stk }
  | Result.Error _exps -> None

(** [lookup_top] performs the lookup. Usually one lookup steps consists of

    - process clause
    - handle graph node
    - book-keep global search state
    - create sub-lookups and push into the scheduler

    [state.node_map] is once used to keep track of [Lookup_key.t] created. *)
let[@landmark] lookup_top ~(config : Top_config.t) ~(info : Global_state.info)
    ~(state : Global_state.t) job_queue : _ Lwt.t =
  let target = info.target in
  let map = info.block_map in
  let x_first = info.first in
  (* let block0 = Tracelet.cut_before true target block in *)
  let block0 = Tracelet.find_by_id target map in

  let add_phi = Global_state.add_phi state in

  (* reset and init *)
  Solver.reset ();
  Riddler.reset ();
  state.phis_z3 <- [ Riddler.pick_at_key (Lookup_key.start target) ];

  let[@landmark] rec lookup (this_key : Lookup_key.t) block
      (gate_tree : Node.t ref) : unit -> Lookup_result.t Lwt_seq.t Lwt.t =
    let block_id = Tracelet.id_of_block block in
    let x, xs, r_stk = Lookup_key.to_parts this_key in

    let[@landmark] lookup_work () =
      let%lwt _ = Lwt.pause () in
      state.tree_size <- state.tree_size + 1;

      (* Logs_lwt.app (fun m ->
             m "[Lookup]: %a in block %a\n" Lookup_key.pp this_key Id.pp block_id)
         >>= fun _ -> *)
      Hash_set.strict_remove_exn state.lookup_created this_key;

      let[@landmark] apply_rule () : Lookup_result.t Lwt.t =
        let defined_site_opt = Tracelet.defined x block in
        let p = Riddler.pick_at_key this_key in
        match defined_site_opt with
        | None ->
            Node.update_rule gate_tree Node.mismatch;
            add_phi this_key (Riddler.mismatch this_key);
            Lookup_result.fail_lwt x
        | Some defined_site -> (
            match defined_site with
            (* Value *)
            | At_clause { clause = Clause (_, Value_body v); _ } ->
                Logs.info (fun m -> m "Rule Value: %a" Ast_pp.pp_value v);
                deal_with_value (Some v) this_key block gate_tree
            (* Input *)
            | At_clause { clause = Clause (_, Input_body); _ } ->
                Hash_set.add state.input_nodes this_key;
                deal_with_value None this_key block gate_tree
            (* Alias *)
            | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
                let key_rx = Lookup_key.replace_x this_key x' in
                let node_rx, lookup_rx = create_task key_rx block gate_tree in
                Node.update_rule gate_tree (Node.alias node_rx);
                add_phi this_key (Riddler.alias this_key x');
                let%lwt _ = filter_and_sequence [ lookup_rx ] in
                Lookup_result.ok_lwt x
            (* Record Start *)
            | At_clause
                ({ clause = Clause (_, Projection_body (Var (xr, _), lbl)); _ }
                as tc) ->
                Logs.info (fun m ->
                    m "Rule ProjectBody : %a = %a.%a" Id.pp x Id.pp xr Id.pp lbl);
                let key_r = Lookup_key.replace_x this_key x in
                let node_r, lookup_r = create_task key_r block gate_tree in

                let key_r_l = Lookup_key.replace_x2 this_key (xr, lbl) in
                let node_r_l, lookup_r_l =
                  create_task key_r_l block gate_tree
                in
                Node.update_rule gate_tree (Node.project node_r node_r_l);
                add_phi this_key (Riddler.alias_key this_key key_r_l);

                let%lwt _ =
                  Lwt_list.map_p
                    (fun task -> filter_and_sequence [ task ])
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
                    (fun task -> filter_and_sequence [ task ])
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
                    (fun task -> filter_and_sequence [ task ])
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
                      let cbody_stack =
                        Rstack.push r_stk (x, Id.cond_fid beta)
                      in
                      let key_x_ret =
                        Lookup_key.of_parts x_ret xs cbody_stack
                      in
                      let node_x_ret, lookup_x_ret =
                        create_task key_x_ret ctracelet gate_tree
                      in
                      (sub_trees @ [ node_x_ret ], tasks @ [ lookup_x_ret ]))
                    ~init:([], [])
                in
                Node.update_rule gate_tree
                  (Node.mk_condsite ~cond_var_tree ~sub_trees);
                add_phi this_key (Riddler.cond_bottom this_key cond_block x');

                let%lwt _ = filter_and_sequence [ lookup_cond_var ] in
                let%lwt _ =
                  Lwt_list.exists_p
                    (fun task ->
                      let%lwt _ = filter_and_sequence [ task ] in
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
                let sub_trees, taskss =
                  List.fold callsites
                    ~f:(fun (sub_trees, tasks) callsite ->
                      let callsite_block, x', x'', x''' =
                        Tracelet.fun_info_of_callsite callsite map
                      in
                      match Rstack.pop r_stk (x', fid) with
                      | Some callsite_stack ->
                          let key_f =
                            Lookup_key.of_parts x'' [] callsite_stack
                          in
                          let node_f, lookup_f =
                            create_task key_f callsite_block gate_tree
                          in
                          let key_arg =
                            if is_local
                            then Lookup_key.of_parts x''' xs callsite_stack
                            else
                              Lookup_key.of_parts x'' (x :: xs) callsite_stack
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

                let%lwt _ =
                  Lwt_list.exists_p
                    (fun tasks ->
                      let%lwt _ = filter_and_sequence tasks in
                      Lwt.return_true)
                    taskss
                in
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
                let node_fun, lookup_fun =
                  create_task key_fun block gate_tree
                in
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

                let%lwt f_seq = find_or_add_lookup key_fun block gate_tree in
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
                let handle (xf : Lookup_result.t) =
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
                let%lwt _ = Lwt_seq.iter_p handle f_seq in
                (* let%lwt _ = filter_and_sequence [ lookup_fun ] in
                   let%lwt _ =
                     Lwt_list.exists_p
                       (fun task ->
                         let%lwt _ = filter_and_sequence [ task ] in
                         Lwt.return_true)
                       tasks
                   in *)
                Lookup_result.ok_lwt x
            | At_clause ({ clause = Clause (_, _); _ } as tc) ->
                Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
                failwith "error lookup cases"
            | Lookup_mismatch -> failwith "should not mismatch here")
      in
      let%lwt _ = apply_rule () in
      if state.tree_size mod config.steps = 0
      then
        (* Logs_lwt.app (fun m ->
               m "Step %d\t%a\n" state.tree_size Lookup_key.pp this_key)
           >>= fun _ -> *)
        match check state config with
        | Some { model; c_stk } -> Lwt.fail (Found_solution { model; c_stk })
        | None -> Lwt.return (Lwt_seq.of_list [ Lookup_result.fail x ])
      else Lwt.return (Lwt_seq.of_list [ Lookup_result.ok x ])
      (* Lookup_result.ok_lwt x *)
    in
    lookup_work
  and deal_with_value mv key block (gate_tree : Node.t ref) =
    let block_id = id_of_block block in
    let singleton_lookup = List.is_empty key.xs in

    (* Discovery Main & Non-Main *)
    if singleton_lookup
    then (
      if Ident.equal block_id id_main
      then (
        (* Discovery Main *)
        let target_stk = Rstack.concretize_top key.r_stk in
        Node.update_rule gate_tree (Node.done_ target_stk);
        add_phi key (Riddler.discover_main key mv);
        Lookup_result.ok_lwt key.x)
      else
        (* Discovery Non-Main *)
        let key_first = Lookup_key.to_first key x_first in
        let node_child, lookup_first = create_task key_first block gate_tree in
        gate_tree := { !gate_tree with rule = Node.to_first node_child };
        add_phi key (Riddler.discover_non_main key x_first mv);
        (* ignore @@ filter_and_sequence [ lookup_first ]; *)
        let%lwt _ = filter_and_sequence [ lookup_first ] in
        Lookup_result.ok_lwt key.x)
    else
      (* Discard *)
      match mv with
      | Some (Value_function _f) ->
          let key_drop_x = Lookup_key.drop_x key in
          let node_sub, lookup_sub = create_task key_drop_x block gate_tree in
          Node.update_rule gate_tree (Node.discard node_sub);
          add_phi key (Riddler.discard key mv);
          let%lwt _ = filter_and_sequence [ lookup_sub ] in
          Lookup_result.ok_lwt key.x
      (* Record Eng *)
      | Some (Value_record r) -> (
          Logs.info (fun m -> m "Rule Record: %a" Ast_pp.pp_record_value r);
          let (Record_value rmap) = r in
          let _x, xs, r_stk = Lookup_key.to_parts key in
          let labal, xs' = (List.hd_exn xs, List.tl_exn xs) in
          match Ident_map.Exceptionless.find labal rmap with
          | Some (Var (vid, _)) ->
              let key' = Lookup_key.of_parts2 (vid :: xs') r_stk in
              let node_key, lookup_key = create_task key' block gate_tree in
              Node.update_rule gate_tree (Node.alias node_key);
              add_phi key (Riddler.alias_key key key');
              let%lwt _ = filter_and_sequence [ lookup_key ] in
              Lookup_result.ok_lwt key.x
          | None ->
              Node.update_rule gate_tree Node.mismatch;
              add_phi key (Riddler.mismatch key);
              Lookup_result.fail_lwt key.x)
      | _ ->
          Node.update_rule gate_tree Node.mismatch;
          add_phi key (Riddler.mismatch key);
          Lookup_result.fail_lwt key.x
  and create_task key block node_parent :
      Node.Node_ref.t * (unit -> Lookup_result.t Lwt.t) option =
    let exist, node_child =
      Global_state.find_or_add state key block node_parent
    in
    let sub_lookup =
      if exist then None else (
        Some (lookup key block node_child)
      )
    in
    (node_child, sub_lookup |> Lwt_seq.)
  and find_or_add_lookup key block node_parent =
    let exist, _node_child =
      Global_state.find_or_add state key block node_parent
    in
    let lookup_result =
      if exist
      then Lwt.return (Global_state.get_lookup_result state key)
      else
        let%lwt seq = lookup key block node_parent () in
        Global_state.set_lookup_result state key seq
    in
    lookup_result
  and filter_and_sequence tasks (* : Lookup_result.t *) =
    let valid_tasks = List.filter_map tasks ~f:(fun t -> t) in
    let dummy = Lookup_result.ok_lwt (Ident "dummy") in
    Scheduler.push_in_line dummy job_queue valid_tasks
  in

  lookup (Lookup_key.of_parts target [] Rstack.empty) block0 state.root_node ()

let[@landmark] lookup_main ~(config : Top_config.t) program target =
  let job_queue = Scheduler.create () in
  let info : Global_state.info =
    {
      first = Ast_tools.first_id program;
      target;
      program;
      block_map = Tracelet.annotate program target;
    }
  in
  let state =
    let block0 = Tracelet.find_by_id target info.block_map in
    let state = Global_state.create_state block0 target in
    state
  in
  let handle_found model c_stk =
    Logs.info (fun m ->
        m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident target
          Concrete_stack.pp c_stk);
    Logs.debug (fun m ->
        m "Nodes picked states\n%a\n"
          (Fmt.Dump.iter_bindings
             (fun f c ->
               Hashtbl.iteri c ~f:(fun ~key ~data:_ ->
                   f key (Riddler.is_picked (Some model) key)))
             Fmt.nop Lookup_key.pp Fmt.bool)
          state.node_map);
    Hashtbl.clear state.rstk_picked;
    Hashtbl.iter_keys state.node_map ~f:(fun key ->
        if Riddler.is_picked (Some model) key
        then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
        else ());
    (* Global_state.refresh_picked state model; *)
    if config.debug_graph
    then
      print_dot_graph ~model:(Some model) ~program:info.program
        ~testname:config.filename state
    else ();
    let inputs_from_interpreter =
      Solver.get_inputs ~state ~config target model c_stk program
    in

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
    [ inputs_from_interpreter ]
  in
  let post_process () =
    match check state config with
    | Some { model; c_stk } -> handle_found model c_stk
    | None ->
        Logs.info (fun m -> m "UNSAT");
        if config.debug_model
        then
          Logs.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ()))
        else ();
        []
  in
  let do_lookup () = lookup_top ~config ~info ~state job_queue in
  let main_task () =
    match config.timeout with
    | Some ts -> Lwt_unix.with_timeout (Time.Span.to_sec ts) do_lookup
    | None -> do_lookup ()
  in
  Scheduler.push job_queue do_lookup;
  Lwt_main.run
    (try%lwt
       Lwt_unix.with_timeout 5.0 (fun () ->
           Scheduler.run job_queue >>= fun _ -> Lwt.return (post_process ()))
     with
    | Found_solution { model; c_stk } -> Lwt.return (handle_found model c_stk)
    | Lwt_unix.Timeout ->
        prerr_endline "timeout";
        Lwt.return (post_process ()))
