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

  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let%lwt _ =
      Logs_lwt.app (fun m ->
          m "[Lookup][=>]: %a in block %a\n" Lookup_key.pp this_key Id.pp
            (Tracelet.id_of_block block))
    in
    let x, _xs, _r_stk = Lookup_key.to_parts this_key in
    (* A lookup must be required before. *)
    let gate_tree = Global_state.find_node_exn state this_key block in
    (* update global state *)
    state.tree_size <- state.tree_size + 1;
    Hash_set.strict_remove_exn state.lookup_created this_key;
    let result_pusher = Global_state.get_lookup_pusher state this_key in

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
              let%lwt _ =
                Logs_lwt.info (fun m -> m "Rule Value: %a" Ast_pp.pp_value v)
              in
              deal_with_value (Some v) this_key block gate_tree
          (* Input *)
          (* Alias *)
          | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
              let%lwt _ =
                Logs_lwt.info (fun m -> m "Rule Alias: %a" Ast_pp.pp_ident x')
              in
              let key_rx = Lookup_key.replace_x this_key x' in
              let node_rx, lookup_rx =
                find_or_add_lookup key_rx block gate_tree
              in
              Node.update_rule gate_tree (Node.alias node_rx);
              add_phi this_key (Riddler.alias this_key x');
              let%lwt _ =
                Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_rx
              in
              Lookup_result.ok_lwt x
          | At_clause ({ clause = Clause (_, _); _ } as tc) ->
              Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
              failwith "error lookup cases"
          | Lookup_mismatch -> failwith "should not mismatch here"
          | _ -> failwith "TODO")
    in
    let%lwt rule_result = apply_rule () in
    let%lwt _ =
      if state.tree_size mod config.steps = 0
      then (
        (* Logs_lwt.app (fun m ->
               m "Step %d\t%a\n" state.tree_size Lookup_key.pp this_key)
           >>= fun _ -> *)
        (* match check state config with *)
        match None with
        | Some { model; c_stk } -> Lwt.fail (Found_solution { model; c_stk })
        | None ->
            result_pusher (Some rule_result);
            result_pusher None;
            Lwt.return_unit)
      else (
        result_pusher (Some rule_result);
        result_pusher None;
        Lwt.return_unit)
    in
    let%lwt _ =
      Logs_lwt.app (fun m ->
          m "[Lookup][<=]: %a in block %a\n" Lookup_key.pp this_key Id.pp
            (Tracelet.id_of_block block))
    in
    Lwt.return_unit
  (* Lookup_result.ok_lwt x *)
  and deal_with_value mv key block (gate_tree : Node.t ref) =
    let singleton_lookup = List.is_empty key.xs in

    (* Discovery Main & Non-Main *)
    if singleton_lookup
    then (
      if Ident.equal (id_of_block block) id_main
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
        let%lwt _ = filter_and_sequence lookup_first in
        Lookup_result.ok_lwt key.x)
    else
      (* Discard *)
      match mv with
      | Some (Value_function _f) ->
          let key_drop_x = Lookup_key.drop_x key in
          let node_sub, lookup_sub = create_task key_drop_x block gate_tree in
          Node.update_rule gate_tree (Node.discard node_sub);
          add_phi key (Riddler.discard key mv);
          let%lwt _ = filter_and_sequence lookup_sub in
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
              let%lwt _ = filter_and_sequence lookup_key in
              Lookup_result.ok_lwt key.x
          | None ->
              Node.update_rule gate_tree Node.mismatch;
              add_phi key (Riddler.mismatch key);
              Lookup_result.fail_lwt key.x)
      | _ ->
          Node.update_rule gate_tree Node.mismatch;
          add_phi key (Riddler.mismatch key);
          Lookup_result.fail_lwt key.x
  (* These things exist together:
     1. a node for a lookup (key)
     2. a result stream for a lookup
     3. a producer for a lookup in the scheduler

     For a parent lookup, it should only be interested in the
     (1) to update the nodes relation in the global DAG
     and (2) to do its own processing.
     (3) is an initializaion. However, we cannot put it in the beginning
     of a lookup function. It's the force to create a lookup, an init-pre-init.

     In a complex lookup e.g. `r` at `r = f a`. The node and constraints are eagerly set.
     The lookup of `f` and `a` are lazy. However, it's possible `a` are eager
  *)
  and find_or_add_lookup key block node_parent =
    let exist, node_child, result_stream =
      Global_state.find_or_add state key block node_parent
    in
    if exist then () else Scheduler.add_and_detach job_queue (lookup key block);
    (node_child, result_stream)
  and create_task key block node_parent :
      Node.Node_ref.t * Lookup_result.t Lwt_stream.t =
    let _exist, node_child, result_stream =
      Global_state.find_or_add state key block node_parent
    in
    (node_child, result_stream)
  and filter_and_sequence _task (* : Lookup_result.t *) =
    let _ = job_queue in
    (* let valid_tasks = List.filter_map tasks ~f:(fun t -> t) in
       let dummy = Lookup_result.ok (Ident "dummy") in
       let dummy_stream = Lwt.return (Lwt_stream.return dummy) in
       Scheduler.wait_all dummy_stream job_queue valid_tasks *)
    Lwt.return_unit
  in

  let key_target = Lookup_key.of_parts target [] Rstack.empty in
  let _ = Global_state.init_node state key_target state.root_node in
  lookup key_target block0 ()

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
    (* input_from_model *)
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
