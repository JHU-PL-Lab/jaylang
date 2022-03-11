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

  let module RS = (val (module struct
                         let state = state
                         let add_phi = add_phi
                         let x_first = x_first
                       end) : Ruler.Ruler_state)
  in
  let module R = Ruler.Make (RS) in
  let[@landmark] rec lookup (this_key : Lookup_key.t) block () : unit Lwt.t =
    let%lwt _ =
      Logs_lwt.app (fun m ->
          m "[Lookup][=>]: %a in block %a\n" Lookup_key.pp this_key Id.pp
            (Tracelet.id_of_block block))
    in
    let x, xs, r_stk = Lookup_key.to_parts this_key in
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
                Logs_lwt.app (fun m -> m "Rule Value: %a" Ast_pp.pp_value v)
              in
              R.deal_with_value (Some v) this_key block gate_tree result_pusher
                find_or_add_task
          (* Input *)
          | At_clause { clause = Clause (_, Input_body); _ } ->
              let%lwt _ = Logs_lwt.info (fun m -> m "Rule Value (Input)") in
              Hash_set.add state.input_nodes this_key;
              R.deal_with_value None this_key block gate_tree result_pusher
                find_or_add_task
          (* Alias *)
          | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
              let%lwt _ =
                Logs_lwt.info (fun m -> m "Rule Alias: %a" Ast_pp.pp_ident x')
              in
              let key_rx = Lookup_key.replace_x this_key x' in
              let node_rx, lookup_rx =
                find_or_add_task key_rx block gate_tree
              in
              Node.update_rule gate_tree (Node.alias node_rx);
              add_phi this_key (Riddler.alias this_key x');
              let%lwt _ =
                Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_rx
              in
              Lookup_result.ok_lwt x
          (* Record Start *)
          | At_clause
              ({ clause = Clause (_, Projection_body (Var (xr, _), lbl)); _ } as
              tc) ->
              Logs.info (fun m ->
                  m "Rule ProjectBody : %a = %a.%a" Id.pp x Id.pp xr Id.pp lbl);
              let key_r = Lookup_key.replace_x this_key x in
              let node_r = find_node key_r block gate_tree in

              let key_r_l = Lookup_key.replace_x2 this_key (xr, lbl) in
              let node_r_l, lookup_r_l =
                find_or_add_task key_r_l block gate_tree
              in
              Node.update_rule gate_tree (Node.project node_r node_r_l);
              add_phi this_key (Riddler.alias_key this_key key_r_l);

              let%lwt _ =
                Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_r_l
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
              let node_x1, lookup_x1 =
                find_or_add_task key_x1 block gate_tree
              in
              let key_x2 = Lookup_key.of_parts x2 xs r_stk in
              let node_x2, lookup_x2 =
                find_or_add_task key_x2 block gate_tree
              in

              Node.update_rule gate_tree (Node.binop node_x1 node_x2);
              add_phi this_key (Riddler.binop this_key bop x1 x2);

              (* TODO: unclear what should a binop pushes. *)
              let rec loop () =
                let r1 = Lwt_stream.get lookup_x1 in
                let r2 = Lwt_stream.get lookup_x2 in
                let%lwt v1, v2 = Lwt.both r1 r2 in
                match (v1, v2) with
                | None, None -> Lwt.return_unit
                | _, _ ->
                    result_pusher (Some (Lookup_result.ok x));
                    loop ()
              in
              let%lwt _ = loop () in
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
                find_or_add_task key_x2 condsite_block gate_tree
              in
              let key_xxs = Lookup_key.of_parts x xs condsite_stack in
              let node_xxs, lookup_xxs =
                find_or_add_task key_xxs condsite_block gate_tree
              in

              Node.update_rule gate_tree (Node.cond_choice node_x2 node_xxs);
              add_phi this_key (Riddler.cond_top this_key cb condsite_stack);

              (* TODO *)
              let rec loop () =
                let r1 = Lwt_stream.get lookup_x2 in
                let r2 = Lwt_stream.get lookup_xxs in
                let%lwt v1, v2 = Lwt.both r1 r2 in
                match (v1, v2) with
                | None, None -> Lwt.return_unit
                | _, _ ->
                    result_pusher (Some (Lookup_result.ok x));
                    loop ()
              in
              let%lwt _ = loop () in

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
                find_or_add_task key_cond_var block gate_tree
              in

              let sub_trees =
                List.fold [ true; false ]
                  ~f:(fun sub_trees beta ->
                    let ctracelet =
                      Cond { cond_block with choice = Some beta }
                    in
                    let x_ret = Tracelet.ret_of ctracelet in
                    let cbody_stack = Rstack.push r_stk (x, Id.cond_fid beta) in
                    let key_x_ret = Lookup_key.of_parts x_ret xs cbody_stack in
                    let node_x_ret = find_node key_x_ret ctracelet gate_tree in
                    sub_trees @ [ node_x_ret ])
                  ~init:[]
              in
              Node.update_rule gate_tree
                (Node.mk_condsite ~cond_var_tree ~sub_trees);
              add_phi this_key (Riddler.cond_bottom this_key cond_block x');

              let%lwt _ =
                Lwt_stream.find
                  (fun (c : Lookup_result.t) ->
                    if c.status
                    then (
                      List.iter [ true; false ] ~f:(fun beta ->
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
                            find_or_add_task key_cond_var block gate_tree
                          in
                          ());
                      true)
                    else false)
                  lookup_cond_var
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
              let%lwt _ =
                Logs_lwt.app (fun m ->
                    m "FunEnter%s: %a -> %a"
                      (if is_local then "" else "Nonlocal")
                      Id.pp fid Id.pp_list callsites)
              in
              let sub_trees =
                List.fold callsites
                  ~f:(fun sub_trees callsite ->
                    let callsite_block, x', x'', x''' =
                      Tracelet.fun_info_of_callsite callsite map
                    in
                    match Rstack.pop r_stk (x', fid) with
                    | Some callsite_stack ->
                        let key_f = Lookup_key.of_parts x'' [] callsite_stack in
                        let node_f, _lookup_f =
                          find_or_add_task key_f callsite_block gate_tree
                        in
                        let key_arg =
                          if is_local
                          then Lookup_key.of_parts x''' xs callsite_stack
                          else Lookup_key.of_parts x'' (x :: xs) callsite_stack
                        in
                        let node_arg, _lookup_f_arg =
                          find_or_add_task key_arg callsite_block gate_tree
                        in
                        sub_trees @ [ (node_f, node_arg) ]
                    | None -> sub_trees)
                  ~init:[]
              in

              Node.update_rule gate_tree (Node.mk_para ~sub_trees);
              add_phi this_key
                (Riddler.fun_enter this_key is_local fb callsites map);
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
                find_or_add_task key_fun block gate_tree
              in
              let sub_trees =
                List.fold fids
                  ~f:(fun sub_trees fid ->
                    let fblock = Ident_map.find fid map in
                    let key_x_ret =
                      Lookup_key.get_f_return map fid r_stk x xs
                    in
                    (* let node_x_ret, _lookup_x_ret =
                         find_or_add_task key_x_ret fblock gate_tree
                       in *)
                    let node_x_ret = find_node key_x_ret fblock gate_tree in
                    sub_trees @ [ node_x_ret ])
                  ~init:[]
              in

              Node.update_rule gate_tree
                (Node.mk_callsite ~fun_tree:node_fun ~sub_trees);
              add_phi this_key (Riddler.fun_exit this_key xf fids map);

              let%lwt _ =
                Lwt_stream.iter_p
                  (fun (rf : Lookup_result.t) ->
                    ignore @@ assert false;
                    let fid = rf.from in
                    assert (List.mem fids fid ~equal:Id.equal);
                    let fblock = Ident_map.find fid map in
                    let key_x_ret =
                      Lookup_key.get_f_return map fid r_stk x xs
                    in
                    let _, lookup_x_ret =
                      find_or_add_task key_x_ret fblock gate_tree
                    in
                    let%lwt _ = Lwt_stream.next lookup_x_ret in
                    Lwt.return_unit)
                  lookup_fun
              in
              ignore @@ assert false;

              Lookup_result.ok_lwt x
          | At_clause ({ clause = Clause (_, _); _ } as tc) ->
              Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
              failwith "error lookup cases"
          | Lookup_mismatch -> failwith "should not mismatch here")
    in
    let%lwt _rule_result = apply_rule () in
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
            (* result_pusher (Some rule_result); *)
            result_pusher None;
            Lwt.return_unit)
      else (
        (* result_pusher (Some rule_result); *)
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
  and find_or_add_task key block node_parent =
    let exist, node_child, result_stream =
      Global_state.find_or_add state key block node_parent
    in
    if exist then () else Scheduler.add_and_detach job_queue (lookup key block);
    (node_child, result_stream)
  and find_node key block node_parent =
    let _exist, node_child, _result_stream =
      Global_state.find_or_add state key block node_parent
    in
    node_child
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
  let handle_graph model =
    if config.debug_graph
    then
      print_dot_graph ~model ~program:info.program ~testname:config.filename
        state
    else ()
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
    handle_graph (Some model);

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
        handle_graph None;
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
