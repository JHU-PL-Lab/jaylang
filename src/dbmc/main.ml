open Core
open Lwt.Infix
open Odefa_ast
open Odefa_ast.Ast
open Tracelet
open Odefa_ddpa
module C = Constraint

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

let print_dot_graph ~noted_phi_map ~model ~program ~testname
    (state : Search_tree.state) =
  let graph_info : Out_graph.graph_info_type =
    {
      phi_map = state.phi_map;
      noted_phi_map;
      source_map = Ddpa_helper.clause_mapping program;
      vertex_info_map = Hashtbl.create (module Lookup_key);
      model;
      testname = Some testname;
    }
  in
  let module GI = (val (module struct
                         let graph_info = graph_info
                       end) : Out_graph.Graph_info)
  in
  let module Graph_dot_printer = Out_graph.DotPrinter_Make (GI) in
  let graph = Graph_dot_printer.graph_of_gate_tree state in
  Graph_dot_printer.output_graph graph

let check (state : Search_tree.state) (config : Top_config.t) =
  Logs.info (fun m -> m "Search Tree Size:\t%d" state.tree_size);
  let unfinish_lookup =
    Hash_set.to_list state.lookup_created
    |> List.map ~f:(fun key ->
           Solver.SuduZ3.not_ (Riddler.pick_at_key state key))
  in
  let check_result = Solver.check state.phis_z3 unfinish_lookup in
  Search_tree.clear_phis state;
  match check_result with
  | Result.Ok model ->
      if config.debug_model then (
        Logs.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ()));
        Logs.debug (fun m -> m "Model: %s" (Z3.Model.to_string model)))
      else
        ();
      let c_stk_mach =
        Solver.SuduZ3.(get_unbox_fun_exn model Riddler.top_stack)
      in
      let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
      print_endline @@ Concrete_stack.show c_stk;
      Some { model; c_stk }
  | Result.Error _exps ->
      Logs.info (fun m -> m "UNSAT");
      None

let[@landmark] lookup_top ~config ~(info : Search_tree.info)
    ~(state : Search_tree.state) job_queue : _ Lwt.t =
  let target = info.target in
  let map = info.block_map in
  let x_first = info.first in
  let block0 = Tracelet.find_by_id target map in

  (* let block0 = Tracelet.cut_before true target block in *)

  (* reset and init *)
  Solver.reset ();
  state.phis_z3 <- [ Riddler.pick_at_key state (Lookup_key.start target) ];
  (* let add_phi key data =
       Search_tree.add_phi ~debug:config.debug_lookup_graph state key data
     in
  *)
  let encode_constraint = Riddler.mk_encode_constraint map state in
  let add_phi' ?(x_first = None) ?(callsites = []) this_key defined_site =
    let data : Z3.Expr.expr =
      encode_constraint ~x_first ~callsites this_key defined_site
    in
    state.phis_z3 <- data :: state.phis_z3
  in

  let create_cvar cvar = Search_tree.create_cvar state cvar in
  let bubble_up_complete edge parent_node =
    let changed_cvars =
      Gate.bubble_up_complete state.cvar_complete edge parent_node
    in
    List.iter changed_cvars ~f:(fun cvar ->
        match Hash_set.strict_remove state.cvar_complete_false cvar with
        | Ok () ->
            let cvar_z3 = Solver.cvar_complete_to_z3 cvar true in
            state.cvar_complete_true_z3 <-
              cvar_z3 :: state.cvar_complete_true_z3
        | Error _ -> ())
  in

  let[@landmark] rec lookup (xs0 : Lookup_stack.t) block rel_stack
      (gate_tree : Gate.Node.t ref) : unit -> _ Lwt.t =
    let[@landmark] lookup_work () =
      state.tree_size <- state.tree_size + 1;
      let x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
      let r_stk = rel_stack in
      let block_id = Tracelet.id_of_block block in
      let this_key : Lookup_key.t = Lookup_key.of_parts x xs rel_stack in
      Hash_set.strict_remove_exn state.lookup_created this_key;
      Logs.info (fun m ->
          m "search begin: %a in block %a" Lookup_key.pp this_key Id.pp block_id);
      let[@landmark] apply_rule () =
        let defined_site = Tracelet.defined x block in
        match defined_site with
        | At_clause { clause = Clause (_, Value_body v); _ } ->
            deal_with_value (Some v) this_key block gate_tree
        (* Input *)
        | At_clause { clause = Clause (_, Input_body); _ } ->
            deal_with_value None this_key block gate_tree
        (* Alias *)
        | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
            add_phi' this_key defined_site;
            let sub_tree, edge =
              create_lookup_task
                (Lookup_key.replace_x this_key x')
                block gate_tree
            in
            gate_tree := { !gate_tree with rule = Gate.alias sub_tree };
            bubble_up_edges [ edge ]
        (* Binop *)
        | At_clause
            {
              clause =
                Clause
                  (_, Binary_operation_body (Var (x1, _), _bop, Var (x2, _)));
              _;
            } ->
            add_phi' this_key defined_site;

            let sub_tree1, edge1 =
              create_lookup_task
                (Lookup_key.of_parts x1 xs rel_stack)
                block gate_tree
            in
            let sub_tree2, edge2 =
              create_lookup_task
                (Lookup_key.of_parts x2 xs rel_stack)
                block gate_tree
            in
            gate_tree :=
              { !gate_tree with rule = Gate.binop sub_tree1 sub_tree2 };
            bubble_up_edges [ edge1; edge2 ]
        (* Cond Top *)
        | At_chosen cb ->
            let condsite_block = Tracelet.outer_block block map in
            let choice = Option.value_exn cb.choice in

            let condsite_stack =
              match Rstack.pop rel_stack (cb.point, Id.cond_fid choice) with
              | Some stk -> stk
              | None -> failwith "impossible in CondTop"
            in
            let x2 = cb.cond in

            add_phi' this_key defined_site;
            let sub_tree1, edge1 =
              create_lookup_task
                (Lookup_key.of_parts x2 [] condsite_stack)
                condsite_block gate_tree
            in
            let sub_tree2, edge2 =
              create_lookup_task
                (Lookup_key.of_parts x xs condsite_stack)
                condsite_block gate_tree
            in
            gate_tree :=
              { !gate_tree with rule = Gate.cond_choice sub_tree1 sub_tree2 };
            bubble_up_edges [ edge1; edge2 ]
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
            if Option.is_some cond_block.choice then
              failwith "conditional_body: not both"
            else
              ();

            let cond_var_tree, var_edge =
              create_lookup_task
                (Lookup_key.of_parts x' [] rel_stack)
                block gate_tree
            in

            let sub_trees, edges, _cvars =
              List.fold [ true; false ]
                ~f:(fun (sub_trees, edges, cvars) beta ->
                  let cvar =
                    create_cvar
                      (Cvar.mk_condsite_beta (x :: xs) x rel_stack beta)
                  in
                  let ctracelet = Cond { cond_block with choice = Some beta } in
                  let x_ret = Tracelet.ret_of ctracelet in
                  let cbody_stack =
                    Rstack.push rel_stack (x, Id.cond_fid beta)
                  in
                  let sub_tree, edge =
                    create_lookup_task ~cvar
                      (Lookup_key.of_parts x_ret xs cbody_stack)
                      ctracelet gate_tree
                  in
                  ( sub_trees @ [ (cvar, sub_tree) ],
                    edges @ [ edge ],
                    cvar :: cvars ))
                ~init:([], [ var_edge ], [])
            in
            add_phi' this_key defined_site;

            gate_tree :=
              {
                !gate_tree with
                rule = Gate.mk_condsite ~cond_var_tree ~sub_trees;
              };
            bubble_up_edges edges
        (* Fun Enter Parameter *)
        | At_fun_para (true, fb) ->
            let fid = fb.point in
            let callsites =
              match Rstack.paired_callsite rel_stack fid with
              | Some callsite -> [ callsite ]
              | None -> fb.callsites
            in
            Logs.info (fun m ->
                m "FunEnter: %a -> %a" Id.pp fid Id.pp_list callsites);
            let outs, sub_trees, edges =
              List.fold fb.callsites
                ~f:(fun (outs, sub_trees, edges) callsite ->
                  let callsite_block, x', x'', x''' =
                    Tracelet.fun_info_of_callsite callsite map
                  in
                  match Rstack.pop rel_stack (x', fid) with
                  | Some callsite_stack ->
                      let cvar =
                        create_cvar
                          (Cvar.mk_fun_to_callsite (x :: xs) rel_stack fid x''
                             x')
                      in
                      let out : Cvar.fc_out =
                        {
                          stk_out = callsite_stack;
                          xs_out = x''' :: xs;
                          f_out = x'';
                          site = x';
                          cvar;
                        }
                      in
                      let sub_tree1, edge1 =
                        create_lookup_task ~cvar
                          (Lookup_key.of_parts x'' [] callsite_stack)
                          callsite_block gate_tree
                      in
                      let sub_tree2, edge2 =
                        create_lookup_task ~cvar
                          (Lookup_key.of_parts x''' xs callsite_stack)
                          callsite_block gate_tree
                      in
                      let sub_tree = (cvar, sub_tree1, sub_tree2) in

                      ( outs @ [ out ],
                        sub_trees @ [ sub_tree ],
                        edges @ [ edge1; edge2 ] )
                  | None -> (outs, sub_trees, edges))
                ~init:([], [], [])
            in
            let fc =
              Cvar.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
            in

            add_phi' ~callsites this_key defined_site;

            gate_tree := { !gate_tree with rule = Gate.mk_para ~sub_trees ~fc };
            bubble_up_edges edges
        (* Fun Enter Non-Local *)
        | At_fun_para (false, fb) ->
            let fid = fb.point in
            let callsites =
              match Rstack.paired_callsite rel_stack fid with
              | Some callsite -> [ callsite ]
              | None -> fb.callsites
            in
            Logs.info (fun m ->
                m "FunEnterNonlocal: %a -> %a" Id.pp fid Id.pp_list callsites);
            let outs, sub_trees, edges =
              List.fold callsites
                ~f:(fun (outs, sub_trees, edges) callsite ->
                  let callsite_block, x', x'', _x''' =
                    Tracelet.fun_info_of_callsite callsite map
                  in
                  match Rstack.pop rel_stack (x', fid) with
                  | Some callsite_stack ->
                      let cvar =
                        create_cvar
                          (Cvar.mk_fun_to_callsite (x :: xs) rel_stack fid x''
                             x')
                      in
                      let out : Cvar.fc_out =
                        {
                          stk_out = callsite_stack;
                          xs_out = x'' :: x :: xs;
                          f_out = x'';
                          site = x';
                          cvar;
                        }
                      in
                      let sub_tree1, edge1 =
                        create_lookup_task ~cvar
                          (Lookup_key.of_parts x'' [] callsite_stack)
                          callsite_block gate_tree
                      in
                      let sub_tree2, edge2 =
                        create_lookup_task ~cvar
                          (Lookup_key.of_parts x'' (x :: xs) callsite_stack)
                          callsite_block gate_tree
                      in
                      let sub_tree = (cvar, sub_tree1, sub_tree2) in
                      ( outs @ [ out ],
                        sub_trees @ [ sub_tree ],
                        edges @ [ edge1; edge2 ] )
                  | None -> (outs, sub_trees, edges))
                ~init:([], [], [])
            in

            let fc =
              Cvar.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
            in

            add_phi' ~callsites this_key defined_site;

            gate_tree := { !gate_tree with rule = Gate.mk_para ~sub_trees ~fc };
            bubble_up_edges edges
        (* Fun Exit *)
        | At_clause
            {
              clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
              cat = App fids;
              _;
            } ->
            Logs.info (fun m -> m "FunExit: %a -> %a" Id.pp xf Id.pp_list fids);

            let fun_tree, fun_edge =
              create_lookup_task
                (Lookup_key.of_parts xf [] rel_stack)
                block gate_tree
            in

            let ins, sub_trees, edges =
              List.fold fids
                ~f:(fun (ins, sub_trees, edges) fid ->
                  let fblock = Ident_map.find fid map in
                  let x' = Tracelet.ret_of fblock in
                  let rel_stack' = Rstack.push rel_stack (x, fid) in
                  let cvar =
                    create_cvar
                      (Cvar.mk_callsite_to_fun (x :: xs) rel_stack x xf fid)
                  in
                  let cf_in =
                    Cvar.
                      {
                        stk_in = rel_stack';
                        xs_in = x' :: xs;
                        fun_in = fid;
                        cvar;
                      }
                  in
                  let sub_tree, sub_edge =
                    create_lookup_task ~cvar
                      (Lookup_key.of_parts x' xs rel_stack')
                      fblock gate_tree
                  in

                  ( ins @ [ cf_in ],
                    sub_trees @ [ (cvar, sub_tree) ],
                    edges @ [ sub_edge ] ))
                ~init:([], [], [ fun_edge ])
            in

            let cf : Cvar.cf =
              {
                xs_out = x :: xs;
                stk_out = rel_stack;
                site = x;
                f_out = xf;
                ins;
              }
            in
            add_phi' this_key defined_site;

            gate_tree :=
              {
                !gate_tree with
                rule = Gate.mk_callsite ~fun_tree ~sub_trees ~cf;
              };
            bubble_up_edges edges
        | At_clause ({ clause = Clause (_, _); _ } as tc) ->
            Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
            failwith "error lookup cases"
        | Lookup_mismatch -> failwith "should not mismatch here"
      in
      apply_rule ();

      (* if !(state.pvar_reach_top) then ( *)
      (* !(state.root_node).has_complete_path &&  *)
      if state.tree_size mod 500 = 0 then
        match check state config with
        | Some { model; c_stk } ->
            if config.debug_lookup_graph then
              print_dot_graph ~noted_phi_map:state.noted_phi_map
                ~model:(Some model) ~program:info.program
                ~testname:config.filename state
            else
              ();
            Lwt.fail (Found_solution { model; c_stk })
        | None -> Lwt.return_unit
      else
        Lwt.return_unit
    in
    lookup_work
  and bubble_up_edges (_edges : Gate.Node.edge list) = ()
  (* List.iter edges ~f:(fun edge ->
      if !(edge.succ).has_complete_path then
        bubble_up_complete edge edge.pred
      else
        ()) *)
  and create_lookup_task ?cvar (key : Lookup_key.t) block parent_node =
    let block_id = block |> Tracelet.id_of_block in
    match Hashtbl.find state.node_map key with
    | Some child_node ->
        let edge = Gate.mk_edge ?cvar parent_node child_node in
        Gate.add_pred child_node edge;
        (child_node, edge)
    | None ->
        Hash_set.strict_add_exn state.lookup_created key;

        let child_node =
          ref (Gate.mk_node ~block_id ~key ~rule:Gate.pending_node)
        in
        let edge = Gate.mk_edge ?cvar parent_node child_node in
        Gate.add_pred child_node edge;
        Hashtbl.add_exn state.node_map ~key ~data:child_node;
        Scheduler.push job_queue
        @@ lookup (Lookup_key.lookups key) block key.r_stk child_node;
        (child_node, edge)
  and deal_with_value mv key block (gate_tree : Gate.Node.t ref) =
    let block_id_here = id_of_block block in
    let block_id = block_id_here in

    (* Discovery Main & Non-Main *)
    if List.is_empty key.xs then
      if Ident.equal block_id_here id_main then (
        (* Discovery Main *)
        let target_stk = Rstack.concretize key.r_stk in
        add_phi' key (Tracelet.defined key.x block);
        gate_tree := { !gate_tree with rule = Gate.done_ target_stk };
        let edge = Gate.mk_edge gate_tree gate_tree in
        bubble_up_complete edge gate_tree)
      else (
        (* Discovery Non-Main *)
        add_phi' ~x_first:(Some x_first) key (Tracelet.defined key.x block);
        let child_tree, edge =
          create_lookup_task (Lookup_key.to_first key x_first) block gate_tree
        in
        gate_tree := { !gate_tree with rule = Gate.to_first child_tree };
        bubble_up_edges [ edge ])
    else (* Discard *)
      match mv with
      | Some (Value_function _f) ->
          add_phi' key (Tracelet.defined key.x block);
          let sub_tree, edge =
            create_lookup_task (Lookup_key.drop_x key) block gate_tree
          in
          gate_tree := { !gate_tree with rule = Gate.discard sub_tree };
          bubble_up_edges [ edge ]
      | _ ->
          add_phi' key Tracelet.Lookup_mismatch;
          gate_tree := { !gate_tree with rule = Gate.mismatch }
  in
  lookup [ target ] block0 Rstack.empty state.root_node ()

let[@landmark] lookup_main ~(config : Top_config.t) program target =
  let job_queue = Scheduler.create () in
  let info : Search_tree.info =
    {
      first = Ddpa_helper.first_var program;
      target;
      program;
      block_map = Tracelet.annotate program target;
    }
  in
  let state =
    let block0 = Tracelet.find_by_id target info.block_map in
    let state = Search_tree.create_state block0 target in
    state
  in
  let do_lookup () = lookup_top ~config ~info ~state job_queue in
  let main_task =
    match config.timeout with
    | Some ts -> Lwt_unix.with_timeout (Time.Span.to_sec ts) do_lookup
    | None -> do_lookup ()
  in
  let handle_found model c_stk =
    Logs.info (fun m ->
        m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident target
          Concrete_stack.pp c_stk);
    [ Solver.get_inputs target model c_stk program ]
  in
  let post_process () =
    match check state config with
    | Some { model; c_stk } -> handle_found model c_stk
    | None -> []
  in

  Scheduler.push job_queue (fun () -> main_task);
  Lwt_main.run
    (try%lwt
       Scheduler.run job_queue >>= fun _ -> Lwt.return (post_process ())
     with
    | Found_solution { model; c_stk } -> Lwt.return (handle_found model c_stk)
    | Lwt_unix.Timeout ->
        prerr_endline "timeout";
        Lwt.return (post_process ()))
