open Core
open Lwt.Infix
open Odefa_ast
open Odefa_ast.Ast
open Tracelet
open Odefa_ddpa
module C = Constraint

type def_site =
  | At_clause of tl_clause
  | At_fun_para of bool * fun_block
  | At_chosen of cond_block

let defined x' block =
  let x = Id.to_ast_id x' in
  match (Tracelet.clause_of_x block x, block) with
  | Some tc, _ -> At_clause tc
  | None, Fun fb -> At_fun_para (Ident.(equal fb.para x), fb)
  | None, Cond cb -> At_chosen cb
  | None, Main _mb -> failwith "main block must have target"

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

let print_dot_graph ~noted_phi_map ~model ~program ~testname
    (state : Search_tree.t) =
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

let[@landmark] lookup_top ~(config : Top_config.t) job_queue program x_target :
    _ Lwt.t =
  (* program analysis *)
  let map = Tracelet.annotate program x_target in
  let x_first = Ddpa_helper.first_var program in
  let block0 = Tracelet.find_by_id x_target map in
  (* let block0 = Tracelet.cut_before true x_target block in *)
  let x_target' = Id.of_ast_id x_target in
  let state = Search_tree.create block0 x_target' in
  let add_phi ?debug_info key data =
    Search_tree.add_phi ~debug_info state key data
  in
  Solver_helper.reset ();

  let collect_cvar cvar =
    Hashtbl.add_exn state.cvar_complete ~key:cvar ~data:false
  in

  let fun_info_of_callsite callsite map =
    let callsite_block = Tracelet.find_by_id callsite map in
    let tc = Tracelet.clause_of_x_exn callsite_block callsite in
    let x', x'', x''' =
      match tc.clause with
      | Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _))) ->
          (Id.of_ast_id x', Id.of_ast_id x'', Id.of_ast_id x''')
      | _ -> failwith "incorrect clause for callsite"
    in
    (callsite_block, x', x'', x''')
  in

  let[@landmark] rec lookup (xs0 : Lookup_stack.t) block rel_stack
      (gate_tree : Gate.Node.t ref) : unit -> _ Lwt.t =
   fun () ->
    let x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
    let block_id = block |> Tracelet.id_of_block |> Id.of_ast_id in
    let this_key : Lookup_key.t = (x, xs, rel_stack) in
    (* Logs.info *)
    Logs.app (fun m ->
        m "search begin: %a in block %a" Lookup_key.pp this_key Id.pp block_id);
    let () =
      match defined x block with
      | At_clause { clause = Clause (_, Value_body v); _ } ->
          deal_with_value (Some v) x xs block rel_stack gate_tree
      (* Input *)
      | At_clause { clause = Clause (_, Input_body); _ } ->
          deal_with_value None x xs block rel_stack gate_tree
      (* Alias *)
      | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
          let x' = Id.of_ast_id x' in
          add_phi this_key
          @@ C.eq_lookup (x :: xs) rel_stack (x' :: xs) rel_stack;
          let sub_tree, edge =
            create_lookup_task (x', xs, rel_stack) block gate_tree
          in
          gate_tree := { !gate_tree with rule = Gate.alias sub_tree };
          bubble_up_edges [ edge ]
      (* Binop *)
      | At_clause
          {
            clause =
              Clause (_, Binary_operation_body (Var (x1, _), bop, Var (x2, _)));
            _;
          } ->
          let x1, x2 = (Id.of_ast_id x1, Id.of_ast_id x2) in
          add_phi this_key @@ C.bind_binop x x1 bop x2 rel_stack;

          let sub_tree1, edge1 =
            create_lookup_task (x1, xs, rel_stack) block gate_tree
          in
          let sub_tree2, edge2 =
            create_lookup_task (x2, xs, rel_stack) block gate_tree
          in
          gate_tree := { !gate_tree with rule = Gate.binop sub_tree1 sub_tree2 };
          bubble_up_edges [ edge1; edge2 ]
      (* Cond Top *)
      | At_chosen cb ->
          let condsite_block = Tracelet.outer_block block map in
          let choice = BatOption.get cb.choice in

          let condsite_stack, paired =
            match
              Relative_stack.pop_check_paired rel_stack
                (cb.point |> Id.of_ast_id) (Id.cond_fid choice)
            with
            | Some (stk, paired) -> (stk, paired)
            | None -> failwith "impossible in CondTop"
          in
          let x2 = cb.cond |> Id.of_ast_id in

          if paired then
            ()
          else
            add_phi this_key (C.bind_v x2 (Value_bool choice) condsite_stack);
          add_phi this_key
          @@ C.eq_lookup (x :: xs) rel_stack (x :: xs) condsite_stack;

          let sub_tree1, edge1 =
            create_lookup_task (x2, [], condsite_stack) condsite_block gate_tree
          in
          let sub_tree2, edge2 =
            create_lookup_task (x, xs, condsite_stack) condsite_block gate_tree
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
          let x' = Id.of_ast_id x' in
          let cond_block =
            Ident_map.find tid map |> Tracelet.cast_to_cond_block
          in
          if Option.is_some cond_block.choice then
            failwith "conditional_body: not both"
          else
            ();

          let cond_var_tree, var_edge =
            create_lookup_task (x', [], rel_stack) block gate_tree
          in

          let phis, sub_trees, edges =
            List.fold [ true; false ]
              ~f:(fun (phis, sub_trees, edges) beta ->
                let cvar = Cvar.mk_condsite_beta (x :: xs) x rel_stack beta in
                collect_cvar cvar;
                let ctracelet = Cond { cond_block with choice = Some beta } in
                let x_ret = Tracelet.ret_of ctracelet |> Id.of_ast_id in
                let cbody_stack =
                  Relative_stack.push rel_stack x (Id.cond_fid beta)
                in
                let phi =
                  C.and_
                    (C.bind_v x' (Value_bool beta) rel_stack)
                    (C.eq_lookup [ x ] rel_stack [ x_ret ] cbody_stack)
                in
                let sub_tree, edge =
                  create_lookup_task ~cvar (x_ret, xs, cbody_stack) ctracelet
                    gate_tree
                in
                ( phis @ [ phi ],
                  sub_trees @ [ (cvar, sub_tree) ],
                  edges @ [ edge ] ))
              ~init:([], [], [ var_edge ])
          in
          add_phi this_key (C.cond_bottom xs0 x rel_stack phis);
          gate_tree :=
            {
              !gate_tree with
              rule = Gate.mk_condsite ~cond_var_tree ~sub_trees;
            };
          bubble_up_edges edges
      (* Fun Enter Parameter *)
      | At_fun_para (true, fb) ->
          let fid = Id.of_ast_id fb.point in
          let callsites =
            match Relative_stack.paired_callsite rel_stack fid with
            | Some callsite -> [ callsite |> Id.to_ast_id ]
            | None -> fb.callsites
          in
          Logs.info (fun m ->
              m "FunEnter: %a -> %a" Id.pp fid Id.pp_old_list callsites);
          let outs, sub_trees, edges =
            List.fold fb.callsites
              ~f:(fun (outs, sub_trees, edges) callsite ->
                let callsite_block, x', x'', x''' =
                  fun_info_of_callsite callsite map
                in
                match Relative_stack.pop rel_stack x' fid with
                | Some callsite_stack ->
                    let out : Helper.fc_out =
                      {
                        stk_out = callsite_stack;
                        xs_out = x''' :: xs;
                        f_out = x'';
                        site = x';
                      }
                    in
                    let cvar =
                      Cvar.fun_to_callsite (x :: xs) rel_stack fid out
                    in
                    collect_cvar cvar;
                    let sub_tree1, edge1 =
                      create_lookup_task ~cvar (x'', [], callsite_stack)
                        callsite_block gate_tree
                    in
                    let sub_tree2, edge2 =
                      create_lookup_task ~cvar (x''', xs, callsite_stack)
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
            Helper.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
          in
          add_phi this_key (C.Fbody_to_callsite (x :: xs, fc));
          gate_tree := { !gate_tree with rule = Gate.mk_para ~sub_trees ~fc };
          bubble_up_edges edges
      (* Fun Enter Non-Local *)
      | At_fun_para (false, fb) ->
          let fid = Id.of_ast_id fb.point in
          let callsites =
            match Relative_stack.paired_callsite rel_stack fid with
            | Some callsite -> [ callsite |> Id.to_ast_id ]
            | None -> fb.callsites
          in
          Logs.info (fun m ->
              m "FunEnterNonlocal: %a -> %a" Id.pp fid Id.pp_old_list callsites);
          let outs, sub_trees, edges =
            List.fold callsites
              ~f:(fun (outs, sub_trees, edges) callsite ->
                let callsite_block, x', x'', _x''' =
                  fun_info_of_callsite callsite map
                in
                match Relative_stack.pop rel_stack x' fid with
                | Some callsite_stack ->
                    let out : Helper.fc_out =
                      {
                        stk_out = callsite_stack;
                        xs_out = x'' :: x :: xs;
                        f_out = x'';
                        site = x';
                      }
                    in
                    let cvar =
                      Cvar.fun_to_callsite (x :: xs) rel_stack fid out
                    in
                    collect_cvar cvar;
                    let sub_tree1, edge1 =
                      create_lookup_task ~cvar (x'', [], callsite_stack)
                        callsite_block gate_tree
                    in
                    let sub_tree2, edge2 =
                      create_lookup_task ~cvar
                        (x'', x :: xs, callsite_stack)
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
            Helper.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
          in
          add_phi this_key (C.Fbody_to_callsite (x :: xs, fc));
          gate_tree := { !gate_tree with rule = Gate.mk_para ~sub_trees ~fc };
          bubble_up_edges edges
      (* Fun Exit *)
      | At_clause
          {
            clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
            cat = App fids;
            _;
          } ->
          let xf = Id.of_ast_id xf in
          Logs.info (fun m ->
              m "FunExit: %a -> %a" Id.pp xf Id.pp_old_list fids);

          let fun_tree, fun_edge =
            create_lookup_task (xf, [], rel_stack) block gate_tree
          in

          let ins, sub_trees, edges =
            List.fold fids
              ~f:(fun (ins, sub_trees, edges) fid ->
                let fblock = Ident_map.find fid map in
                let x' = Tracelet.ret_of fblock |> Id.of_ast_id in
                let fid = Id.of_ast_id fid in
                let rel_stack' = Relative_stack.push rel_stack x fid in
                let cf_in =
                  Helper.{ stk_in = rel_stack'; xs_in = x' :: xs; fun_in = fid }
                in
                let cvar =
                  Cvar.callsite_to_fun (x :: xs) rel_stack x xf cf_in
                in
                collect_cvar cvar;
                let sub_tree, sub_edge =
                  create_lookup_task ~cvar (x', xs, rel_stack') fblock gate_tree
                in

                ( ins @ [ cf_in ],
                  sub_trees @ [ (cvar, sub_tree) ],
                  edges @ [ sub_edge ] ))
              ~init:([], [], [ fun_edge ])
          in

          let cf : Helper.cf =
            { xs_out = x :: xs; stk_out = rel_stack; site = x; f_out = xf; ins }
          in
          add_phi this_key (C.Callsite_to_fbody (x :: xs, cf));
          gate_tree :=
            { !gate_tree with rule = Gate.mk_callsite ~fun_tree ~sub_trees ~cf };
          bubble_up_edges edges
      | At_clause ({ clause = Clause (_, _); _ } as tc) ->
          Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
          failwith "error lookup cases"
    in

    let top_complete =
      Gate.get_c_vars_and_complete state.cvar_complete_map !(state.root_node)
    in

    (* verify two implemenetation of cvar_map are equal *)
    (* if
         not (Hashtbl.equal Bool.equal state.cvar_complete_map state.cvar_complete)
       then (
         Logs.app (fun m ->
             m "---\n[OLD]%a\n[NEW]%a\n" Cvar.pp_set state.cvar_complete_map
               Cvar.pp_set state.cvar_complete);
         let noted_phi_map = Hashtbl.create (module Lookup_key) in
         let phi_z3_map =
           Hashtbl.mapi state.phi_map ~f:(fun ~key ~data ->
               List.map data
                 ~f:
                   (Solver_helper.Z3API.phi_z3_of_constraint ~debug:true
                      ~debug_tool:(key, noted_phi_map)))
         in
           print_dot_graph ~noted_phi_map ~model:None ~program
             ~testname:config.filename state;
           failwith "cvar_map should equal")
       else
         (); *)
    if top_complete then (
      Logs.app (fun m ->
          m "Search Tree Size:\t%d" (Gate.size !(state.root_node)));
      let choices_complete = Hashtbl.to_alist state.cvar_complete_map in

      let choices_complete_z3 =
        Solver_helper.Z3API.z3_gate_out_phis choices_complete
      in

      Logs.info (fun m ->
          m "Z3_choices_complete: %a"
            Fmt.(Dump.list string)
            (List.map ~f:Z3.Expr.to_string choices_complete_z3));
      let noted_phi_map = Hashtbl.create (module Lookup_key) in
      let phi_z3_map =
        Hashtbl.mapi state.phi_map ~f:(fun ~key ~data ->
            List.map data
              ~f:
                (Solver_helper.Z3API.phi_z3_of_constraint ~debug:true
                   ~debug_tool:(key, noted_phi_map)))
      in
      let phi_z3_list = Hashtbl.data phi_z3_map in
      Search_tree.merge_to_acc_phi_map state ();
      match Solver_helper.check (List.join phi_z3_list) choices_complete_z3 with
      | Result.Ok model ->
          Logs.debug (fun m ->
              m "Solver Phis: %s" (Solver_helper.string_of_solver ()));
          Logs.debug (fun m -> m "Model: %s" (Z3.Model.to_string model));

          (* can we shrink bool optional to bool?
             the answer should be yes since we are not interested in
             any false or unpicked cvar
          *)
          state.cvar_picked_map <-
            Hashtbl.mapi
              ~f:(fun ~key:cname ~data:_cc ->
                Cvar.set_picked cname |> Cvar.print
                |> Solver_helper.Z3API.boole_of_str
                |> Solver_helper.Z3API.get_bool model
                |> Option.value ~default:false)
              state.cvar_complete_map;

          Logs.debug (fun m ->
              m "Cvar Complete: %a"
                Fmt.Dump.(list (pair Cvar.pp_print Fmt.bool))
                choices_complete);

          Logs.debug (fun m ->
              m "Cvar Picked: %a"
                Fmt.Dump.(list (pair Cvar.pp_print Fmt.bool))
                (Hashtbl.to_alist state.cvar_picked_map));

          if config.output_dot then
            print_dot_graph ~noted_phi_map
              ~model:(Some (ref model))
              ~program ~testname:config.filename state
          else
            ();

          let c_stk = Search_tree.get_singleton_c_stk_exn state in

          let result_info = { model; c_stk } in
          Lwt.fail (Found_solution result_info)
      | Result.Error _exps ->
          Logs.info (fun m -> m "UNSAT");
          Lwt.return_unit)
    else
      Lwt.return_unit
  and bubble_up_edges (edges : Gate.Node.edge list) =
    List.iter edges ~f:(fun edge ->
        if !(edge.succ).has_complete_path then
          Gate.bubble_up_complete state.cvar_complete edge edge.pred)
  and create_lookup_task ?cvar (key : Lookup_key.t) block parent_node =
    let block_id = block |> Tracelet.id_of_block |> Id.of_ast_id in
    let x, xs, rel_stack = key in
    match Hashtbl.find state.node_map key with
    | Some child_node ->
        let edge = Gate.mk_edge ?cvar parent_node child_node in
        Gate.add_pred child_node edge;
        (child_node, edge)
        (* if !existing_tree.has_complete_path then (
             Logs.app (fun m -> m ".1");
             let edge' =
               match parent with
               | Direct node -> Direct node
               | With_cvar (cvar, parent) -> With_cvar (cvar, existing_tree)
             in
             (existing_tree, edge')
             (* (, Gate.bubble_up_complete state.cvar_complete edge' node);
                Logs.app (fun m ->
                    m "{%a: %B}\n[...]%a\n" Lookup_key.pp !existing_tree.key
                      !existing_tree.has_complete_path Cvar.pp_set state.cvar_complete)) *))
           else
             (existing_tree, []) *)
        (* ref
           (Gate.mk_node ~block_id ~key ~parent
              ~rule:(Gate.to_visited existing_tree)) *)
    | None ->
        let child_node =
          ref (Gate.mk_node ~block_id ~key ~rule:Gate.pending_node)
        in
        let edge = Gate.mk_edge ?cvar parent_node child_node in
        Gate.add_pred child_node edge;
        Hashtbl.add_exn state.node_map ~key ~data:child_node;
        Scheduler.push job_queue @@ lookup (x :: xs) block rel_stack child_node;
        (child_node, edge)
  and deal_with_value mv x xs block rel_stack (gate_tree : Gate.Node.t ref) =
    let key : Lookup_key.t = (x, xs, rel_stack) in
    let block_id_here = id_of_block block in
    let block_id = block_id_here |> Id.of_ast_id in

    (* Discovery Main & Non-Main *)
    if List.is_empty xs then (
      (match mv with
      | Some (Value_function _) -> add_phi key @@ C.bind_fun x rel_stack x
      | Some v -> add_phi key @@ C.bind_v x v rel_stack
      | None -> add_phi key @@ C.bind_input x rel_stack);
      if Ident.equal block_id_here id_main then (
        (* Discovery Main *)
        let target_stk = Relative_stack.concretize rel_stack in
        add_phi ~debug_info:(x, xs, rel_stack) key @@ C.Target_stack target_stk;
        gate_tree := { !gate_tree with rule = Gate.done_ target_stk };
        let edge = Gate.mk_edge gate_tree gate_tree in
        Logs.app (fun m -> m ".2");
        Gate.bubble_up_complete state.cvar_complete edge gate_tree;
        Logs.app (fun m ->
            m "{%a: %B}\n[...]%a\n" Lookup_key.pp !gate_tree.key
              !gate_tree.has_complete_path Cvar.pp_set state.cvar_complete))
      else (* Discovery Non-Main *)
        let child_tree, edge =
          create_lookup_task
            (Id.of_ast_id x_first, [], rel_stack)
            block gate_tree
        in
        gate_tree := { !gate_tree with rule = Gate.to_first child_tree };
        bubble_up_edges [ edge ])
    else (* Discard *)
      match mv with
      | Some (Value_function _f) ->
          add_phi key @@ C.eq_lookup (x :: xs) rel_stack xs rel_stack;
          add_phi key @@ C.bind_fun x rel_stack x;
          let sub_tree, edge =
            create_lookup_task
              (List.hd_exn xs, List.tl_exn xs, rel_stack)
              block gate_tree
          in
          gate_tree := { !gate_tree with rule = Gate.discard sub_tree };
          bubble_up_edges [ edge ]
      | _ -> failwith "why mismatch"
  in

  lookup [ x_target' ] block0 Relative_stack.empty state.root_node ()

let[@landmark] lookup_main ~(config : Top_config.t) program x_target =
  let job_queue = Scheduler.create () in
  let main_task =
    match config.timeout with
    | Some ts ->
        Lwt_unix.with_timeout (Time.Span.to_sec ts) (fun () ->
            lookup_top ~config job_queue program x_target)
    | None -> lookup_top ~config job_queue program x_target
  in
  Scheduler.push job_queue (fun () -> main_task);
  Lwt_main.run
    (try%lwt Scheduler.run job_queue >>= fun _ -> Lwt.return [ [] ] with
    | Found_solution ri ->
        Logs.info (fun m ->
            m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident x_target
              Concrete_stack.pp ri.c_stk);
        Lwt.return
          [ Solver_helper.get_inputs x_target ri.model ri.c_stk program ]
    | Lwt_unix.Timeout ->
        prerr_endline "timeout";
        Lwt.return [ [] ])
