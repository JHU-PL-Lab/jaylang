open Batteries
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
  | None, Main _mb -> failwith "main block must have target"
  | None, Fun fb -> At_fun_para (fb.para = x, fb)
  | None, Cond cb -> At_chosen cb

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

let job_queue = Scheduler.empty ()

let push_job job = Scheduler.push job_queue job

let lookup_top ?testname program x_target : _ Lwt.t =
  (* lookup top-level _global_ state *)
  let phi_map = Core.Hashtbl.create (module Lookup_key) in
  let acc_phi_map = Core.Hashtbl.create (module Lookup_key) in
  let phi_z3_map = Core.Hashtbl.create (module Lookup_key) in
  let add_phi ?debug_info key data =
    ignore debug_info;
    Core.Hashtbl.add_multi phi_map ~key ~data
    (* match Core.Hashtbl.add phi_map ~key ~data with
       | `Ok -> ()
       | `Duplicate ->
           let old_v = Core.Hashtbl.find_exn phi_map key in
           if Constraint.equal old_v data then
             ()
           else (
             (match debug_info with
             | Some l_key ->
                 Fmt.pr "key: %a\nv_old: %a\nv_new: %a\n" Lookup_key.pp l_key
                   Constraint.pp old_v Constraint.pp data
             | None -> ());
             failwith "add_phi key duplication") *)
  in

  let merge_to_acc_phi_map () =
    Core.Hashtbl.merge_into ~src:phi_map ~dst:acc_phi_map ~f:(fun ~key a ob ->
        let _ = key in
        match ob with
        | Some _b -> failwith "acc_phi_map should not have it"
        | None -> Set_to a);
    Core.Hashtbl.clear phi_map
  in
  let search_tree = ref Gate.dummy_start in
  let lookup_task_map = ref (Core.Map.empty (module Lookup_key)) in
  Solver_helper.reset ();

  (* program analysis *)
  let map = Tracelet.annotate program x_target in
  let x_first = Ddpa_helper.first_var program in

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

  let rec lookup (xs0 : Lookup_stack.t) block rel_stack gate_tree :
      unit -> _ Lwt.t =
   fun () ->
    let x, xs = (List.hd xs0, List.tl xs0) in
    let block_id = block |> Tracelet.id_of_block |> Id.of_ast_id in
    let this_key : Lookup_key.t = (x, xs, rel_stack) in
    let key = this_key in
    Logs.info (fun m ->
        m "search begin: %a in block %a" Lookup_key.pp this_key Id.pp block_id);
    let kont =
      match defined x block with
      | At_clause tc -> (
          let (Clause (_, rhs)) = tc.clause in
          match rhs with
          | Value_body v ->
              deal_with_value (Some v) x xs block rel_stack gate_tree
          | Input_body -> deal_with_value None x xs block rel_stack gate_tree
          (* Alias *)
          | Var_body (Var (x', _)) ->
              let x' = Id.of_ast_id x' in
              add_phi this_key
              @@ C.eq_lookup (x :: xs) rel_stack (x' :: xs) rel_stack;

              let sub_tree = create_lookup_task (x', xs, rel_stack) block in
              (gate_tree := Gate.{ block_id; key; rule = alias sub_tree });

              Lwt.return_unit
          (* Binop *)
          | Binary_operation_body (Var (x1, _), bop, Var (x2, _)) ->
              let x1, x2 = (Id.of_ast_id x1, Id.of_ast_id x2) in
              add_phi this_key @@ C.bind_binop x x1 bop x2 rel_stack;

              let sub_tree1 = create_lookup_task (x1, xs, rel_stack) block in
              let sub_tree2 = create_lookup_task (x2, xs, rel_stack) block in
              (gate_tree :=
                 Gate.{ block_id; key; rule = binop sub_tree1 sub_tree2 });
              Lwt.return_unit
          (* Fun Exit *)
          | Appl_body (Var (xf, _), Var (_xv, _)) -> (
              match tc.cat with
              | App fids ->
                  let xf = Id.of_ast_id xf in
                  Logs.info (fun m ->
                      m "FunExit: %a -> %a" Id.pp xf Id.pp_old_list fids);

                  let fun_tree = create_lookup_task (xf, [], rel_stack) block in

                  let ins, sub_trees =
                    List.fold_right
                      (fun fid (ins, sub_trees) ->
                        let fblock = Ident_map.find fid map in
                        let x' = Tracelet.ret_of fblock |> Id.of_ast_id in
                        let fid = Id.of_ast_id fid in
                        let rel_stack' = Relative_stack.push rel_stack x fid in

                        let cf_in =
                          C.
                            {
                              stk_in = rel_stack';
                              xs_in = x' :: xs;
                              fun_in = fid;
                            }
                        in
                        let sub_tree =
                          create_lookup_task (x', xs, rel_stack') fblock
                        in

                        (ins @ [ cf_in ], sub_trees @ [ sub_tree ]))
                      fids ([], [])
                  in

                  let cf =
                    C.
                      {
                        xs_out = x :: xs;
                        stk_out = rel_stack;
                        site = x;
                        f_out = xf;
                        ins;
                      }
                  in
                  add_phi this_key (C.Callsite_to_fbody cf);
                  (gate_tree :=
                     Gate.
                       { block_id; key; rule = callsite fun_tree sub_trees cf });

                  Lwt.return_unit
              | _ -> failwith "fun exit clauses")
          (* Cond Bottom *)
          | Conditional_body (Var (x', _), _e1, _e2) ->
              let x' = Id.of_ast_id x' in
              let cond_block =
                Ident_map.find tc.id map |> Tracelet.cast_to_cond_block
              in
              if cond_block.choice <> None then
                failwith "conditional_body: not both"
              else
                ();

              let cond_var_tree =
                create_lookup_task (x', [], rel_stack) block
              in

              let phis, sub_trees =
                List.fold_right
                  (fun beta (phis, sub_trees) ->
                    let ctracelet =
                      Cond { cond_block with choice = Some beta }
                    in
                    let x_ret = Tracelet.ret_of ctracelet |> Id.of_ast_id in
                    let cbody_stack =
                      Relative_stack.push rel_stack x (Id.cond_fid beta)
                    in
                    let phi =
                      C.and_
                        (C.bind_v x' (Value_bool beta) rel_stack)
                        (C.eq_lookup [ x ] rel_stack [ x_ret ] cbody_stack)
                    in
                    let sub_tree =
                      create_lookup_task (x_ret, xs, cbody_stack) ctracelet
                    in
                    (phis @ [ phi ], sub_trees @ [ sub_tree ]))
                  [ true; false ] ([], [])
              in
              add_phi this_key (C.cond_bottom x rel_stack phis);
              (gate_tree :=
                 Gate.{ block_id; key; rule = condsite cond_var_tree sub_trees });

              Lwt.return_unit
          | _ ->
              Logs.err (fun m -> m "%a" Ast_pp.pp_clause tc.clause);
              failwith "error clause cases")
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
          let outs, sub_trees =
            List.fold_right
              (fun callsite (outs, sub_trees) ->
                let callsite_block, x', x'', x''' =
                  fun_info_of_callsite callsite map
                in
                match Relative_stack.pop rel_stack x' fid with
                | Some callsite_stack ->
                    let out =
                      C.
                        {
                          stk_out = callsite_stack;
                          xs_out = x''' :: xs;
                          f_out = x'';
                          site = x';
                        }
                    in
                    let sub_tree1 =
                      create_lookup_task (x'', [], callsite_stack)
                        callsite_block
                    in
                    let sub_tree2 =
                      create_lookup_task (x''', xs, callsite_stack)
                        callsite_block
                    in
                    let sub_tree = (sub_tree1, sub_tree2) in

                    (outs @ [ out ], sub_trees @ [ sub_tree ])
                | None -> (outs, sub_trees))
              fb.callsites ([], [])
          in
          let fc =
            C.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
          in
          add_phi this_key (C.Fbody_to_callsite fc);
          (gate_tree := Gate.{ block_id; key; rule = para_local sub_trees fc });

          Lwt.return_unit
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
          let outs, sub_trees =
            List.fold_right
              (fun callsite (outs, sub_trees) ->
                let callsite_block, x', x'', _x''' =
                  fun_info_of_callsite callsite map
                in
                match Relative_stack.pop rel_stack x' fid with
                | Some callsite_stack ->
                    let out =
                      C.
                        {
                          stk_out = callsite_stack;
                          xs_out = x'' :: x :: xs;
                          f_out = x'';
                          site = x';
                        }
                    in
                    (* let sub_tree =
                         create_lookup_task
                           (x'', x :: xs, callsite_stack)
                           callsite_block
                       in *)
                    let sub_tree1 =
                      create_lookup_task (x'', [], callsite_stack)
                        callsite_block
                    in
                    let sub_tree2 =
                      create_lookup_task
                        (x'', x :: xs, callsite_stack)
                        callsite_block
                    in
                    let sub_tree = (sub_tree1, sub_tree2) in
                    (outs @ [ out ], sub_trees @ [ sub_tree ])
                | None -> (outs, sub_trees))
              callsites ([], [])
          in

          let fc =
            C.{ xs_in = x :: xs; stk_in = rel_stack; fun_in = fid; outs }
          in
          add_phi this_key (C.Fbody_to_callsite fc);
          (gate_tree :=
             Gate.{ block_id; key; rule = para_nonlocal sub_trees fc });

          Lwt.return_unit
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

          let sub_tree1 =
            create_lookup_task (x2, [], condsite_stack) condsite_block
          in
          let sub_tree2 =
            create_lookup_task (x, xs, condsite_stack) condsite_block
          in
          (gate_tree :=
             Gate.{ block_id; key; rule = cond_choice sub_tree1 sub_tree2 });
          Lwt.return_unit
    in
    kont >>= fun _ ->
    let ( (top_complete : bool),
          (cvar_complete_map : (string, bool) Core.Hashtbl.t) ) =
      Gate.get_c_vars_and_complete !search_tree
    in

    if top_complete then (
      Logs.info (fun m -> m "Search Tree Size:\t%d" (Gate.size !search_tree));
      let choices_complete = Core.Hashtbl.to_alist cvar_complete_map in

      let choices_complete_z3 =
        Solver_helper.Z3API.z3_gate_out_phis choices_complete
      in

      Logs.info (fun m ->
          m "Z3_choices_complete: %a"
            Fmt.(Dump.list string)
            (List.map Z3.Expr.to_string choices_complete_z3));
      let noted_phi_map = ref (Core.Hashtbl.create (module Lookup_key)) in
      let phi_z3_map =
        Core.Hashtbl.mapi phi_map ~f:(fun ~key ~data ->
            Core.List.map data
              ~f:
                (Solver_helper.Z3API.phi_z3_of_constraint ~debug:true
                   ~debug_tool:(key, noted_phi_map)))
      in
      let phi_z3_list = Core.Hashtbl.data phi_z3_map in
      merge_to_acc_phi_map ();
      match
        Solver_helper.check (Core.List.join phi_z3_list) choices_complete_z3
      with
      | Core.Result.Ok model ->
          Logs.debug (fun m ->
              m "Solver Phis: %s" (Solver_helper.string_of_solver ()));
          Logs.debug (fun m -> m "Model: %s" (Z3.Model.to_string model));

          (* can we shrink bool optional to bool?
             the answer should be yes since we are not interested in
             any false or unpicked cvar
          *)
          let cvar_picked_map =
            Core.Hashtbl.mapi
              ~f:(fun ~key:cname ~data:_cc ->
                Constraint.mk_cvar_picked cname
                |> Solver_helper.Z3API.boole_of_str
                |> Solver_helper.Z3API.get_bool model
                |> Core.Option.value ~default:false)
              cvar_complete_map
          in

          Logs.debug (fun m ->
              m "Cvar Complete: %a"
                Fmt.Dump.(list (pair Fmt.string Fmt.bool))
                choices_complete);

          Logs.debug (fun m ->
              m "Cvar Picked: %a"
                Fmt.Dump.(list (pair Fmt.string Fmt.bool))
                (Core.Hashtbl.to_alist cvar_picked_map));


          Out_graph.output_graph graph;
          let result_info = { model; c_stk } in
          Lwt.fail (Found_solution result_info)
      | Core.Result.Error _exps ->
          Logs.info (fun m -> m "UNSAT");
          Lwt.return_unit)
    else (
      (* Logs.debug (fun m -> m "Search Tree (UNDONE): @;%a]" (Gate.pp_compact ()) !search_tree); *)
      Logs.info (fun m -> m "UNDONE");
      Lwt.return_unit)
  and create_lookup_task (task_key : Lookup_key.t) block =
    (* let sub_tree = ref Gate.pending_node in
       push_job @@ lookup xs block rel_stack sub_tree;
       sub_tree *)
            let graph_info : Out_graph.graph_info_type =
              {
                phi_map = sts.phi_map;
                noted_phi_map;
                source_map = Ddpa_helper.clause_mapping program;
                vertex_info_map = Core.Hashtbl.create (module Lookup_key);
                edge_info_map = Core.Hashtbl.create (module Core.String);
                model = Some (ref model);
                testname;
              }
            in
            let module GI = (val (module struct
                                   let graph_info = graph_info
                                 end) : Out_graph.Graph_info)
            in
            let module Graph_dot_printer = Out_graph.DotPrinter_Make (GI) in
            let graph, picked_c_stk_set =
              Graph_dot_printer.graph_of_gate_tree sts
            in
            let c_stk =
              if Core.Hash_set.length picked_c_stk_set = 1 then
                Core.List.hd_exn (Core.Hash_set.to_list picked_c_stk_set)
              else
                failwith "incorrect c_stk set"
            in
    let block_id = block |> Tracelet.id_of_block |> Id.of_ast_id in
    let x, xs, rel_stack = task_key in
    match Core.Map.find !lookup_task_map task_key with
    | Some existing_tree ->
        ref Gate.{ block_id; key = task_key; rule = to_visited existing_tree }
    | None ->
        let sub_tree =
          ref Gate.{ block_id; key = task_key; rule = pending_node }
        in
        lookup_task_map :=
          Core.Map.add_exn !lookup_task_map ~key:task_key ~data:sub_tree;
        push_job @@ lookup (x :: xs) block rel_stack sub_tree;
        sub_tree
  and deal_with_value mv x xs block rel_stack gate_tree =
    let this_key : Lookup_key.t = (x, xs, rel_stack) in
    let block_id = id_of_block block in

    (* Discovery Main & Non-Main *)
    (if List.is_empty xs then (
       (match mv with
       | Some (Value_function _) -> add_phi this_key @@ C.bind_fun x rel_stack x
       | Some v -> add_phi this_key @@ C.bind_v x v rel_stack
       | None -> add_phi this_key @@ C.bind_input x rel_stack);
       if block_id = id_main then (
         (* Discovery Main *)
         let target_stk = Relative_stack.concretize rel_stack in
         add_phi ~debug_info:(x, xs, rel_stack) this_key
         @@ C.Target_stack target_stk;
         gate_tree :=
           Gate.
             {
               block_id = block_id |> Id.of_ast_id;
               key = this_key;
               rule = done_ target_stk;
             })
       else (* Discovery Non-Main *)
         let child_tree =
           create_lookup_task (Id.of_ast_id x_first, [], rel_stack) block
         in
         gate_tree :=
           Gate.
             {
               block_id = block_id |> Id.of_ast_id;
               key = this_key;
               rule = to_first child_tree;
             })
    else (* Discard *)
      match mv with
      | Some (Value_function _f) ->
          add_phi this_key @@ C.eq_lookup (x :: xs) rel_stack xs rel_stack;
          add_phi this_key @@ C.bind_fun x rel_stack x;
          let sub_tree =
            create_lookup_task (List.hd xs, List.tl xs, rel_stack) block
          in
          gate_tree :=
            Gate.
              {
                block_id = block_id |> Id.of_ast_id;
                key = this_key;
                rule = discard sub_tree;
              }
      | _ ->
          gate_tree :=
            Gate.
              {
                block_id = block_id |> Id.of_ast_id;
                key = this_key;
                rule = mismatch;
              });
    Lwt.return_unit
  in

  let block0 = Tracelet.find_by_id x_target map in
  (* let block0 = Tracelet.cut_before true x_target block in *)
  let x_target' = Id.of_ast_id x_target in

  lookup [ x_target' ] block0 Relative_stack.empty search_tree ()

let lookup_main ?testname ?timeout program x_target =
  let main_task =
    match timeout with
    | Some ts ->
        Lwt_unix.with_timeout (Core.Time.Span.to_sec ts) (fun () ->
            lookup_top ?testname program x_target)
    | None -> lookup_top ?testname program x_target
  in
  push_job (fun () -> main_task);
  Lwt_main.run
    (try%lwt Scheduler.run job_queue >>= fun _ -> Lwt.return [ [] ] with
    | Found_solution ri ->
        Fmt.(
          pr "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident x_target
            Concrete_stack.pp ri.c_stk);
        Lwt.return
          [ Solver_helper.get_input x_target ri.model ri.c_stk program ]
    | Lwt_unix.Timeout ->
        prerr_endline "timeout";
        Lwt.return [ [ -42 ] ])
