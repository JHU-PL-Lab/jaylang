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
  (* print_endline ("defined " ^ (show_ident x)); *)
  (* print_endline @@ Tracelet.show block; *)
  match Tracelet.clause_of_x block x, block with
  | Some tc, _ -> 
    Lwt.return @@ At_clause tc
  | None, Main _mb -> failwith "main block must have target"
  | None, Fun fb -> 
    Lwt_fmt.(fprintf stdout "Rule FunEnter%s: in %a, to %a\n"
               (if fb.para = x then "Local" else "Nonlocal")
               Ast_pp.pp_ident fb.point
               Id.pp_old_list fb.callsites
            ) >|= fun _ ->
    At_fun_para (fb.para = x, fb)
  | None, Cond cb -> 
    Lwt_fmt.(fprintf stdout "Rule CondTop: in %a\n"
               Ast_pp.pp_ident cb.point) >|= fun _ ->
    At_chosen cb

(* let debug_count = ref 30 *)

exception Model_result of Z3.Model.model

let job_queue = Scheduler.empty ()

let lookup_top program x_target : _ Lwt.t =
  (* lookup top-level _global_ state *)
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
  let gate_set = ref [] in
  let add_gate gate = 
    gate_set := gate :: !gate_set in
  let gate_counter = ref 0 in
  let gate_tree_root = ref Gate.Pending in
  Solver_helper.reset ();

  (* program analysis *)
  let map = Tracelet.annotate program x_target in
  let x_first = Ddpa_helper.first_var program in

  let fun_info_of_callsite callsite map = 
    let callsite_block = Tracelet.find_by_id callsite map in
    let tc = Tracelet.clause_of_x_exn callsite_block callsite in
    let x', x'', x''' = match tc.clause with
      | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
        Id.of_ast_id x' ,Id.of_ast_id x'' ,Id.of_ast_id x'''
      | _ -> failwith "incorrect clause for callsite"
    in
    callsite_block, x', x'', x'''
  in

  let rec lookup (xs0 : Lookup_stack.t) block rel_stack gate_tree : unit -> _ Lwt.t =
    fun () ->
      (* debug_count := !debug_count - 1;
         if !debug_count <= 0 then failwith "debug_count" else (); *)
      Lwt_fmt.(fprintf stdout "Lookup: %a, Relstack: %a, Block: %a \n" 
                 Lookup_stack.pp xs0
                 Relative_stack.pp rel_stack
                 Ast_pp.pp_ident (Tracelet.id_of_block block)) >>= fun _ ->
      let x, xs = List.hd xs0, List.tl xs0 in

      defined x block >>= fun xdef ->

      let kont = match xdef with
        | At_clause tc -> (
            begin
              let (Clause (_, rhs)) = tc.clause in
              let block_point = id_of_block block in
              match rhs with 

              (* Value Discovery Main *)
              | Value_body v when block_point = id_main && List.is_empty xs -> 
                (match Relative_stack.concretize rel_stack with
                 | Some target_stk ->
                   (match v with
                    | Value_function _ -> ()
                    | _ -> add_phi @@ C.bind_v x v rel_stack
                   );
                   add_phi @@ C.Target_stack target_stk;
                   gate_tree := Gate.Done x;
                 | None ->
                   gate_tree := Gate.Mismatch x;
                );
                Lwt.return_unit  
              | Input_body when block_point = id_main  ->
                (
                  match Relative_stack.concretize rel_stack with
                  | Some target_stk ->
                    add_phi @@ C.bind_input x rel_stack;
                    add_phi @@ C.Target_stack target_stk;
                    gate_tree := Gate.Done x;
                  | None ->
                    gate_tree := Gate.Mismatch x;
                );
                Lwt.return_unit

              (* Value Discovery Non-Main *)
              | Value_body v when List.is_empty xs ->
                (match v with
                 | Value_function _vf -> add_phi @@ C.bind_fun x rel_stack x
                 | _ -> add_phi @@ C.bind_v x v rel_stack
                );

                let sub_tree = ref Gate.Pending in
                gate_tree := Gate.Pass (x, sub_tree);

                Scheduler.push job_queue @@ lookup [Id.of_ast_id x_first] block rel_stack sub_tree;
                Lwt.return_unit

              | Input_body ->
                add_phi @@ C.bind_input x rel_stack;

                let sub_tree = ref Gate.Pending in
                gate_tree := Gate.Pass (x, sub_tree);

                Scheduler.push job_queue @@ lookup [Id.of_ast_id x_first] block rel_stack sub_tree;
                Lwt.return_unit

              (* Value Discard *)
              | Value_body(Value_function _f) -> 
                add_phi @@ C.eq_lookup (x::xs) rel_stack xs rel_stack;

                let sub_tree = ref Gate.Pending in
                gate_tree := Gate.Pass (x, sub_tree);

                Scheduler.push job_queue @@ lookup xs block rel_stack sub_tree;
                Lwt.return_unit

              (* Alias *)
              | Var_body (Var (x', _)) ->
                let x' = Id.of_ast_id x' in
                add_phi @@ C.eq_lookup (x::xs) rel_stack (x'::xs) rel_stack;

                let sub_tree = ref Gate.Pending in
                gate_tree := Gate.Pass (x, sub_tree);

                Scheduler.push job_queue @@ lookup (x'::xs) block rel_stack sub_tree;
                Lwt.return_unit

              (* Binop *)
              | Binary_operation_body (Var (x1, _), bop, Var (x2, _)) ->
                let x1, x2 = Id.of_ast_id x1, Id.of_ast_id x2 in
                add_phi @@ C.bind_binop x x1 bop x2 rel_stack;

                let sub_tree1 = ref Gate.Pending in
                let sub_tree2 = ref Gate.Pending in
                gate_tree := Gate.And (x, [sub_tree1; sub_tree2]);

                Scheduler.push job_queue @@ lookup (x1::xs) block rel_stack sub_tree1;
                Scheduler.push job_queue @@ lookup (x2::xs) block rel_stack sub_tree2;
                Lwt.return_unit

              (* Fun Exit *)
              | Appl_body (Var (xf, _), Var (_xv, _)) -> (
                  match tc.cat with
                  | App fids -> (
                      let xf = Id.of_ast_id xf in
                      Lwt_fmt.(fprintf stdout "Rule FunExit: to %a\n" 
                                 Id.pp_old_list fids) >>= fun _ ->

                      let phis, sub_trees, job_infos = List.fold_left (fun (phis, sub_trees, job_infos) fid  -> 
                          let fblock = Ident_map.find fid map in
                          let x' = Tracelet.ret_of fblock |> Id.of_ast_id in
                          let fid = Id.of_ast_id fid in
                          let rel_stack' = Relative_stack.push rel_stack x fid in

                          let sub_tree = ref Gate.Pending in
                          let gate = ref false in
                          add_gate gate;

                          let phi = C.and_ 
                              (C.bind_fun xf rel_stack fid)
                              (C.eq_lookup (x::xs) rel_stack (x'::xs) rel_stack') in
                          let job_info = (x', fblock, rel_stack', gate, sub_tree) in
                          (phis @ [phi], sub_trees @ [sub_tree], job_infos @ [job_info])
                        ) ([], [], []) fids in

                      add_phi (C.only_one !gate_counter phis);
                      gate_counter := !gate_counter + List.length fids;

                      let fun_tree = ref Gate.Pending in
                      gate_tree := Gate.GuardedChoice (x, [fun_tree], sub_trees);

                      Scheduler.push job_queue @@ lookup [xf] block rel_stack fun_tree;
                      List.iter (fun (x', fblock, rel_stack', gate, sub_tree) ->
                          let search_job = lookup (x'::xs) fblock rel_stack' sub_tree in
                          let with_gate_open_job () = 
                            search_job () >>= fun _ ->
                            gate := true;
                            Lwt.return_unit
                          in
                          Scheduler.push job_queue @@ with_gate_open_job) 
                        job_infos;
                      Lwt.return_unit
                    )
                  | _ -> failwith "fun exit clauses"
                )

              (* Cond Bottom *)
              | Conditional_body (Var (x', _), _e1, _e2) -> (
                  let x' = Id.of_ast_id x' in
                  let cond_block = 
                    Ident_map.find tc.id map
                    |> Tracelet.cast_to_cond_block in
                  (
                    if cond_block.choice <> None then 
                      failwith "conditional_body: not both"
                    else ()
                  );

                  let phis, sub_trees, job_infos = List.fold_left (fun (phis, sub_trees, job_infos) beta ->
                      let ctracelet = 
                        Cond { cond_block with choice = Some beta }
                      in
                      let x_ret = Tracelet.ret_of ctracelet |> Id.of_ast_id in
                      let sub_tree = ref Gate.Pending in
                      let gate = ref false in
                      add_gate gate;

                      let phi = C.and_ 
                          (C.bind_v x' (Value_bool beta) rel_stack)
                          (C.eq_lookup [x] rel_stack [x_ret] rel_stack) in
                      let job_info = (x_ret, ctracelet, gate, sub_tree) in

                      (phis @ [phi], sub_trees @ [sub_tree], job_infos @ [job_info])
                    ) ([],[],[]) [true; false] in

                  add_phi (C.only_one !gate_counter phis);
                  gate_counter := !gate_counter + 2;

                  let cond_var_tree = ref Gate.Pending in
                  gate_tree := Gate.GuardedChoice (x, [cond_var_tree], sub_trees);

                  Scheduler.push job_queue @@ lookup [x'] block rel_stack cond_var_tree;

                  List.iter (fun (x_ret, ctracelet, gate, sub_tree) -> 
                      let search_job = lookup [x_ret] ctracelet rel_stack sub_tree in
                      let with_gate_open_job () =
                        search_job () >>= fun _ ->
                        gate := true;
                        Lwt.return_unit
                      in
                      Scheduler.push job_queue @@ with_gate_open_job) 
                    job_infos;
                  Lwt.return_unit
                )
              | _ -> failwith "error clause cases"
            end
          )

        (* Fun Enter Parameter *)
        | At_fun_para (true, fb) ->
          let fid = Id.of_ast_id fb.point in
          let callsites = 
            match Relative_stack.paired_callsite rel_stack fid with
            | Some callsite -> [callsite |> Id.to_ast_id]
            | None -> fb.callsites
          in
          let phis, sub_trees = List.fold_left (fun (phis, sub_trees) callsite -> 
              let callsite_block, x', x'', x''' = fun_info_of_callsite callsite map in
              match Relative_stack.pop rel_stack x' fid with
              | Some callsite_stack -> 
                let phi = C.and_
                    (C.eq_lookup (x::xs) rel_stack (x'''::xs) callsite_stack)
                    (C.bind_fun x'' callsite_stack fid) in

                let sub_tree1 = ref Gate.Pending in
                let sub_tree2 = ref Gate.Pending in
                let sub_tree = ref (Gate.And (fid, [sub_tree1; sub_tree2])) in

                let gate = ref false in
                add_gate gate;

                let job () = Lwt.both
                    (lookup [x''] callsite_block callsite_stack sub_tree1 ()) 
                    (lookup (x'''::xs) callsite_block callsite_stack sub_tree2 ())  >>= fun _ ->
                  gate := true;
                  Lwt.return_unit
                in
                Scheduler.push job_queue @@ job;

                (phis @ [phi], sub_trees @ [sub_tree])
              | None -> (phis, sub_trees)
            ) ([],[]) fb.callsites in

          add_phi (C.only_one !gate_counter phis);
          gate_counter := !gate_counter + List.length fb.callsites;

          gate_tree := Gate.Choice (x, sub_trees);
          Lwt.return_unit

        (* Fun Enter Non-Local *)
        | At_fun_para (false, fb) ->
          let fid = Id.of_ast_id fb.point in
          let callsites = 
            match Relative_stack.paired_callsite rel_stack fid with
            | Some callsite -> [callsite |> Id.to_ast_id]
            | None -> fb.callsites
          in
          let phis, sub_trees = List.fold_left (fun (phis, sub_trees) callsite -> 
              let callsite_block, x', x'', _x''' = fun_info_of_callsite callsite map in
              match Relative_stack.pop rel_stack x' fid with
              | Some callsite_stack -> 
                let phi = C.and_ 
                    (C.eq_lookup (x::xs) rel_stack (x''::x::xs) callsite_stack)
                    (C.bind_fun x'' callsite_stack fid) in 
                let sub_tree1 = ref Gate.Pending in
                let sub_tree2 = ref Gate.Pending in
                let sub_tree = ref (Gate.And (fid, [sub_tree1; sub_tree2])) in

                let gate = ref false in
                add_gate gate;

                let job () =
                  (lookup (x''::x::xs) callsite_block callsite_stack sub_tree2 ()) >>= fun _ ->
                  gate := true;
                  Lwt.return_unit
                in
                Scheduler.push job_queue @@ job;

                (phis @ [phi], sub_trees @ [sub_tree])
              | None -> (phis, sub_trees)
            ) ([],[]) callsites in

          add_phi (C.only_one !gate_counter phis);
          gate_counter := !gate_counter + List.length fb.callsites;

          gate_tree := Gate.Choice (x, sub_trees);
          Lwt.return_unit

        (* Cond Top *)
        | At_chosen cb -> 
          let condsite_block = Ident_map.find (outer_id_of_block block) map in
          let x2 = cb.cond |> Id.of_ast_id in
          let choice = BatOption.get cb.choice in
          add_phi (C.bind_v x2 (Value_bool choice) rel_stack);

          let sub_tree1 = ref Gate.Pending in
          let sub_tree2 = ref Gate.Pending in
          gate_tree := Gate.And (x, [sub_tree1; sub_tree2]);

          Scheduler.push job_queue @@ lookup [x2] condsite_block rel_stack sub_tree1;
          Scheduler.push job_queue @@ lookup (x::xs) condsite_block rel_stack sub_tree2;
          Lwt.return_unit
      in 
      kont >>= fun _ ->
      if not (List.is_empty !phi_set) && Gate.check_valid_tree !gate_tree_root then
        (* 
      List.iter !phi_set ~f:(fun phi ->
          let z3_phi = Z3API.z3_phis_of_smt_phi phi in
          Fmt.(pr "%a\n%s\n\n" Constraint.pp phi (Z3.Expr.to_string z3_phi));
          Z3.Solver.add solver [z3_phi]
        );
      List.iter z3_gate_phis ~f:(fun phi ->
      Fmt.(pr "%s\n" (Z3.Expr.to_string phi))
    );
    *)
        (*  -> *)
        Lwt_io.printl @@ string_of_int (Gate.size !gate_tree_root) >>= fun _ ->
        let model = Solver_helper.check phi_set !gate_set in
        match model with
        | Some model -> (
            Lwt_io.printl @@ Gate.show_node !gate_tree_root >>= fun _ ->
            Lwt.fail @@ Model_result model
          ) 
        | None ->  Lwt_io.printl @@ "UNSAT"
      else
        Lwt.return_unit

  in
  let block0 = Tracelet.find_by_id x_target map in
  (* let block0 = Tracelet.cut_before true x_target block in *)
  let x_target' = Id.of_ast_id x_target in

  lookup [x_target'] block0 Relative_stack.empty gate_tree_root ()

let lookup_main program x_target =
  let main_task = lookup_top program x_target in
  Scheduler.push job_queue (fun () -> main_task);
  (* let timeout_task = Lwt_unix.with_timeout 2. (fun () -> main_task) in *)
  Lwt_main.run begin
    try%lwt
      Scheduler.run job_queue >>= fun _ ->
      Lwt.return [[]]
    with
    | Model_result model ->
      Lwt_io.printl @@ Z3.Model.to_string model >>= fun _ ->
      Lwt.return [[]]

    | Lwt_unix.Timeout ->
      prerr_endline "timeout";
      Lwt.return [[]]
  end

