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
    Fmt.(pr "found in clause\n");
    At_clause tc
  | None, Main _mb -> failwith "main block must have target"
  | None, Fun fb -> 
    Fmt.(pr ("found in fun %s\n"
             ^^ "callsites %a\n"
             ^^ "%a = fun %a ->\n")
           (if fb.para = x then "local" else "nonlocal")
           (Dump.list string) (List.map (fun id -> 
               let (Ident s) = id in s
             ) fb.callsites)
           Ast_pp.pp_ident fb.point
           Ast_pp.pp_ident fb.para
        );
    At_fun_para (fb.para = x, fb)
  | None, Cond cb -> 
    Fmt.(pr ("found in cond\n"
             ^^ "%a = %a ? \n")
           Ast_pp.pp_ident cb.point
           Ast_pp.pp_ident cb.cond
        );
    At_chosen cb

let lookup_top program x_target : _ Lwt.t =
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
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

  let rec lookup (xs0 : Lookup_stack.t) block rel_stack : _ Lwt.t =
    let x, xs = List.hd xs0, List.tl xs0 in
    (* x can either be 
       1. the id of the clause, while the stack is singleton or not
       2. the argument of this fun
       3. the argument of furthur funs *)

    match defined x block with
    | At_clause tc -> (
        begin
          let (Clause (_, rhs)) = tc.clause in
          let block_point = id_of_block block in
          match rhs with 
          (* Value Discovery Main *)
          | Value_body v when block_point = id_main && List.is_empty xs -> 
            (match v with
             | Value_function _ -> ()
             | _ -> add_phi @@ C.bind_v x v rel_stack
            );
            add_phi @@ C.concretize rel_stack;
            Lwt.return_unit
          | Input_body when block_point = id_main  ->
            add_phi @@ C.bind_input x rel_stack;
            add_phi @@ C.concretize rel_stack;
            Lwt.return_unit

          (* Value Discovery Non-Main *)
          | Value_body v when List.is_empty xs ->
            (match v with
             | Value_function _vf -> add_phi @@ C.bind_fun x rel_stack x
             | _ -> add_phi @@ C.bind_v x v rel_stack
            );
            lookup [Id.of_ast_id x_first] block rel_stack
          | Input_body ->
            add_phi @@ C.bind_input x rel_stack;
            lookup [Id.of_ast_id x_first] block rel_stack

          (* Value Discard *)
          | Value_body(Value_function _f) -> 
            add_phi @@ C.eq_lookup (x::xs) rel_stack xs rel_stack;
            lookup xs block rel_stack

          (* Alias *)
          | Var_body (Var (x', _)) ->
            let x' = Id.of_ast_id x' in
            add_phi @@ C.eq_lookup (x::xs) rel_stack (x'::xs) rel_stack;
            lookup (x'::xs) block rel_stack

          (* Binop *)
          | Binary_operation_body (Var (x1, _), bop, Var (x2, _)) ->
            let x1, x2 = Id.of_ast_id x1, Id.of_ast_id x2 in
            add_phi @@ C.bind_binop x x1 bop x2 rel_stack;

            Lwt.both
              (lookup (x1::xs) block rel_stack)
              (lookup (x2::xs) block rel_stack)
            >>= fun _ -> Lwt.return_unit

          (* Fun Exit *)
          | Appl_body (Var (xf, _), Var (_xv, _)) -> (
              match tc.cat with
              | App fids -> (
                  let xf = Id.of_ast_id xf in
                  lookup [xf] block rel_stack >>= fun _ ->

                  let phis, sub_lookups = List.fold_right (fun fid (phis, sub_lookups) -> 
                      let fblock = Ident_map.find fid map in
                      let x' = Tracelet.ret_of fblock |> Id.of_ast_id in
                      let fid = Id.of_ast_id fid in
                      let rel_stack' = Relative_stack.push rel_stack x fid in

                      let phi = C.and_ 
                          (C.bind_fun xf rel_stack fid)
                          (C.eq_lookup (x::xs) rel_stack (x'::xs) rel_stack') in
                      let sub_lookup = lookup (x'::xs) fblock rel_stack' in

                      (phi::phis, sub_lookup::sub_lookups)
                    ) fids ([], []) in

                  add_phi (C.only_one phis);
                  Lwt.join sub_lookups
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

              lookup [x'] block rel_stack >>= fun _ ->

              let phis, sub_lookups = List.fold_right (fun beta (phis, sub_lookups) ->
                  let ctracelet = 
                    Cond { cond_block with choice = Some beta }
                  in
                  let x_ret = Tracelet.ret_of ctracelet |> Id.of_ast_id in 

                  let phi = C.and_ 
                      (C.bind_v x' (Value_bool beta) rel_stack)
                      (C.eq_lookup [x] rel_stack [x_ret] rel_stack) in
                  let sub_lookup = lookup [x_ret] ctracelet rel_stack in

                  (phi::phis, sub_lookup::sub_lookups)
                ) [true; false] ([],[]) in
              add_phi (C.only_one phis);
              Lwt.join sub_lookups
            )
          | _ -> failwith "error clause cases"
        end
      )

    (* Fun Enter Parameter *)
    | At_fun_para (true, fb) ->
      let fid = Id.of_ast_id fb.point in

      let phis, sub_lookups = List.fold_right (fun callsite (phis, sub_lookups) -> 
          let callsite_block, x', x'', x''' = fun_info_of_callsite callsite map in
          match Relative_stack.pop rel_stack x' fid with
          | Some callsite_stack -> 
            let phi = C.and_
                (C.eq_lookup (x::xs) rel_stack (x'''::xs) callsite_stack)
                (C.bind_fun x'' callsite_stack fid) in
            let sub_lookup = 
              lookup [x''] callsite_block callsite_stack >>= fun _ ->
              lookup (x'''::xs) callsite_block callsite_stack
            in
            (phi::phis, sub_lookup::sub_lookups)

          | None -> (phis, sub_lookups)
        ) fb.callsites ([],[]) in
      add_phi (C.only_one phis);
      Lwt.join sub_lookups

    (* Fun Enter Non-Local *)
    | At_fun_para (false, fb) ->
      let fid = Id.of_ast_id fb.point in

      let phis, sub_lookups = List.fold_right (fun callsite (phis, sub_lookups) -> 
          let callsite_block, x', x'', _x''' = fun_info_of_callsite callsite map in
          match Relative_stack.pop rel_stack x' fid with
          | Some callsite_stack -> 
            let phi = C.and_ 
                (C.eq_lookup (x::xs) rel_stack (x''::x::xs) callsite_stack)
                (C.bind_fun x'' callsite_stack fid) in 
            let sub_lookup =
              lookup [x''] callsite_block callsite_stack >>= fun _ ->
              lookup (x''::x::xs) callsite_block callsite_stack in
            (phi::phis, sub_lookup::sub_lookups)
          | None -> (phis, sub_lookups)
        ) fb.callsites ([],[]) in

      add_phi (C.only_one phis);
      Lwt.join sub_lookups

    (* Cond Top *)
    | At_chosen cb -> 
      let condsite_block = Ident_map.find (outer_id_of_block block) map in
      let x2 = cb.cond |> Id.of_ast_id in
      let choice = BatOption.get cb.choice in
      add_phi (C.bind_v x2 (Value_bool choice) rel_stack);
      lookup [x2] condsite_block rel_stack >>= fun _ ->
      lookup (x::xs) condsite_block rel_stack

  in
  let block0 = Tracelet.find_by_id x_target map in
  (* let block0 = Tracelet.cut_before true x_target block in *)
  let x_target' = Id.of_ast_id x_target in

  lookup [x_target'] block0 Relative_stack.empty >>= fun _  ->
  (!phi_set)
  |> Constraint.integrate_stack 
  |> Constraint.simplify
  |> Lwt.return

let lookup_main program x_target =
  Lwt_main.run (lookup_top program x_target)