open Batteries
open Odefa_ast.Ast
open Middle_step
open Tracelet
open Dbmc_lib
module C = Dbmc_lib.Constraint

type def_site =
  | At_clause of tl_clause
  | At_fun_para of bool * fun_block
  | At_chosen of cond_block

let lookup_main program x_target =
  let phi_set = ref [] in
  let add_phi phi = 
    phi_set := phi :: !phi_set in
  let map = Tunnel.annotate program x_target in
  let x_first = Ast_helper.first_var program in

  let defined x' block = 
    let x = Id.to_ast_id x' in
    match Tracelet.clause_of_x block x, block with
    | Some tc, _ -> At_clause tc
    | None, Main mb -> failwith "main block must have target"
    | None, Fun fb -> At_fun_para (fb.para = x, fb)
    | None, Cond cb -> At_chosen cb
  in

  let rec lookup (xs0 : Concrete_stack.t) block rel_stack =
    (* print_endline @@ show_clause_list (get_clauses block); *)
    let x, xs = List.hd xs0, List.tl xs0 in

    (* x can either be 
       1. the id of the clause, while the stack is singleton or not
       2. the argument of this fun
       3. the argument of furthur funs *)

    (* Inductive on definition site *)
    match defined x block with
    | At_clause tc -> (
        begin
          let (Clause (Var (x_def', _), rhs)) = tc.clause in
          let x_def = Id.of_ast_id x_def' in
          let block_point = id_of_block block in
          match rhs with 
          (* Value Discovery Main *)
          | Value_body v when block_point = id_main -> 
            (match v with
             | Value_function vf -> ()
             | _ -> add_phi @@ C.bind_v x v rel_stack
            );
            add_phi @@ C.concretize rel_stack
          | Input_body when block_point = id_main  ->
            add_phi @@ C.bind_input x rel_stack;
            add_phi @@ C.concretize rel_stack

          (* Value Discovery Non-Main *)
          | Value_body v when List.is_empty xs ->
            (match v with
             | Value_function vf -> add_phi @@ C.bind_fun x rel_stack x_def
             | _ -> add_phi @@ C.bind_v x v rel_stack
            );
            lookup [Id.of_ast_id x_first] block rel_stack
          | Input_body ->
            add_phi @@ C.bind_input x rel_stack;
            lookup [Id.of_ast_id x_first] block rel_stack

          (* Value Discard *)
          | Value_body(Value_function f) -> 
            add_phi @@ C.eq_lookups (x::xs) rel_stack xs rel_stack;
            lookup xs block rel_stack

          (* Alias *)
          | Var_body (Var (x', _)) ->
            let x' = Id.of_ast_id x' in
            add_phi @@ C.eq_lookups (x::xs) rel_stack (x'::xs) rel_stack;
            lookup (x'::xs) block rel_stack

          (* Binop *)
          | Binary_operation_body (Var (x1, _), bop, Var (x2, _)) ->
            let x1, x2 = Id.of_ast_id x1, Id.of_ast_id x2 in
            add_phi @@ C.bind_binop x x1 bop x2 rel_stack;
            lookup (x1::xs) block rel_stack;
            lookup (x2::xs) block rel_stack

          (* Fun Exit *)
          | Appl_body (Var (xf, _), Var (xv, _)) -> (
              let xf, xv = Id.of_ast_id xf, Id.of_ast_id xv in
              lookup [xf] block rel_stack;
              match tc.cat with
              | App funs -> (
                  let phis = List.map (fun fun_id ->
                      let fblock = Ident_map.find fun_id map in
                      let x' = Tracelet.ret_of fblock |> Id.of_ast_id in
                      let fun_id = Id.of_ast_id fun_id in
                      let rel_stack' = Relative_stack.push rel_stack x fun_id in
                      lookup [x'] fblock rel_stack';
                      C.and_ 
                        (C.eq_lookup x rel_stack x' rel_stack')
                        (C.bind_fun xf rel_stack ((id_of_block fblock) |> Id.of_ast_id))
                    ) funs in
                  add_phi (C.only_one phis)
                )
              | _ -> failwith "fun exit clauses"
            )

          (* Cond Bottom *)
          | Conditional_body (Var (x', _), e1, e2) -> (
              lookup [x' |> Id.of_ast_id] block rel_stack;
              let cond_tracelet = Ident_map.find x' map in
              let cond_block = match cond_tracelet with
                | Cond c when c.choice = None -> c
                | Cond _ -> failwith "conditional_body: not both"
                | _ -> failwith "conditional_body: ?"
              in
              let phis = List.map (fun beta ->
                  let ctracelet = 
                    Cond { cond_block with choice = Some beta }
                  in
                  let x_ret = Tracelet.ret_of ctracelet |> Id.of_ast_id in 
                  lookup [x_ret] ctracelet rel_stack;
                  let x' = Id.of_ast_id x' in
                  C.and_ 
                    (C.bind_v x' (Value_bool beta) rel_stack)
                    (C.eq_lookup x rel_stack x_ret rel_stack)
                )
                  [true; false] in
              add_phi (C.only_one phis)
            )
          | _ -> failwith "error clause cases"
        end
      )

    (* Fun Enter Parameter *)
    | At_fun_para (true, fb) ->
      let phis = List.map (fun callsite -> 
          let callsite_block = Tracelet.find_by_id callsite map in
          let tc = Tracelet.clause_of_x_exn callsite_block callsite in
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let x' = Id.of_ast_id x' in
            let x'' = Id.of_ast_id x'' in
            let x''' = Id.of_ast_id x''' in
            let fid = Id.of_ast_id fb.point in
            let rel_stack' = Relative_stack.pop rel_stack x' fid in
            lookup [x''] callsite_block rel_stack';
            lookup (x'''::xs) callsite_block rel_stack';
            C.and_ 
              (C.eq_lookups (x::xs) rel_stack (x'''::xs) rel_stack')
              (C.bind_fun x'' rel_stack' (id_of_block block |> Id.of_ast_id))
          | _ -> failwith "incorrect callsite in fun para"
        ) fb.callsites in
      add_phi (C.only_one phis)

    (* Fun Enter Non-Local *)
    | At_fun_para (false, fb) ->
      let phis = List.map (fun callsite -> 
          let callsite_block = Tracelet.find_by_id callsite map in
          let tc = Tracelet.clause_of_x_exn callsite_block callsite in
          match tc.clause with
          | (Clause (Var (x', _), Appl_body (Var (x'', _), Var (x''', _)))) ->
            let x' = Id.of_ast_id x' in
            let x'' = Id.of_ast_id x'' in
            let x''' = Id.of_ast_id x''' in
            let fid = Id.of_ast_id fb.point in
            let rel_stack' = Relative_stack.pop rel_stack x' fid in
            lookup [x''] callsite_block rel_stack';
            lookup (x''::x::xs) callsite_block rel_stack';
            C.and_ 
              (C.eq_lookups (x::xs) rel_stack (x'''::xs) rel_stack')
              (C.bind_fun x'' rel_stack (id_of_block block |> Id.of_ast_id))
          | _ -> failwith "incorrect callsite in fun non-local"
        )
          fb.callsites in
      add_phi (C.only_one phis)

    (* Cond Top 
       must be inside a then_ or else_ block
    *)
    | At_chosen cb -> 
      let condsite_block = Ident_map.find (outer_id_of_block block) map in
      let x2 = cb.cond |> Id.of_ast_id in
      let choice = BatOption.get cb.choice in
      add_phi (C.bind_v x2 (Value_bool choice) rel_stack);
      lookup [x2] condsite_block rel_stack;
      lookup (x::xs) condsite_block rel_stack

  in
  let block0 = Tracelet.find_by_id x_target map in
  (* let block0 = Tracelet.cut_before true x_target block in *)
  let x_target' = Id.of_ast_id x_target in

  lookup [x_target'] block0 Relative_stack.empty;
  !phi_set
