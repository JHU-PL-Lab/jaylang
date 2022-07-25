open Core
open Sato_args
open Odefa_ast.Ast
open Odefa_natural.On_to_odefa_maps

let get_operand_type op = 
  match op with
  | Binary_operator_plus | Binary_operator_minus
  | Binary_operator_times | Binary_operator_divide
  | Binary_operator_modulus | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to | Binary_operator_equal_to
  | Binary_operator_not_equal_to -> Int_type
  | Binary_operator_and | Binary_operator_or -> Bool_type

(* Enumerate all aborts in a program *)
let rec enum_all_aborts_in_expr expr : (ident * abort_value) list =
  let Expr clauses = expr in
  List.concat @@ List.map ~f:enum_all_aborts_in_clause clauses 

and enum_all_aborts_in_clause clause : (ident * abort_value) list =
  let Clause (Var (cls_id, _), body) = clause in
  match body with
  | Conditional_body (Var (pred_id, _), e1, e2) ->
    begin
      let enum_ret_abort e branch =
        let Expr(c_list) = e in
        match List.last c_list with
        | None -> []
        | Some cls ->
          begin
            match cls with
            | Clause (Var (abort_id, _), Abort_body) ->
              let abort_val = {
                abort_conditional_ident = cls_id;
                abort_predicate_ident = pred_id;
                abort_conditional_branch = branch;
              }
              in
              [(abort_id, abort_val)]
            | _ -> []
          end
      in
      []
      |> List.append (enum_all_aborts_in_expr e1)
      |> List.append (enum_ret_abort e1 true)
      |> List.append (enum_all_aborts_in_expr e2)
      |> List.append (enum_ret_abort e2 false)
    end
  | Value_body v ->
    enum_all_aborts_in_value v
  | Abort_body (* Aborts are enumerated in conditionals *)
  | Assume_body _
  | Assert_body _
  | Var_body _
  | Input_body
  | Appl_body (_, _)
  | Binary_operation_body (_, _, _)
  | Not_body _
  | Match_body (_, _)
  | Projection_body (_, _) ->
    []

and enum_all_aborts_in_value value : (ident * abort_value) list =
  match value with
  | Value_function (Function_value (_, e)) ->
    enum_all_aborts_in_expr e
  | Value_int _ | Value_bool _ | Value_record _ ->
    []
;;

(* let list_instrument_conditionals (e : expr) : (abort_value, ident) list =
  e
  |> enum_all_aborts_in_expr
  (* |> List.map ~f:(fun (_, abort_val) -> abort_val.abort_conditional_ident) *)
  |> List.map ~f:(fun ((abort_id, _) as ab_val) -> (ab_var, abort_id))
  |> List.rev
;; *)

let get_target_vars 
  (ab_pairs : (ident * abort_value) list) : ident list =
  ab_pairs
  |> List.map ~f:(fun (abort_id, _) -> abort_id)
;;

let get_abort_cond_clause_id
  (ab_pairs : (ident * abort_value) list) (ab_id : ident) : ident =
  let mapper (abort_id, ab_val) = 
    if Ident.equal abort_id ab_id 
    then
      Some ab_val.abort_conditional_ident
    else
      None
  in
  let clause_opt = List.find_map ab_pairs ~f:mapper in
  match clause_opt with
  | Some cls -> cls
  | None -> failwith "Should have a corresponding clause here!"
;;


let create_initial_dmbc_config (sato_config : Sato_args.t) 
  : Dbmc.Global_config.t =
  (* Extract basic configuration from sato args *)
  let filename = sato_config.filename in
  let ddpa_ver = sato_config.ddpa_c_stk in
  let max_step = sato_config.run_max_step in
  let timeout = sato_config.timeout in
  let open Dbmc.Global_config in
  {
    target = Dbmc.Id.(Ident "target");
    filename = filename;
    engine = E_dbmc;
    is_instrumented = false;
    mode = Sato;
    ddpa_c_stk = ddpa_ver;
    run_max_step = max_step;
    timeout = timeout;
    stride_init = 100;
    stride_max = 100;
    log_level = None;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
  }
;;

let show_error inputs err_loc errors =
  "** Odefa Type Errors **\n" ^
  (Printf.sprintf "- Input sequence  : %s\n" (Dbmc.Std.string_of_inputs inputs)) ^
  (Printf.sprintf "- Found at clause : %s\n" (Odefa_ast.Ast_pp.show_clause err_loc)) ^
  (* (Printf.sprintf "- Found in steps  : %s\n" (string_of_int error.err_steps)) ^ *)
  "--------------------\n" ^
  (String.concat ~sep:"\n--------------------\n"
    @@ List.map ~f:Odefa_ast.Error.Odefa_error.show errors)

let main_commandline () =
  let sato_config = Argparse.parse_commandline_config () in
  let dbmc_config_init = create_initial_dmbc_config sato_config in
  let (pre_inst_program, program, on_to_odefa_maps, _) = 
    File_util.read_source_sato dbmc_config_init.filename 
  in
  let ab_pairs = enum_all_aborts_in_expr program in
  let target_vars = get_target_vars ab_pairs in
  let rec search_all_targets (remaining_targets : ident list) =
    match remaining_targets with
    | [] -> print_endline "No errors found."
    | hd :: tl ->
      let dbmc_config = 
        { dbmc_config_init with target = hd }
      in
      (* Right now we're stopping after one error is found. *)
      (try
        let open Dbmc in
        let (inputss, _, state, ret_opt) = Dbmc.Main.main_details_ret_model ~config:dbmc_config program in
        (* Getting the target clause *)
        let Expr c_list = pre_inst_program in 
        let source_map = 
          let alist = 
            List.map ~f:(fun (Clause (Var (x, _), cls_body)) -> (x, cls_body)) c_list
          in
          Hashtbl.of_alist_exn (module Ident_new) alist
        in
        match (List.hd inputss, ret_opt) with
        | (Some inputs, Some (_, c_stk)) ->
          let abort_var = state.target in
          let abort_cond_var = get_abort_cond_clause_id ab_pairs abort_var in
          let Clause (_, cls) as pre_inst = 
            get_pre_inst_equivalent_clause on_to_odefa_maps abort_cond_var 
          in
          let () = print_endline "This is the point of error: " in
          let () = print_endline @@ Odefa_ast.Ast_pp.show_clause pre_inst in
          begin
            let session = 
              { Interpreter.default_session with input_feeder = Input_feeder.from_list inputs }
            in
            try
              (
              Interpreter.eval_verbose session program
              )
            with
            | Interpreter.Terminate_with_env (_, ab_clo) ->
              match cls with
              (* If the point of error is a binary operation, we know that one of
                 the two operands must have taken the wrong type.
              *)
              | Binary_operation_body ((Var (x1, _) as v1), op, (Var (x2, _) as v2)) ->
                let expected_type = 
                  get_operand_type op
                in
                let c_stk_actual = 
                  let tl = 
                    c_stk 
                    |> Concrete_stack.to_list 
                    |> List.rev
                    |> List.tl_exn
                    |> List.rev
                  in
                  Concrete_stack.of_list @@ tl
                in
                let x1_val, x2_val = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (v1, _) = Ident_map.find x1 final_env in
                    let (v2, _) = Ident_map.find x2 final_env in
                    (v1, v2)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                failwith "TBI!"
                (* let c_body1 = Hashtbl.find_exn source_map x1 in
                let c_body2 = Hashtbl.find_exn source_map x2 in
                let alias_map = session.alias_map in
                let c_stk_actual = 
                  let tl = List.tl_exn @@ Concrete_stack.to_list c_stk in
                  Concrete_stack.of_list @@ tl
                in
                let x1_with_stk = (x1, c_stk_actual) in
                (* let () = print_endline @@ show_ident x1 ^ " @ " ^ Concrete_stack.show c_stk_actual in *)
                let find_alias ((x, _) as x_with_stk) ~key ~data acc =
                  if (Interpreter.Ident_with_stack.equal key x_with_stk) then 
                    data
                    |> Hash_set.to_list
                    |> List.map ~f:(fun (x_id, _) -> x_id)
                    |> fun l -> x :: l
                  else
                    if Hash_set.mem data x_with_stk then
                      data
                      |> Hash_set.to_list
                      |> List.map ~f:(fun (x_id, _) -> x_id)
                      |> fun l -> x :: l
                    else
                      acc
                in
                let find_left = find_alias (x1, c_stk) in
                let find_right = find_alias (x2, c_stk) in
                let left_aliases =
                  Hashtbl.fold alias_map ~init:[x1] ~f:find_left
                in
                let right_aliases =
                  Hashtbl.fold alias_map ~init:[x2] ~f:find_right
                in
                    
                let error = 
                  Odefa_ast.Error.Odefa_error.Error_binop
                  { err_binop_left_aliases = left_aliases;
                    err_binop_right_aliases = right_aliases;
                    err_binop_left_val = c_body1;
                    err_binop_right_val = c_body2;
                    err_binop_operation = (Odefa_ast.Ast.Var_body v1, op, Odefa_ast.Ast.Var_body v2);
                  } 
                in
                let () = print_endline @@ show_error inputs pre_inst [error] in
                () *)
              | _ -> exit 0
              (* let () = 
              Ident_map.iter (fun x _v -> print_endline @@ show_ident x) env
              in
              () *)
            (* | _ -> failwith "Should terminate with an environment!" *)
          end
        | None, _ -> search_all_targets tl
        | Some _, None -> failwith "Shouldn't happen here!"
      with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                  raise ex)
  in
  search_all_targets target_vars;
  Dbmc.Log.close ()
