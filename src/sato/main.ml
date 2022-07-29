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

let get_value_type v = 
  match v with
  | Value_int _ -> Int_type
  | Value_bool _ -> Bool_type
  | Value_function _ -> Fun_type
  | Value_record (Record_value r) -> 
    let lbls = Ident_set.of_enum @@ Ident_map.keys r in
    Rec_type lbls

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
  let (program, on_to_odefa_maps, _) = 
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
        let (inputss, _, state) = 
          Dbmc.Main.main_details ~config:dbmc_config program 
        in
        match List.hd inputss with
        | Some inputs ->
          (* Getting the target clause *)
          let abort_var = state.target in
          let abort_cond_var = get_abort_cond_clause_id ab_pairs abort_var in
          let Clause (_, cls) as pre_inst = 
            get_pre_inst_equivalent_clause on_to_odefa_maps abort_cond_var 
          in
          (* let () = print_endline "This is the point of error: " in
          let () = print_endline @@ Odefa_ast.Ast_pp.show_clause pre_inst in *)
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
              let alias_map = session.alias_map in
              let find_alias x_with_stk ~key ~data acc =
                if (Interpreter.Ident_with_stack.equal key x_with_stk) then 
                  Hash_set.union acc data
                else
                  if Hash_set.mem data x_with_stk then
                    (Hash_set.iter data ~f:(Hash_set.add acc); 
                    Hash_set.add acc key;
                    acc)
                  else
                    acc
              in
              let rec find_source_cls cls_mapping xs =
                match xs with
                | [] -> failwith "Should have found a value definition clause!"
                | hd :: tl ->
                  let found = Hashtbl.find cls_mapping hd in
                  match found with
                  | Some cls -> cls
                  | None -> find_source_cls cls_mapping tl
              in
              let mk_match_err expected_type actual_val x x_stk = 
                match expected_type, actual_val with
                | Int_type, Value_int _| Bool_type, Value_bool _ -> []
                | _ -> 
                  let find_aliases = find_alias (x, x_stk) in
                  let match_aliases_raw =
                    let init_set = 
                      Hash_set.create (module Interpreter.Ident_with_stack)
                    in
                    Hash_set.add init_set (x, x_stk);
                    Hashtbl.fold alias_map ~init:init_set ~f:find_aliases
                    |> Hash_set.to_list
                  in
                  (* let () = print_endline @@ "This is the alias set" in
                  let () = 
                    print_endline @@ 
                    List.to_string ~f:(Interpreter.show_ident_with_stack) match_aliases_raw 
                  in *)
                  let match_val_source = 
                    find_source_cls session.val_def_map match_aliases_raw 
                  in
                  let match_aliases = 
                    match_aliases_raw
                    |> List.map ~f:(fun (x, _) -> x)
                  in
                  let actual_type = get_value_type actual_val in
                  let match_error = Odefa_ast.Error.Odefa_error.Error_match {
                    err_match_aliases = match_aliases;
                    err_match_val = match_val_source;
                    err_match_expected = expected_type;
                    err_match_actual = actual_type;
                  }
                  in
                  [match_error]
              in
              (
              match cls with
              (* If the point of error is a binary operation, we know that one of
                 the two operands must have taken the wrong type.
              *)
              | Binary_operation_body (Var (x1, _), op, Var (x2, _)) ->
                let expected_type = 
                  get_operand_type op
                in
                let (x1_val, x1_stk), (x2_val, x2_stk) = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (dv1, stk1) = Ident_map.find x1 final_env in
                    let (dv2, stk2) = Ident_map.find x2 final_env in
                    let v1, v2 = 
                      Interpreter.value_of_dvalue dv1,
                      Interpreter.value_of_dvalue dv2
                    in 
                    (v1, stk1), (v2, stk2)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                let left_error = mk_match_err expected_type x1_val x1 x1_stk in
                let right_error = mk_match_err expected_type x2_val x2 x2_stk in
                let errors = List.append left_error right_error in
                let () = print_endline @@ show_error inputs pre_inst errors in
                ()
              (* If it's a Not operation, we know that the operand has the
                 wrong type. *)
              | Not_body (Var (x, _)) ->
                let expected_type = Bool_type in
                let (x_val, x_stk) = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (dv, stk) = Ident_map.find x final_env in
                    let v = 
                      Interpreter.value_of_dvalue dv
                    in 
                    (v, stk)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                let error = mk_match_err expected_type x_val x x_stk in
                let () = print_endline @@ show_error inputs pre_inst error in
                ()
              (* If it's a function application, we know that we're applying to
                 a non-function. *)
              | Appl_body (Var (x, _), _) ->
                let expected_type = Fun_type in
                let (x_val, x_stk) = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (dv, stk) = Ident_map.find x final_env in
                    let v = 
                      Interpreter.value_of_dvalue dv
                    in 
                    (v, stk)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                let error = mk_match_err expected_type x_val x x_stk in
                let () = print_endline @@ show_error inputs pre_inst error in
                ()
              (* If it's a record projection, we know that we're projecting from
                 the wrong record. *)
              | Projection_body (Var (x, _), lbl) ->
                let expected_type = Rec_type (Ident_set.singleton lbl) in
                let (x_val, x_stk) = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (dv, stk) = Ident_map.find x final_env in
                    let v = 
                      Interpreter.value_of_dvalue dv
                    in 
                    (v, stk)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                let error = mk_match_err expected_type x_val x x_stk in
                let () = print_endline @@ show_error inputs pre_inst error in
                ()
              (* If it's a conditional, we know that condition itself is not a 
                 boolean type. *)
              | Conditional_body (Var (x, _), _, _) ->
                let expected_type = Bool_type in
                let (x_val, x_stk) = 
                  match ab_clo with
                  | AbortClosure final_env ->
                    let (dv, stk) = Ident_map.find x final_env in
                    let v = 
                      Interpreter.value_of_dvalue dv
                    in 
                    (v, stk)
                  | _ -> failwith "Shoud have run into abort here!"
                in
                let error = mk_match_err expected_type x_val x x_stk in
                let () = print_endline @@ show_error inputs pre_inst error in
                ()
              | _ -> exit 0
              )
            | _ -> failwith "Should terminate with an environment!"
          end
        | None -> search_all_targets tl
      with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                  raise ex)
  in
  search_all_targets target_vars;
  Dbmc.Log.close ()
