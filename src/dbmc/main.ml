open Core
open Dj_common
open Lwt.Infix
open Jayil
open Jayil.Ast
open Cfg
open Ddpa
open Log.Export

type result = {
  inputss : int option list list;
  is_timeout : bool;
  is_checked : bool option;
  symbolic_result : (Z3.Model.model * Concrete_stack.t) option;
  state : Global_state.t;
}

let check_expected_input ~(config : Global_config.t) ~(state : Global_state.t)
    inputs =
  let history = ref [] in
  let session =
    let input_feeder = Input_feeder.memorized_from_list inputs history in
    let mode = Interpreter.With_target_x config.target in
    Interpreter.create_session state config mode input_feeder
  in
  let expected_stk =
    try Interpreter.eval session state.program with
    | Interpreter.Found_target target ->
        Fmt.(
          pr "[Expected]%a"
            (list (Std.pp_tuple3 Id.pp Concrete_stack.pp (option int))))
          !history ;
        target.stk
    | ex -> raise ex
  in
  if Solver.check_expected_input_sat expected_stk !history state.solver
  then ()
  else failwith "expected input leads to a wrong place."

let get_input ~(config : Global_config.t) ~(state : Global_state.t) model
    (target_stack : Concrete_stack.t) =
  let history = ref [] in
  let input_feeder = Input_feeder.from_model ~history model target_stack in
  let session =
    let max_step = config.run_max_step in
    let mode = Interpreter.With_full_target (config.target, target_stack) in
    let debug_mode =
      if config.is_check_per_step
      then
        let clause_cb x c_stk v =
          let stk = Rstack.relativize target_stack c_stk in
          let key =
            Lookup_key.of3 x stk (Cfg.find_block_by_id x state.block_map)
          in
          let key_z = Riddler.key_to_var key in
          let key_picked = Riddler.picked key in
          let eq_z =
            match v with
            | Value_function _ -> Riddler.true_
            | _ -> Riddler.eqv key v
          in
          state.phis_staging <- key_picked :: eq_z :: state.phis_staging ;
          let info =
            Fmt.str "[Con]: %a %a = %a \n[Sym] %a\n\n" Id.pp x Concrete_stack.pp
              c_stk Jayil.Pp.value v Lookup_key.pp key
          in
          Fmt.pr "[Check] %s" info ;

          match Checker.check state config with
          | Some _ -> ()
          | None -> failwith @@ "step check failed"
        in
        Interpreter.Debug_clause clause_cb
      else Interpreter.No_debug
    in
    Interpreter.create_session ?max_step ~debug_mode state config mode
      input_feeder
  in
  (try Interpreter.eval session state.program with
  | Interpreter.Found_target _ -> ()
  | ex -> raise ex) ;
  List.rev !history

let handle_both (config : Global_config.t) (state : Global_state.t) model =
  (* print graph *)
  if config.debug_graph
  then () (* Graphviz.output_graph ~model ~testname:config.filename state *)
  else () ;
  Observe.process_rstk_stat_map config state ;
  Observe.dump_lookup_status state ;
  SLog.warn (fun m ->
      m "@,%a"
        Fmt.(vbox (list ~sep:sp Check_info.pp))
        (List.rev state.check_infos)) ;
  ()

let handle_found (config : Global_config.t) (state : Global_state.t) model c_stk
    =
  LLog.info (fun m ->
      m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident config.target
        Concrete_stack.pp c_stk) ;
  Observe.update_rstk_pick config state model ;
  handle_both config state (Some model) ;
  print_endline @@ Concrete_stack.show c_stk ;

  print_endline @@ string_of_int state.tree_size ;

  let inputs_from_interpreter = get_input ~config ~state model c_stk in
  (match config.mode with
  | Dbmc_check inputs -> check_expected_input ~config ~state inputs
  | _ -> ()) ;
  ([ inputs_from_interpreter ], false, Some (model, c_stk))

let handle_not_found (config : Global_config.t) (state : Global_state.t)
    is_timeout =
  SLog.info (fun m -> m "UNSAT") ;
  (* (match config.mode with
     | Dbmc_check inputs -> check_expected_input ~config ~state inputs
     | _ -> ()) ; *)
  if config.debug_model
  then
    SLog.debug (fun m ->
        m "Solver Phis: %s" (Solver.string_of_solver state.solver)) ;
  handle_both config state None ;
  ([], is_timeout, None)

let[@landmark] main_lookup ~(config : Global_config.t) ~(state : Global_state.t)
    =
  let post_check_dbmc is_timeout =
    match Checker.check state config with
    | Some { model; c_stk } -> handle_found config state model c_stk
    | None -> handle_not_found config state is_timeout
  in
  let post_check_ddse is_timeout = handle_not_found config state is_timeout in
  try%lwt
    (Lwt.async_exception_hook :=
       fun exn ->
         match exn with
         | Riddler.Found_solution { model; c_stk } ->
             ignore @@ raise (Riddler.Found_solution { model; c_stk })
         | exn -> failwith (Caml.Printexc.to_string exn)) ;
    let do_work () =
      match config.engine with
      | Global_config.E_dbmc ->
          Lookup.run_dbmc ~config ~state >>= fun _ ->
          Lwt.return (post_check_dbmc false)
      | Global_config.E_ddse ->
          Lookup.run_ddse ~config ~state >>= fun _ ->
          Lwt.return (post_check_ddse false)
    in
    match config.timeout with
    | Some ts -> Lwt_unix.with_timeout (Time.Span.to_sec ts) do_work
    | None -> do_work ()
  with
  | Riddler.Found_solution { model; c_stk } ->
      Lwt.return (handle_found config state model c_stk)
  | Lwt_unix.Timeout -> (
      prerr_endline "lookup: timeout" ;
      match config.engine with
      | Global_config.E_dbmc -> Lwt.return (post_check_dbmc true)
      | Global_config.E_ddse -> Lwt.return (post_check_ddse true))

(* entry functions *)

let main_lwt ~config program =
  let state = Global_state.create config program in
  let%lwt inputss, is_timeout, symbolic_result = main_lookup ~config ~state in
  let result =
    { inputss; is_timeout; is_checked = None; symbolic_result; state }
  in
  Lwt.return result

let main ~config program = Lwt_main.run (main_lwt ~config program)

let from_commandline () =
  let config = Argparse.parse_commandline_config () in
  Log.init config ;
  let is_instrumented = config.is_instrumented in
  let target_var = Var (config.target, None) in
  let program =
    Dj_common.File_utils.read_source ~is_instrumented ~consts:[ target_var ]
      config.filename
  in
  (try
     let result = main ~config program in
     let { inputss; is_timeout; state; _ } = result in

     match config.mode with
     | Dbmc_search -> (
         match List.hd inputss with
         | Some inputs -> Fmt.pr "[%s]@;" (Std.string_of_inputs inputs)
         | None -> Fmt.pr "Unreachable")
     | Dbmc_check inputs -> Fmt.pr "%B" is_timeout
     | _ -> ()
   with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
              raise ex) ;
  Log.close ()
