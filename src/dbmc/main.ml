open Core
open Lwt.Infix
open Jayil
open Jayil.Ast
open Cfg
open Ddpa
open Log.Export

let check_expected_input ~(config : Global_config.t) ~(state : Global_state.t) =
  match config.mode with
  | Dbmc_check inputs ->
      (* = Input_feeder.from_list inputs *)
      let history = ref [] in
      let input_feeder = Input_feeder.memorized_from_list inputs history in
      let session =
        Interpreter.expected_input_session input_feeder config.target
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
      if Solver.check_expected_input_sat expected_stk !history
      then ()
      else failwith "expected input leads to a wrong place."
  | _ -> ()

let get_input ~(config : Global_config.t) ~(state : Global_state.t) model
    (target_stack : Concrete_stack.t) =
  let history = ref [] in
  let input_feeder = Input_feeder.from_model ~history model target_stack in
  let session =
    let max_step = config.run_max_step in
    Interpreter.create_session ?max_step target_stack state config input_feeder
  in
  (try Interpreter.eval session state.program with
  | Interpreter.Found_target _ -> ()
  | ex -> raise ex) ;
  List.rev !history

let handle_graph (config : Global_config.t) state model =
  if config.debug_graph
  then () (* Graphviz.output_graph ~model ~testname:config.filename state *)
  else ()

let handle_found (config : Global_config.t) (state : Global_state.t) model c_stk
    =
  LLog.info (fun m ->
      m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident config.target
        Concrete_stack.pp c_stk) ;
  (* LLog.debug (fun m ->
      m "Nodes picked states\n%a\n"
        (Fmt.Dump.iter_bindings
           (fun f c ->
             Hashtbl.iteri c ~f:(fun ~key ~data:_ ->
                 f key (Riddler.is_picked (Some model) key)))
           Fmt.nop Lookup_key.pp Fmt.bool)
        state.key_map) ; *)
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.term_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
      else ()) ;
  (* Global_state.refresh_picked state model; *)
  handle_graph config state (Some model) ;

  let inputs_from_interpreter = get_input ~config ~state model c_stk in
  (* check expected inputs *)
  check_expected_input ~config ~state ;
  ([ inputs_from_interpreter ], false (* true *), Some (model, c_stk))

let[@landmark] main_with_state_lwt ~(config : Global_config.t)
    ~(state : Global_state.t) =
  let job_queue =
    let open Scheduler in
    let cmp t1 t2 =
      Int.compare (Lookup_key.length t1.key) (Lookup_key.length t2.key)
    in
    Scheduler.create ~cmp ()
  in
  let post_check_dbmc is_timeout =
    match Riddler.check state config with
    | Some { model; c_stk } -> handle_found config state model c_stk
    | None ->
        SLog.info (fun m -> m "UNSAT") ;
        if config.debug_model
        then
          SLog.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ()))
        else () ;
        handle_graph config state None ;
        ([], is_timeout, None)
  in
  let post_check_ddse is_timeout =
    if config.debug_model
    then SLog.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ()))
    else () ;
    handle_graph config state None ;
    ([], is_timeout, None)
  in
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
          Lookup.run_dbmc ~config ~state job_queue >>= fun _ ->
          Lwt.return (post_check_dbmc false)
      | Global_config.E_ddse ->
          Lookup.run_ddse ~config ~state job_queue >>= fun _ ->
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

let main_details ~config program =
  let state = Global_state.create config program in
  let inputs, is_timeout, _model =
    Lwt_main.run (main_with_state_lwt ~config ~state)
  in
  (inputs, is_timeout, state)

let search_input ~config program =
  main_details ~config program |> fun (a, _b, _c) -> a

let check_input ~(config : Global_config.t) program inputs =
  let mode = Global_config.Dbmc_check inputs in
  let config = { config with mode } in
  main_details ~config program |> fun (_a, b, _c) -> b

let from_commandline () =
  let config = Argparse.parse_commandline_config () in
  Log.init config ;
  let is_instrumented = config.is_instrumented in
  let program = File_util.read_source ~is_instrumented config.filename in
  (try
     let inputss = search_input ~config program in

     match List.hd inputss with
     | Some inputs -> Fmt.pr "[%s]@;" (Std.string_of_inputs inputs)
     | None -> Fmt.pr "Unreachable"
   with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
              raise ex) ;
  Log.close ()
