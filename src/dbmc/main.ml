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

let check_expected_input ~(config : Global_config.t) ~(state : Global_state.t) =
  match config.mode with
  | Dbmc_check inputs ->
      let history = ref [] in
      let input_feeder = Input_feeder.memorized_from_list inputs history in
      let is_check_per_step = config.is_check_per_step in
      let session =
        Interpreter.expected_input_session ~is_check_per_step input_feeder
          config.target
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
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.term_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
      else ()) ;
  handle_graph config state (Some model) ;

  let inputs_from_interpreter = get_input ~config ~state model c_stk in
  check_expected_input ~config ~state ;
  ([ inputs_from_interpreter ], false (* true *), Some (model, c_stk))

let[@landmark] main_lookup ~(config : Global_config.t) ~(state : Global_state.t)
    =
  let post_check_dbmc is_timeout =
    match Riddler.check state config with
    | Some { model; c_stk } -> handle_found config state model c_stk
    | None ->
        SLog.info (fun m -> m "UNSAT") ;
        if config.is_check_per_step
        then check_expected_input ~config ~state
        else () ;
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
  let program =
    Dj_common.File_utils.read_source ~is_instrumented config.filename
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
