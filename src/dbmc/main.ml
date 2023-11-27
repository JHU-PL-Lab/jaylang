open Core
open Dj_common
open Lwt.Infix
open Jayil
open Jayil.Ast
open Cfg
open Ddpa
open Log.Export

type result_no_state =
  int option list list * bool * (Z3.Model.model * Concrete_stack.t) option

type result = {
  inputss : int option list list;
  is_timeout : bool;
  symbolic_result : (Z3.Model.model * Concrete_stack.t) option;
  state : Global_state.t;
}

let check_expected_input ~(config : Global_config.t) ~(state : Global_state.t)
    inputs =
  let history = ref [] in
  let session =
    let input_feeder = Input_feeder.memorized_from_list history inputs in
    let mode = Interpreter.With_target_x config.target in
    Interpreter.create_session state config mode input_feeder
  in
  try Interpreter.eval session state.info.program with
  | Interpreter.Found_target target ->
      Solver.check_expected_input_sat target.stk !history state.solve.solver
  | ex -> false

let get_input ~(config : Global_config.t) ~(state : Global_state.t) model
    (target_stack : Concrete_stack.t) =
  let history = ref [] in
  let input_feeder = Checker.mk_input_feeder ~history model target_stack in
  let session =
    let max_step = config.run_max_step in
    let mode = Interpreter.With_full_target (config.target, target_stack) in
    let debug_mode =
      if config.is_check_per_step
      then
        let clause_cb x c_stk v =
          let stk = Rstack.relativize target_stack c_stk in
          let key =
            Lookup_key.of3 x stk
              (Cfg.find_reachable_block x state.info.block_map)
          in
          let key_z = Riddler.key_to_var key in
          let key_picked = Riddler.picked key in
          let eq_z =
            match v with
            | Value_function _ -> Riddler.true_
            | _ -> Riddler.eqv key v
          in
          state.solve.phis_staging <-
            key_picked :: eq_z :: state.solve.phis_staging ;
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
  (try Interpreter.eval session state.info.program with
  | Interpreter.Found_target _ -> ()
  | ex -> raise ex) ;
  List.rev !history

let handle_found (config : Global_config.t) (state : Global_state.t) model c_stk
    : result_no_state =
  LLog.info (fun m ->
      m "{target}\nx: %a\ntgt_stk: %a\n\n" Ast.pp_ident config.target
        Concrete_stack.pp c_stk) ;
  Observe.update_rstk_pick config state model ;
  Observe.handle_both config state (Some model) ;
  let inputs_from_interpreter = get_input ~config ~state model c_stk in
  ([ inputs_from_interpreter ], false, Some (model, c_stk))

let handle_not_found (config : Global_config.t) (state : Global_state.t)
    is_timeout : result_no_state =
  SLog.info (fun m -> m "UNSAT") ;
  if config.debug_model
  then
    SLog.debug (fun m ->
        m "Solver Phis: %s" (Solver.string_of_solver state.solve.solver)) ;
  Observe.handle_both config state None ;
  ([], is_timeout, None)

let[@landmark] main_lookup ~(config : Global_config.t) ~(state : Global_state.t)
    =
  let post_check is_timeout =
    match config.engine with
    | Global_config.E_dbmc -> (
        match Checker.check state config with
        | Some { model; c_stk } -> handle_found config state model c_stk
        | None -> handle_not_found config state is_timeout)
    | Global_config.E_ddse -> handle_not_found config state is_timeout
  in
  try%lwt
    (Lwt.async_exception_hook :=
       fun exn ->
         match exn with
         | Riddler.Found_solution { model; c_stk } ->
             ignore @@ raise (Riddler.Found_solution { model; c_stk })
         | exn -> failwith (Stdlib.Printexc.to_string exn)) ;
    Timeout_utils.no_matter config.timeout (fun () ->
        Lookup.run_dbmc ~config ~state >|= fun _ -> post_check false)
  with
  | Riddler.Found_solution { model; c_stk } ->
      Lwt.return (handle_found config state model c_stk)
  | Lwt_unix.Timeout ->
      prerr_endline "real timeout" ;
      Lwt.return @@ post_check true
  | exn ->
      Fmt.pr "exn = %s" (Stdlib.Printexc.to_string exn) ;
      Lwt.return @@ handle_not_found config state false

(* The main function should have only one function that doing all the work.
    The function is configured by a pre-tuned argument config.
    The function returns one result that concerning all the mode.
    The config setting and the result filtering are processed by other
    pre-/post-processing functions to keep the real main unique and versatile.
*)

(* entry functions *)

let dump_result ~(config : Global_config.t) symbolic_result =
  let dump_result symbolic_result =
    match symbolic_result with
    | None -> ()
    | Some (model, c_stk) ->
        (* Fmt.pr "%d" state.tree_size ; *)
        Fmt.pr "%s" (Concrete_stack.show c_stk)
  in
  match config.mode with
  | Dbmc_search -> dump_result symbolic_result
  | Dbmc_check _ -> dump_result symbolic_result
  | _ -> Fmt.pr "."

let main_lwt ~config ~state program =
  let%lwt inputss, is_timeout, symbolic_result = main_lookup ~config ~state in
  dump_result ~config symbolic_result ;
  Lwt.return { inputss; is_timeout; symbolic_result; state }

let main_top_lwt ~config program =
  let state = Global_state.create config program in
  main_lwt ~config ~state program

let main_top ~config program = Lwt_main.run (main_top_lwt ~config program)

let main_config_lwt config =
  let program = Global_config.read_source config in
  main_top_lwt ~config program

let main_config config = Lwt_main.run (main_config_lwt config)

type stage_result =
  | Argparse of Global_config.t
  | Load_file of Jayil.Ast.expr
  | State_init of Global_state.t
  | Lookup of unit
  | All_done of unit

exception Stage_result of stage_result

let main_commandline () =
  try
    let config = Argparse.parse_commandline () in
    if Global_config.equal_stage config.stage Global_config.Argparse
    then raise (Stage_result (Argparse config)) ;

    Log.init config ;

    let program = Global_config.read_source config in
    if Global_config.equal_stage config.stage Global_config.Load_file
    then raise (Stage_result (Load_file program)) ;

    let config =
      if config.expected_from_file
      then Global_config.load_expect config
      else config
    in

    let state = Global_state.create config program in
    if Global_config.equal_stage config.stage Global_config.State_init
    then raise (Stage_result (State_init state)) ;
    let result = Lwt_main.run (main_lwt ~config ~state program) in

    if Global_config.equal_stage config.stage Global_config.Lookup
    then raise (Stage_result (Lookup ())) ;
    let { inputss; is_timeout; state; _ } = result in

    (match config.mode with
    | Dbmc_search -> (
        match List.hd inputss with
        | Some inputs -> Fmt.pr "[%s]@;" (Std.string_of_opt_int_list inputs)
        | None -> Fmt.pr "Unreachable")
    | Dbmc_check inputs ->
        (match List.hd inputss with
        | Some _inputs ->
            if not (check_expected_input ~config ~state inputs)
            then failwith "expected input cannot reach the target"
        | None -> failwith "should not be reachable") ;
        Fmt.pr "%B" is_timeout
    | Dbmc_perf -> Fmt.pr "."
    | _ -> ()) ;
    (* TODO: mimic a `finally` for it *)
    Log.close ()
  with
  | Stage_result r -> (
      match r with
      | Argparse _ -> Fmt.pr "Reach Argparse"
      | Load_file _ -> Fmt.pr "Reach Load_file"
      | _ -> Fmt.pr "Other stages")
  | Sys_error ex -> Fmt.pr "exception Sys_error(%s)" ex
  | ex -> raise ex
