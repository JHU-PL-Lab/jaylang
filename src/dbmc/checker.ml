open Core
open Dj_common
open Riddler
open SuduZ3
open Log.Export

let eager_check (state : Global_state.t) (config : Global_config.t) target
    assumption =
  let _ = (state, config) in
  let unfinish_lookup =
    Hash_set.to_list state.lookup_created
    (* |> List.map ~f:(fun key ->
           if not (Lookup_key.equal key target)
           then Solver.SuduZ3.not_ (Riddler.picked key)
           else Riddler.picked key) *)
    |> List.map ~f:(fun key -> picked key @=> picked target)
  in
  let list_fix =
    Hashtbl.to_alist state.smt_lists
    |> List.map ~f:(fun (_key, _i) ->
           picked target (* SuduZ3.not_ (pick_key_list key i) *))
  in
  let phi_used_once =
    unfinish_lookup @ [ picked target ] @ list_fix @ assumption
  in

  let check_result =
    Solver.check state.solver state.phis_staging phi_used_once
  in
  Global_state.clear_phis state ;
  SLog.debug (fun m -> m "Eager check") ;
  SLog.debug (fun m ->
      m "Solver Phis: %s" (Solver.string_of_solver state.solver)) ;
  SLog.debug (fun m ->
      m "Used-once Phis (eager): %a"
        Fmt.(list ~sep:sp string)
        (List.map ~f:Z3.Expr.to_string phi_used_once)) ;
  match check_result with
  | Result.Ok _model ->
      (* Fmt.pr "eager_check SAT\n" ; *)
      true
  | Result.Error _exps ->
      (* Fmt.pr "eager_check UNSAT\n" ; *)
      false

let step_eager_check (state : Global_state.t) (config : Global_config.t) target
    assumption stride =
  (* state.tree_size <- state.tree_size + 1 ; *)
  if state.tree_size mod !stride = 0
  then (
    if !stride < config.stride_max
    then (
      stride := !stride * 2 ;
      if !stride > config.stride_max then stride := config.stride_max else ())
    else () ;
    eager_check state config target assumption)
  else true
(* eager_check state config target assumption *)

let check ?(verbose = true) (state : Global_state.t) (config : Global_config.t)
    : result_info option =
  if verbose
  then LLog.info (fun m -> m "Search Tree Size:\t%d" state.tree_size)
  else () ;
  let unfinish_lookup =
    Hash_set.to_list state.lookup_created
    |> List.map ~f:(fun key -> Solver.SuduZ3.not_ (picked key))
  in
  let list_fix =
    Hashtbl.to_alist state.smt_lists
    |> List.map ~f:(fun (key, i) -> SuduZ3.not_ (pick_key_list key i))
  in
  let phi_used_once = unfinish_lookup @ list_fix in

  LLog.info (fun m -> m "before check:\t%d" state.tree_size) ;

  let verbose = verbose && config.debug_model in
  if verbose
  then
    SLog.debug (fun m ->
        m "Used-once Phis:@?@\n@[<v>%a@]"
          Fmt.(list ~sep:cut string)
          (List.map ~f:Z3.Expr.to_string phi_used_once))
  else () ;

  let solver_result =
    Solver.check ~verbose state.solver state.phis_staging phi_used_once
  in
  Global_state.clear_phis state ;

  LLog.info (fun m -> m "before model:\t%d" state.tree_size) ;

  let check_result =
    match solver_result with
    | Result.Ok model ->
        if verbose
        then SLog.debug (fun m -> m "Model: %s" (Z3.Model.to_string model))
        else () ;
        let c_stk_mach = Solver.SuduZ3.(get_unbox_fun_exn model top_stack) in
        let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
        Some { model; c_stk }
    | Result.Error _exps -> None
  in

  (* try another solver using accumulated phis *)
  let another_solver = Z3.Solver.mk_solver Solver.ctx None in
  let another_result =
    Solver.check another_solver state.phis_added phi_used_once
  in
  (match (solver_result, another_result) with
  | Result.Ok _, Result.Ok _ | Result.Error _, Result.Error _ -> ()
  | _, _ -> assert false) ;
  check_result

let simplify_phis () = ()

let try_step_check ~(config : Global_config.t) ~(state : Global_state.t) key
    stride =
  (* state.tree_size <- state.tree_size + 1 ; *)
  if state.tree_size mod !stride = 0
  then (
    simplify_phis () ;
    let t_start = Time_ns.now () in
    let check_result = check state config in
    let t_span = Time_ns.(diff (now ()) t_start) in
    Observe.count_smt_request config state key true Time_ns.Span.(to_sec t_span) ;

    match check_result with
    | Some { model; c_stk } ->
        (* Fmt.pr "Check this\n" ; *)
        Lwt.fail (Found_solution { model; c_stk })
    | None ->
        if !stride < config.stride_max
        then (
          stride := !stride * 2 ;
          if !stride > config.stride_max
          then stride := config.stride_max
          else ())
        else () ;
        Lwt.return_unit)
  else (
    Observe.count_smt_request config state key false 0.0 ;
    Lwt.return_unit)

let check_phis solver phis is_debug : result_info option =
  if is_debug
  then
    SLog.debug (fun m ->
        m "Phis: %a" Fmt.(Dump.list string) (List.map ~f:Z3.Expr.to_string phis))
  else () ;

  match Solver.check solver [] phis with
  | Result.Ok model ->
      SLog.app (fun m -> m "SAT") ;
      if is_debug
      then (
        SLog.debug (fun m ->
            m "Phis: %a"
              Fmt.(Dump.list string)
              (List.map ~f:Z3.Expr.to_string phis)) ;
        SLog.debug (fun m -> m "Model: %s" (Z3.Model.to_string model)))
      else () ;
      let c_stk_mach = Solver.SuduZ3.(get_unbox_fun_exn model top_stack) in
      let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
      print_endline @@ Concrete_stack.show c_stk ;
      Some { model; c_stk }
  | Result.Error _exps ->
      SLog.app (fun m -> m "UNSAT") ;
      None
