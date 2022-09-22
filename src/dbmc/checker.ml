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

  let check_result = Solver.check state.phis phi_used_once in
  Global_state.clear_phis state ;
  SLog.debug (fun m -> m "Eager check") ;
  SLog.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ())) ;
  SLog.debug (fun m ->
      m "Used-once Phis (eager): %a"
        Fmt.(Dump.list string)
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
  let check_result = Solver.check state.phis phi_used_once in
  Global_state.clear_phis state ;

  if config.debug_model && verbose
  then (
    SLog.debug (fun m -> m "Solver Phis: %s" (Solver.string_of_solver ())) ;
    SLog.debug (fun m ->
        m "Used-once Phis: %a"
          Fmt.(Dump.list string)
          (List.map ~f:Z3.Expr.to_string phi_used_once)))
  else () ;

  match check_result with
  | Result.Ok model ->
      if config.debug_model && verbose
      then SLog.debug (fun m -> m "Model: %s" (Z3.Model.to_string model))
      else () ;
      let c_stk_mach = Solver.SuduZ3.(get_unbox_fun_exn model top_stack) in
      let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
      Some { model; c_stk }
  | Result.Error _exps -> None

let step_check ~(config : Global_config.t) ~(state : Global_state.t) stride =
  (* state.tree_size <- state.tree_size + 1 ; *)
  if state.tree_size mod !stride = 0
  then (
    (* LLog.app (fun m ->
        m "Step %d\t%a\n" state.tree_size Lookup_key.pp this_key) ; *)
    match check state config with
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
  else Lwt.return_unit

let check_phis phis is_debug : result_info option =
  if is_debug
  then
    SLog.debug (fun m ->
        m "Phis: %a" Fmt.(Dump.list string) (List.map ~f:Z3.Expr.to_string phis))
  else () ;

  match Solver.check [] phis with
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
