open Core
open Dj_common
open Riddler
open SuduZ3
open Log.Export

let close_smt_list smt_list =
  Hashtbl.to_alist smt_list
  |> List.map ~f:(fun (key, i) -> SuduZ3.not_ (pick_key_list key i))

let lead_smt_list smt_list target =
  Hashtbl.to_alist smt_list
  |> List.map ~f:(fun (_key, _i) ->
         picked target (* SuduZ3.not_ (pick_key_list key i) *))

let close_unfinished_lookups lookups =
  lookups |> List.map ~f:(fun key -> Solver.SuduZ3.not_ (picked key))

let lead_unfinished_lookups lookups target =
  lookups
  (* |> List.map ~f:(fun key ->
         if not (Lookup_key.equal key target)
         then Solver.SuduZ3.not_ (Riddler.picked key)
         else Riddler.picked key) *)
  |> List.map ~f:(fun key -> picked key @=> picked target)

let eager_phi_fix (state : Global_state.t) c =
  let unfinish_lookup =
    lead_unfinished_lookups (Hash_set.to_list state.lookup_created) c
  in
  let list_fix = lead_smt_list state.smt_lists c in
  unfinish_lookup @ [ picked c ] @ list_fix

let phi_fix (state : Global_state.t) =
  let unfinish_lookup =
    close_unfinished_lookups (Hash_set.to_list state.lookup_created)
  in
  let list_fix = close_smt_list state.smt_lists in
  unfinish_lookup @ [ picked state.key_target ] @ list_fix

let eager_check (state : Global_state.t) (config : Global_config.t) c assumption
    =
  SLog.debug (fun m -> m "Eager check") ;
  let phi_used_once = eager_phi_fix state c @ assumption in
  let solver_result =
    Solver.check state.solver state.phis_staging phi_used_once
  in
  Global_state.clear_phis state ;
  match solver_result with
  | Result.Ok _model -> true
  | Result.Error _exps -> false

let check (state : Global_state.t) (config : Global_config.t) :
    result_info option =
  LLog.info (fun m -> m "Search Tree Size:\t%d" state.tree_size) ;
  let phi_used_once = phi_fix state in
  let solver_result =
    Solver.check ~verbose:config.debug_model state.solver state.phis_staging
      phi_used_once
  in
  Global_state.clear_phis state ;

  let check_result =
    match solver_result with
    | Result.Ok model ->
        if config.debug_model
        then SLog.debug (fun m -> m "Model: %s" (Z3.Model.to_string model)) ;
        let c_stk_mach = Solver.SuduZ3.(get_unbox_fun_exn model top_stack) in
        let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
        Some { model; c_stk }
    | Result.Error _exps -> None
  in
  (let this_check_info : Check_info.t =
     {
       total_phis = List.length state.phis_added + List.length phi_used_once;
       solver_resource = Solver.get_rlimit state.solver;
     }
   in
   state.check_infos <- this_check_info :: state.check_infos) ;

  (*
      double check - start
  *)
  let detail_lst = Global_state.detail_alist state in

  SLog.debug (fun m -> m "One: %s" (Z3.Solver.to_string state.solver)) ;

  Solver.reset state.solver ;

  SLog.debug (fun m -> m "Two") ;
  List.iter detail_lst ~f:(fun (key, detail) ->
      let phis =
        if Lookup_status.equal detail.status detail.status_gen_phi
        then detail.phis
        else
          let phis' =
            match detail.status with
            | Complete -> Lookup_rule.complete_phis_of_rule state key detail
            | Fail -> invalid key
            | Good -> failwith "Good in re-gen phis"
          in
          detail.status_gen_phi <- detail.status ;
          detail.phis <- [ phis' ] ;
          [ phis' ]
      in
      let phis_all = phis @ detail.phis_external in
      SLog.debug (fun m ->
          m "%a @@ %a's phis = \n@[<v>%a@]" Lookup_key.pp key
            Lookup_status.pp_short detail.status
            Fmt.(list ~sep:cut string)
            (List.map ~f:Z3.Expr.to_string phis_all)) ;
      state.phis_staging <- phis_all @ state.phis_staging) ;
  let another_result =
    Solver.check ~verbose:config.debug_model state.solver state.phis_staging
      phi_used_once
  in

  SLog.debug (fun m ->
      m "Two (used once): %a"
        Fmt.(Dump.list string)
        (List.map ~f:Z3.Expr.to_string phi_used_once)) ;

  Global_state.clear_phis state ;
  (match (solver_result, another_result) with
  | Result.Ok _, Result.Ok _ | Result.Error _, Result.Error _ -> ()
  | Result.Ok _, Result.Error _ -> failwith "should SAT"
  | Result.Error _, Result.Ok _ -> failwith "should UNSAT") ;

  (*
      double check - end
  *)
  check_result

let simplify_phis () = ()

let try_step_check ~(config : Global_config.t) ~(state : Global_state.t) key
    stride =
  if state.tree_size mod !stride = 0
  then (
    simplify_phis () ;
    let t_start = Time_ns.now () in
    let check_result = check state config in
    let t_span = Time_ns.(diff (now ()) t_start) in
    Observe.count_smt_request config state key true Time_ns.Span.(to_sec t_span) ;

    match check_result with
    | Some { model; c_stk } -> Lwt.fail (Found_solution { model; c_stk })
    | None ->
        if !stride < config.stride_max
        then (
          stride := !stride * 2 ;
          if !stride > config.stride_max then stride := config.stride_max) ;
        Lwt.return_unit)
  else (
    Observe.count_smt_request config state key false 0.0 ;
    Lwt.return_unit)

(* let step_eager_check (state : Global_state.t) (config : Global_config.t) target
     assumption stride =
   (* state.tree_size <- state.tree_size + 1 ; *)
   if state.tree_size mod !stride = 0
   then (
     if !stride < config.stride_max
     then (
       stride := !stride * 2 ;
       if !stride > config.stride_max then stride := config.stride_max )
     ;
     eager_check state config target assumption)
   else true *)

(* `check_phis` are used in ddse and dbmc-debug *)
let check_phis solver phis is_debug : result_info option =
  match Solver.check solver [] phis with
  | Result.Ok model ->
      SLog.app (fun m -> m "SAT") ;
      if is_debug
      then (
        SLog.debug (fun m ->
            m "Phis: %a"
              Fmt.(Dump.list string)
              (List.map ~f:Z3.Expr.to_string phis)) ;
        SLog.debug (fun m -> m "Model: %s" (Z3.Model.to_string model))) ;
      let c_stk_mach = Solver.SuduZ3.(get_unbox_fun_exn model top_stack) in
      let c_stk = c_stk_mach |> Sexp.of_string |> Concrete_stack.t_of_sexp in
      print_endline @@ Concrete_stack.show c_stk ;
      Some { model; c_stk }
  | Result.Error _exps ->
      SLog.app (fun m -> m "UNSAT") ;
      None
