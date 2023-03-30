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

(* Completion *)
let complete_phis_of_rule (state : Global_state.t) key
    (detail : Lookup_detail.t) =
  let open Rule in
  let open Riddler in
  let key_first = Lookup_key.to_first key state.first in
  match detail.rule with
  (* Bounded (same as complete phi) *)
  | Discovery_main p -> discover_main_with_picked key (Some p.v)
  | Discovery_nonmain p -> discover_non_main key key_first (Some p.v)
  | Assume p -> mismatch_with_picked key
  | Assert p -> mismatch_with_picked key
  | Mismatch -> mismatch_with_picked key
  | Abort p ->
      if p.is_target
      then discover_non_main key key_first None
      else mismatch_with_picked key
  | Alias p -> eq_with_picked key p.x'
  | Input p ->
      if p.is_in_main
      then discover_main_with_picked key None
      else discover_non_main key key_first None
  | Not p -> not_with_picked key p.x'
  | Binop p -> binop_with_picked key p.bop p.x1 p.x2
  (*
      Unbounded
  *)
  | Record_start p ->
      picked key @=> and_ (picked p.r :: eq_one_picked_of key detail.domain)
  | Cond_top p ->
      picked key
      @=> and_
            ([ picked p.x; picked p.x2; eq key p.x ]
            (* before the `@` is the original constraits
               after the `@` is the added ones *)
            @ eq_one_picked_of key detail.domain)
  | Cond_btm p -> cond_bottom key p.x' p.cond_both
  | Fun_enter_local p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map p.callsites ~f:(fun callsite ->
            let callsite_block, x', x'', x''' =
              Cfg.fun_info_of_callsite callsite state.block_map
            in
            let callsite_stack =
              Option.value_exn (Rstack.pop key.r_stk (x', fid))
            in
            let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
            let detail_f = Hashtbl.find_exn state.lookup_detail_map key_f in
            let domain_f = detail_f.domain in
            let key_arg = Lookup_key.of3 x''' callsite_stack callsite_block in
            let detail_arg = Hashtbl.find_exn state.lookup_detail_map key_arg in
            let domain_arg = detail_arg.domain in
            and_
              [
                eq key key_arg;
                picked_eq_choices key_f domain_f;
                picked_eq_choices key_arg domain_arg;
              ])
      in
      (* and_
         [
           Riddler.fun_enter_local key key.block.id p.callsites state.block_map;
           ;
         ] *)
      picked key @=> or_ phi_f_and_arg
  | Fun_enter_nonlocal p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map detail.sub_lookups ~f:(fun ((key_f, key_fv), key_arg) ->
            let detail_arg = Hashtbl.find_exn state.lookup_detail_map key_arg in
            and_
              [
                picked key_f;
                picked key_fv;
                eq key_f key_fv;
                picked_eq_choices key_arg detail_arg.domain;
              ])
      in
      picked key @=> or_ phi_f_and_arg
      (* List.map p.callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite state.block_map
          in
          let callsite_stack =
            Option.value_exn (Rstack.pop key.r_stk (x', fid))
          in
          let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
          let detail_f = Hashtbl.find_exn state.lookup_detail_map key_f in
          or_
            (List.map detail_f.sub_lookups
               ~f:(fun ((key_f, key_fv), key_arg) ->
                 let detail_arg =
                   Hashtbl.find_exn state.lookup_detail_map key_arg
                 in
                 and_
                   [
                     picked key_f;
                     picked key_fv;
                     eq key_f key_fv;
                     picked_eq_choices key_arg detail_arg.domain;
                   ]))
          (*   let domain_f = detail_f.domain in *)
          (* let key_arg = Lookup_key.of3 x' callsite_stack callsite_block in *)
          (* let key_arg =
               List.find_exn
                 ~f:(fun (pre, _) -> Lookup_key.equal pre key_f)
                 detail_f.sub_lookups
               |> snd
             in
             let domain_arg = detail_arg.domain in
             and_
               [
                 eq key key_arg;
                 picked_eq_choices key_f domain_f;
                 picked_eq_choices key_arg domain_arg;
               ]) *)) *)

      (* and_
         [
           (* Riddler.fun_enter_local key key.block.id p.callsites state.block_map; *)

         ] *)
  | _ -> true_

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

  let detail_lst = Global_state.detail_alist state in

  (* debug *)
  SLog.debug (fun m -> m "One: %s" (Z3.Solver.to_string state.solver)) ;

  (* Solver.reset state.solver ; *)
  List.iter detail_lst ~f:(fun (key, detail) ->
      if Lookup_status.equal detail.status detail.status_gen_phi
      then state.phis_staging <- detail.phis @ state.phis_staging
      else
        let key_staging =
          match detail.status with
          | Complete -> complete_phis_of_rule state key detail
          | Fail -> mismatch_with_picked key
          | Good -> failwith "Good in re-gen phis"
        in
        detail.status_gen_phi <- detail.status ;
        state.phis_staging <- key_staging :: state.phis_staging ;
        detail.status_gen_phi <- detail.status) ;

  SLog.debug (fun m -> m "Two: %s" (Z3.Solver.to_string state.solver)) ;

  let another_result =
    Solver.check ~verbose:config.debug_model state.solver state.phis_staging
      phi_used_once
  in
  Global_state.clear_phis state ;
  (match (solver_result, another_result) with
  | Result.Ok _, Result.Ok _ | Result.Error _, Result.Error _ -> ()
  | Result.Ok _, Result.Error _ -> failwith "should SAT"
  | Result.Error _, Result.Ok _ -> failwith "should UNSAT") ;

  check_result
(* debug -
   try another solver using accumulated phis
   should be removed *)

(* ;
   let another_solver = Z3.Solver.mk_solver Solver.ctx None in
   let another_result =
     Solver.check another_solver state.phis_added phi_used_once
   in
   (match (solver_result, another_result) with
   | Result.Ok _, Result.Ok _ | Result.Error _, Result.Error _ -> ()
   | _, _ -> assert false) *)
(* debug - end *)
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
  if is_debug
  then
    SLog.debug (fun m ->
        m "Phis: %a" Fmt.(Dump.list string) (List.map ~f:Z3.Expr.to_string phis)) ;

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
