open Core
open Dj_common
open Log.Export

let update_rstk_pick (config : Global_config.t) (state : Global_state.t) model =
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.lookup_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true)

let process_rstk_stat_map (config : Global_config.t) (state : Global_state.t) =
  let raw_plist = state.rstk_stat_map |> Hashtbl.to_alist in
  let srd_list =
    raw_plist
    |> List.filter ~f:(function k, d -> d.smt_checks > 0)
    |> List.sort ~compare:(fun (s1, _) (s2, _) ->
           Int.compare (Rstack_intf.length s1) (Rstack_intf.length s2))
  in
  let time_sum =
    List.sum (module Float) raw_plist ~f:(fun (k, d) -> d.smt_time)
  in

  LS2Log.info (fun m ->
      m "%a@;%f"
        Fmt.(vbox (list ~sep:cut (pair Rstack.pp_length Rstk_stat.pp)))
        srd_list time_sum)

let dump_block_stat (config : Global_config.t) (state : Global_state.t) =
  if state.tree_size mod 1000 = 0
  then
    let raw_plist = state.block_stat_map |> Hashtbl.to_alist in
    LS2Log.debug (fun m ->
        m "%d@,%a" state.tree_size
          Fmt.(vbox (list ~sep:sp (Dump.pair Cfg.Block.pp Block_stat.pp)))
          raw_plist)

let update_block_visits (config : Global_config.t) (state : Global_state.t)
    (key : Lookup_key.t) is_checked smt_time =
  Hashtbl.update state.rstk_stat_map key.r_stk ~f:(function
    | None ->
        {
          block_id = key.block.id;
          visits = 1;
          smt_checks = (if is_checked then 1 else 0);
          smt_size = Solver.get_assertion_count state.solver;
          smt_time;
        }
    | Some d ->
        {
          d with
          visits = d.visits + 1;
          smt_checks = (if is_checked then d.smt_checks + 1 else d.smt_checks);
          smt_size = max d.smt_size (Solver.get_assertion_count state.solver);
          smt_time = d.smt_time +. smt_time;
        }) ;
  let block = key.block.id in
  Hashtbl.update state.block_stat_map key.block ~f:(function
    | None -> { visits = 1 }
    | Some d -> { visits = d.visits + 1 }) ;
  ()

let get_block_visits (state : Global_state.t) (key : Lookup_key.t) =
  Hashtbl.find_and_call state.block_stat_map key.block
    ~if_found:(fun d -> d.visits)
    ~if_not_found:(fun _ -> 0)

let pp_one_sub map oc (pre, key) =
  match Hashtbl.find map key with
  | Some (detail : Lookup_detail.t) ->
      Fmt.pf oc "%a=%a" Lookup_key.pp key Lookup_status.pp_short detail.status
  | None -> Fmt.pf oc "%a=/" Lookup_key.pp key

let pp_subs map oc (detail : Lookup_detail.t) =
  Fmt.(pr "%a" (hbox @@ list ~sep:semi (pp_one_sub map))) detail.sub_lookups

let pp_key_with_detail map oc ((key, detail) : Lookup_key.t * Lookup_detail.t) =
  Fmt.(
    pf oc "%a[%a]: {%d}, {%d}: %a" Lookup_key.pp key Lookup_status.pp_short
      detail.status
      (List.length detail.sub_preconds)
      (List.length detail.sub_lookups)
      (pp_subs map) detail)

let get_dbmc_unroll (state : Global_state.t) =
  match state.unroll with S_dbmc unroll -> unroll | _ -> failwith "unroll"

let dump_lookup_details (state : Global_state.t) =
  let td_lst = Global_state.detail_alist state in

  CMLog.debug (fun m -> m "@.[Size: %d]@." (List.length td_lst)) ;
  let unroll = get_dbmc_unroll state in
  List.iter td_lst ~f:(fun (key, td) ->
      match td.status with
      (* | Complete | Fail -> () *)
      | _ ->
          CMLog.app (fun m ->
              m "%a"
                (Fmt.vbox @@ pp_key_with_detail state.lookup_detail_map)
                (key, td)) ;
          let msg_s = Unrolls.U_dbmc.get_stream unroll key in
          let msg_lst = Lwt_stream.get_available msg_s in
          CMLog.app (fun m ->
              m "--v[%d]: %a" (List.length td.domain)
                (Fmt.Dump.list Lookup_key.pp)
                td.domain) ;
          CMLog.app (fun m ->
              m "--m[%d]: %a@." (List.length msg_lst)
                (Fmt.Dump.list Lookup_result.pp)
                msg_lst))

let dump_check_info filename (state : Global_state.t) =
  List.iter state.check_infos ~f:(fun entry ->
      PLog.debug (fun m ->
          m "%s,%d,%d" filename entry.total_phis entry.solver_resource))
