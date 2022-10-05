open Core
open Dj_common
open Log.Export

let update_rstk_pick (config : Global_config.t) (state : Global_state.t) model =
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.term_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
      else ())

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

  S2Log.info (fun m ->
      m "%a@;%f"
        Fmt.(vbox (list ~sep:cut (pair Rstack.pp_length Rstk_stat.pp)))
        srd_list time_sum)

let dump_block_stat (config : Global_config.t) (state : Global_state.t) =
  if true (* state.tree_size mod 100 = 0 *)
  then
    let raw_plist = state.block_stat_map |> Hashtbl.to_alist in
    S2Log.app (fun m ->
        m "%d@,%a" state.tree_size
          Fmt.(vbox (list ~sep:sp (Dump.pair Cfg.Block.pp Block_stat.pp)))
          raw_plist)
    (* ;
       S2Log.app (fun m -> m "") *)
  else ()

let count_smt_request (config : Global_config.t) (state : Global_state.t)
    (key : Lookup_key.t) is_checked smt_time =
  Hashtbl.update state.rstk_stat_map key.r_stk ~f:(function
    | None ->
        {
          block_id = key.block.id;
          visits = 1;
          smt_checks = (if is_checked then 1 else 0);
          smt_size = Solver.get_assertion_count ();
          smt_time;
        }
    | Some d ->
        {
          d with
          visits = d.visits + 1;
          smt_checks = (if is_checked then d.smt_checks + 1 else d.smt_checks);
          smt_size = max d.smt_size (Solver.get_assertion_count ());
          smt_time = d.smt_time +. smt_time;
        }) ;
  let block = key.block.id in
  Hashtbl.update state.block_stat_map key.block ~f:(function
    | None -> { visits = 1 }
    | Some d -> { visits = d.visits + 1 }) ;
  ()
