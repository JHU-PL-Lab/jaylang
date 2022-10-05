open Core
open Dj_common
open Log.Export

let update_rstk_pick (config : Global_config.t) (state : Global_state.t) model =
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.term_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
      else ())

let process_rstk_detail_map (config : Global_config.t) (state : Global_state.t)
    =
  let srd_list =
    state.rstk_detail_map |> Hashtbl.to_alist
    |> List.filter ~f:(function k, d -> d.smt_checks > 0)
    |> List.sort ~compare:(fun (s1, _) (s2, _) ->
           Int.compare (Rstack_intf.length s1) (Rstack_intf.length s2))
  in

  S2Log.info (fun m ->
      m "%a"
        Fmt.(vbox (list ~sep:cut (pair Rstack.pp_length Rstk_detail.pp)))
        srd_list)

let count_smt_request (config : Global_config.t) (state : Global_state.t)
    (key : Lookup_key.t) is_checked smt_time =
  Hashtbl.update state.rstk_detail_map key.r_stk ~f:(function
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
        })
