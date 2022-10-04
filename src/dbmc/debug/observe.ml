open Core
open Dj_common

let update_rstk_pick (config : Global_config.t) (state : Global_state.t) model =
  Hashtbl.clear state.rstk_picked ;
  Hashtbl.iter_keys state.term_detail_map ~f:(fun key ->
      if Riddler.is_picked (Some model) key
      then ignore @@ Hashtbl.add state.rstk_picked ~key:key.r_stk ~data:true
      else ())
