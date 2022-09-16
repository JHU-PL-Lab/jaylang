open Core
open Lwt.Infix

let main_details ~config program =
  let state = Global_state.create config program in
  let%lwt inputs, is_timeout, _model =
    Main.main_with_state_lwt ~config ~state
  in
  Lwt.return (inputs, is_timeout, state)

let search_input ~config program =
  let%lwt a, b, _c = main_details ~config program in
  Lwt.return (a, b)

let check_input ~(config : Global_config.t) program inputs =
  let mode = Global_config.Dbmc_check inputs in
  let config = { config with mode } in
  let%lwt _a, b, _c = main_details ~config program in
  Lwt.return b
