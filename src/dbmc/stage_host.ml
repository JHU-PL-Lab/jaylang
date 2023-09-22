open Core
open Dj_common
open Stage

type stage_result =
  | Argparse of Global_config.t
  | Load_file of Jayil.Ast.expr
  | State_init of Global_state.t
  | Lookup of unit
  | Post_check of unit
  | All_done of unit

exception Stage_result of stage_result
