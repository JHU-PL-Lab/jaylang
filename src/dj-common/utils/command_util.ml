open Core

let parse_command (all_params : 't Command.Param.t) summary (store : 't ref) :
    't =
  let save_param : (unit -> unit) Command.Param.t =
    Command.Param.(all_params >>| fun params () -> store := params)
  in
  let command = Command.basic ~summary save_param in
  Command_unix.run command ;
  !store
