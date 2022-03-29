open Core

type command = No_op | Pause | Resume

let mutex = Lwt_mutex.create ()
let user_commands, user_push = Lwt_stream.create ()
let handle_command _c = ()

let handle_available_commands () =
  let commands = Lwt_stream.get_available user_commands in
  List.iter commands ~f:handle_command

let () = user_push (Some 1)
