open Core
open Dbmc

let main_commandline () =
  let cfg = Argparse.parse_commandline_config () in
  Log.init cfg ;
  let (program, _, _) = Sato.File_util.read_source_sato cfg.filename in

  (try
     (* Calling dbmc here *)
     let inputss = Main.main ~config:cfg program in
     match List.hd inputss with
     | Some inputs ->
         Format.printf "[%s]\n"
           (String.concat ~sep:","
           @@ List.map
                ~f:(function Some i -> string_of_int i | None -> "-")
                inputs)
     | None -> Format.printf "Unreachable"
   with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
              raise ex) ;

  Log.close ()

let () = main_commandline ()