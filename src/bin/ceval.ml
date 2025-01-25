
open Core
open Lang

let usage_msg = "ceval <file>"

let () =
  match Sys.get_argv () |> List.of_array with
  | _ :: source_file :: [] -> let _ = New_concolic.Driver.test source_file in ()
  | _ -> Format.printf "error in parsing. Usage message: %s\n" usage_msg