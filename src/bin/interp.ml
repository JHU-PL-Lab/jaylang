
open Core
open Lang

let usage_msg = "interp <file> -m <mode> [<input_i>]"

let () =
  match Sys.get_argv () |> List.of_array with
  | _ :: source_file :: "-m" :: mode :: inputs -> begin
    if not @@ List.is_empty inputs
    then failwith "Gave inputs to interpreter, which is currently not handled.";
    let pgm =
      Lang.Parse.parse_single_expr_string
      @@ In_channel.read_all source_file
    in
    match mode with
    | "bluejay" -> let _ = Interp.eval_exp pgm in ()
    | "desugared" -> let _ = Interp.eval_exp @@ Translate.Convert.bjy_to_des pgm in ()
    | "embedded" -> let _ = Interp.eval_exp @@ Translate.Convert.bjy_to_emb pgm in ()
    | _ -> Format.eprintf "Error: mode should be one of bluejay, desugared, embedded."; assert false
  end
  | _ -> Format.printf "error in parsing. Usage message: %s\n" usage_msg
