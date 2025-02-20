(**
  Bin [interp].

  This executable runs the interpreter on the program at the
  given file path. The file must be a Bluejay (`.bjy`) file,
  and the mode (`-m`) argument allows the user to translate the
  programs to other languages before interpreting.
*)

open Core
open Lang

let usage_msg =
  {|
  interp <file> -m <mode> [-w yes/no]
  |}
  (* would like to add [<input_i>] optional inputs *)

let source_file = ref "" 
let mode = ref ""
let wrap = ref "yes"

let read_anon_arg src_file_raw =
  source_file := src_file_raw

let speclist = 
  [ ("-m", Arg.Set_string mode, "Mode: bluejay, desugared, or embedded.")
  ; ("-w", Arg.Set_string wrap, "Wrap flag: yes or no. Default is yes.")]

let () = 
  Arg.parse speclist read_anon_arg usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    let pgm =
      Lang.Parse.parse_single_pgm_string
      @@ In_channel.read_all src_file
    in
    match !mode with
    | "bluejay" -> let _ = Interp.eval_pgm pgm in ()
    | "desugared" -> let _ = Interp.eval_pgm @@ Translate.Convert.bjy_to_des pgm in ()
    | "embedded" ->
      let do_wrap =
        match String.lowercase !wrap with
        | "yes" | "y" -> true
        | "no" | "n" -> false
        | _ -> Format.eprintf "Error: bad string given to wrap -w flag. Should be yes/no."; assert false
      in
      let _ = Interp.eval_pgm @@ Translate.Convert.bjy_to_emb pgm ~do_wrap in
      ()
    | _ -> Format.eprintf "Error: mode should be one of bluejay, desugared, embedded."; assert false
  end
