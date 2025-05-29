(**
  Bin [analysis].

  This executable runs the program analysis on a Bluejay
  program at the given file path.
*)

open Core

let usage_msg =
  {|
  analysis <file> [-w yes/no] [-s]
  |}

let source_file = ref "" 
let wrap = ref "yes"
let type_splay = ref false

let read_anon_arg src_file_raw =
  source_file := src_file_raw

let speclist = 
  (* optional args for evaluation. The record fields get set by arguments *)
  [ ("-w", Arg.Set_string wrap, "Wrap flag: yes or no. Default is yes.")
  ; ("-s", Arg.Set type_splay, "Splay types on recursive functions")
  ]

let () = 
  Arg.parse speclist read_anon_arg usage_msg;
  match !source_file with
  | "" -> ()
  | src_file ->
    let do_wrap =
      match String.lowercase !wrap with
      | "yes" | "y" -> true
      | "no" | "n" -> false
      | _ -> Format.eprintf "Error: bad string given to wrap -w flag. Should be yes/no."; assert false
    in
    (* if *)
      In_channel.read_all src_file
      |> Lang.Parse.parse_single_pgm_string
      |> Translate.Convert.bjy_to_emb ~do_wrap ~do_type_splay:!type_splay
      |> Lang.Ast_tools.Utils.pgm_to_module
      |> Lang.Ast.Embedded.With_callsites.alphatized_of_expr
      |> Analysis.Main.analyze
      |> Analysis.Grammar.M.run_for_error
      |> function
        | Ok _value_set -> Format.printf "Your program is error-free\n"
        | Error err -> Format.printf "ERROR: the analysis found an error.\n%s\n" (Analysis.Grammar.Err.to_string err)
