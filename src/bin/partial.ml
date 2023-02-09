open Core

let usage_msg = "partial -i <file>"
let source_file = ref ""
let anon_fun _ = ()

let run_program source =
  ignore @@ Partial_evaluator.eval_file source ;
  ()

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Input source file") ]
    anon_fun usage_msg ;
  run_program !source_file
