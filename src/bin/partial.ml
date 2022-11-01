open Core

let usage_msg = "partial <file>"
let flag = ref false
let source_file = ref ""
let anon_fun source = source_file := source

let run_program source =
  ignore @@ Partial_evaluator.eval source ;
  ()

let () =
  Arg.parse
    [ ("-f", Arg.Bool (fun fl -> flag := fl), "Iutput source file") ]
    anon_fun usage_msg ;
  run_program !source_file
