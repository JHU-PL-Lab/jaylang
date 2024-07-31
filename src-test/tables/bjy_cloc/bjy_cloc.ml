
open Core

let () =
  let filename = (Sys.get_argv ()).(1) in
  if Filename.check_suffix filename ".bjy"
  then Format.printf "%d\n" (Cloc_lib.count_bjy_lines filename)
  else Format.eprintf "Expected bjy file. Got %s" filename