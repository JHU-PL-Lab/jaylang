(**
  Bin [bjy_cloc].

  This executable counts the lines of code in Bluejay files.
  Note that it is currently out of date because it does not
  handle OCaml-like comments.
*)

open Core

let () =
  let filename = (Sys.get_argv ()).(1) in
  if Filename.check_suffix filename ".bjy"
  then Format.printf "%d\n" (Utils.Cloc_lib.count_bjy_lines filename)
  else Format.eprintf "Expected bjy file. Got %s" filename