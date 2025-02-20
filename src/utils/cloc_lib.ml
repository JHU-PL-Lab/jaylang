(**
  Module [Cloc_lib].

  Count lines of code in Bluejay files (note this is
  outdated now and doesn't ignored OCaml-like comments).
*)

open Core

let count_bjy_lines filename =
  In_channel.with_file filename ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun count line ->
          (* Ignore empty lines and comment lines *)
          if String.is_empty (String.strip line) || String.is_prefix line ~prefix:"#" then
            count
          else
            count + 1))