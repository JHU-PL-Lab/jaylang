open Core

let count_non_comment_lines filename =
  In_channel.with_file filename ~f:(fun file ->
      In_channel.fold_lines file ~init:0 ~f:(fun count line ->
          if String.is_empty (String.strip line) || String.is_prefix line ~prefix:"#" then
            count
          else
            count + 1))

let () =
  let filename = (Sys.get_argv ()).(1) in
  let count = count_non_comment_lines filename in
  Format.printf "%d\n" count
