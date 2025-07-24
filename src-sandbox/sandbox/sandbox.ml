open Core

open Lang.Parse

open Lang.Ast.Expr

let () =
  let filename = (Sys.get_argv ()).(1) in
  let text = In_channel.input_all (In_channel.create filename) in
  let ast1 = parse_program (In_channel.create filename) in
  let pp_ast1 = String.concat ~sep:"\n\n" (List.map ast1 ~f:(statement_to_string)) in
  let ast2 = parse_single_pgm_string pp_ast1 in
  let pp_ast2 = String.concat ~sep:"\n\n" (List.map ast2 ~f:(statement_to_string)) in
  print_endline "Orginal:";
  print_endline text;
  print_endline "\n\nPass 1:";
  print_endline pp_ast1;
  print_endline "\n\nPass 2:";
  print_endline pp_ast2;
