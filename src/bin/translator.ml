open Translator_options
open Dj_common

let main () : unit =
  let options = parse_args () in
  let do_instrument = options.ta_instrument in
  let do_wrap = options.ta_wrap in
  match options.ta_mode with
  | Bluejay_to_jayil ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.bluejay_to_jayil ~do_wrap ~do_instrument
      |> Convert.jil_ast_of_convert
      |> Jayil.Ast_tools.purge
      |> Jayil.Pp.print_expr
  | Bluejay_to_jay ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.bluejay_to_jay ~do_wrap
      |> Jay.Pp.print_expr
  | Jay_to_jayil ->
      File_utils.parse_jay_stdin ()
      |> Convert.jay_to_jayil ~do_instrument
      |> Convert.jil_ast_of_convert |> Jayil.Pp.print_expr
  | Scheme_to_jay -> failwith "scheme-to-jay"

let () = main ()
