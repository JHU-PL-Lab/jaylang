open Jayil
open Ast_pp
open Translator_options
open Dj_common

let main () : unit =
  let options = parse_args () in
  let is_instrumented = options.ta_instrument in
  match options.ta_mode with
  | Bluejay_to_jayil ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.raw_bluejay_to_jayil ~is_instrumented
      |> Fmt.pr "%a" Jayil.Pp.expr
  | Bluejay_to_jay ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.raw_bluejay_to_jay |> Jay.Jay_ast_pp.show_expr_desc
      |> print_endline
  | Jay_to_jayil ->
      File_utils.parse_jay_stdin ()
      |> Convert.raw_jay_to_jayil ~is_instrumented
      |> Fmt.pr "%a" Jayil.Pp.expr
  | Scheme_to_jay -> failwith "scheme-to-jay"

let () = main ()
