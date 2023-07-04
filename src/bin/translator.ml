open Jayil
open Ast_pp
open Translator_options
open Dj_common

let main () : unit =
  let options = parse_args () in
  let do_instrument = options.ta_instrument in
  match options.ta_mode with
  | Bluejay_to_jayil ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.bluejay_to_jayil ~do_wrap:false ~do_instrument
      |> fun (e, _, _, _) -> e |> Fmt.pr "%a" Jayil.Pp.expr
  | Bluejay_to_jay ->
      File_utils.parse_bluejay_stdin ()
      |> Convert.raw_bluejay_to_jay ~do_wrap:false
      |> fst |> Jay.Jay_ast_pp.show_expr_desc |> print_endline
  | Jay_to_jayil ->
      File_utils.parse_jay_stdin () |> Convert.raw_jay_to_jayil ~do_instrument
      |> fun (e, _, _) -> e |> Fmt.pr "%a" Jayil.Pp.expr
  | Scheme_to_jay -> failwith "scheme-to-jay"

let () = main ()
