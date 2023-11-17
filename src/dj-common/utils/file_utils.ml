open! Core

type lang = Jayil | Jay | Bluejay [@@deriving show]

let jayil_exts = [ "jil"; "odefa" ]
let jay_exts = [ "jay"; "natodefa" ]
let bluejay_exts = [ "bjy"; "tnat" ]
let check_jayil_ext s = List.exists jayil_exts ~f:(Filename.check_suffix s)
let check_jay_ext s = List.exists jay_exts ~f:(Filename.check_suffix s)
let check_bluejay_ext s = List.exists bluejay_exts ~f:(Filename.check_suffix s)

let lang_from_file s =
  if check_bluejay_ext s
  then Bluejay
  else if check_jay_ext s
  then Jay
  else if check_jayil_ext s
  then Jayil
  else failwith "file extension must be .jil, .jay, or .bjy"

let check_upto_jay s =
  List.exists (jayil_exts @ jay_exts) ~f:(Filename.check_suffix s)

let check_upto_bluejay s =
  List.exists
    (jayil_exts @ jay_exts @ bluejay_exts)
    ~f:(Filename.check_suffix s)

let parse_jay = Jay.Parse.parse_program
let parse_jayil = Jayil_parser.Parse.parse_program
let parse_bluejay = Bluejay.Bluejay_parse.parse_program
let parse_jay_stdin () = parse_jay In_channel.stdin
let parse_jayil_stdin () = parse_jayil In_channel.stdin
let parse_bluejay_stdin () = parse_bluejay In_channel.stdin
let parse_jay_file filename = In_channel.with_file filename ~f:parse_jay
let parse_jayil_file filename = In_channel.with_file filename ~f:parse_jayil

let parse_bluejay_file filename =
  In_channel.with_file filename ~f:Bluejay.Bluejay_parse.parse_program

let print_delimiter_line content =
  print_endline "*************************************" ;
  print_endline content

let read_source_full ?(do_wrap = false) ?(do_instrument = false) ?(consts = [])
    filename =
  let convert_result =
    match lang_from_file filename with
    | Bluejay ->
        parse_bluejay_file filename
        |> Convert.bluejay_to_jayil ~do_wrap ~do_instrument
    | Jay ->
        parse_jay_file filename |> Convert.jay_to_jayil ~do_instrument ~consts
    | Jayil ->
        parse_jayil_file filename |> Convert.instrument_jayil ~do_instrument
  in
  let jayil_ast = Convert.jil_ast_of_convert convert_result in
  Jayil.Ast_wellformedness.check_wellformed_or_exit jayil_ast ;
  convert_result

let read_source ?(do_wrap = false) ?(do_instrument = false) ?(consts = [])
    filename =
  read_source_full ~do_wrap ~do_instrument ~consts filename
  |> Convert.jil_ast_of_convert

let dump_to_file jil_ast filename =
  let oc = Out_channel.create filename in
  let formatter = Format.formatter_of_out_channel oc in
  Fmt.pf formatter "%a" Jayil.Pp.expr jil_ast ;
  Out_channel.close oc

let load_expect testpath t_of_sexp =
  let expect_path = Filename.chop_extension testpath ^ ".expect.s" in
  if Sys_unix.is_file_exn expect_path
  then Some (Sexp.load_sexp_conv_exn expect_path t_of_sexp)
  else None

let load_expect_d testpath = load_expect testpath Test_expect.t_of_sexp
let load_expect_s testpath = load_expect testpath Test_expect_sato.t_of_sexp
