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

(* for users *)
let read_source ?(is_instrumented = false) ?(consts = []) filename =
  let jayil_ast =
    if check_jay_ext filename
    then
      let jay_ast = parse_jay_file filename in
      Convert.jay_ast_to_jayil ~is_instrumented ~consts jay_ast
    else if check_jayil_ext filename
    then
      let jayal_ast = parse_jayil_file filename in
      Convert.instrument_jayil_if ~is_instrumented jayal_ast
    else failwith "file extension must be .jay or .jil"
  in
  Jayil.Ast_wellformedness.check_wellformed_or_exit jayil_ast ;
  jayil_ast
