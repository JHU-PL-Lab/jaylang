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

let read_source ?(do_wrap = false) ?(do_instrument = false) ?(consts = [])
    filename =
  let jayil_ast =
    if check_jay_ext filename
    then
      let jay_ast = parse_jay_file filename in
      Convert.jay_to_jayil ~do_instrument ~consts jay_ast
      |> Convert.jil_ast_of_convert
    else if check_jayil_ext filename
    then
      let jayil_ast = parse_jayil_file filename in
      if do_instrument
      then Jay_instrumentation.Instrumentation.instrument_jayil jayil_ast |> fst
      else jayil_ast
    else failwith "file extension must be .jay or .jil"
  in
  Jayil.Ast_wellformedness.check_wellformed_or_exit jayil_ast ;
  jayil_ast

let read_source_sato ?(do_wrap = false) ?(do_instrument = true) filename =
  (* let inst_jil, jil_inst_map, on_odefa_maps, bluejay_to_jay_map' = *)
  if check_bluejay_ext filename
  then
    let raw_bluejay = parse_bluejay_file filename in
    Convert.bluejay_to_jayil ~do_wrap ~do_instrument raw_bluejay
    |> Convert.convert_t_to4
  else if check_jay_ext filename
  then
    let raw_jay = parse_jay_file filename in
    Convert.jay_to_jayil ~do_instrument raw_jay |> Convert.convert_t_to4
  else if check_jayil_ext filename
  then
    let raw_jil = parse_jayil_file filename in
    let inst_jil, jil_inst_maps =
      if do_instrument
      then Jay_instrumentation.Instrumentation.instrument_jayil raw_jil
      else (raw_jil, Jay_instrumentation.Jayil_instrumentation_maps.empty false)
    in
    (inst_jil, jil_inst_maps, None, None)
  else failwith "file extension must be .jil, .jay, or .bjy"
