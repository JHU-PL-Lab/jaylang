open! Core

let is_natodefa_ext s = Filename.check_suffix s "natodefa"
(* String.is_suffix s ~suffix:"natodefa" *)

let read_source filename =
  let program =
    if is_natodefa_ext filename
    then
      let natast =
        In_channel.with_file filename ~f:Jay.On_parse.parse_program_raw
      in
      Jay.Jay_to_jayil.translate (Jay.On_ast.new_expr_desc natast) |> fst
    else if is_odefa_ext filename
    then In_channel.with_file filename ~f:Jayil_parser.Parse.parse_program_raw
    else failwith "file extension must be .natodefa"
  in
  ignore @@ Global_config.check_wellformed_or_exit program ;
  program

(*
let parse_natodefa = Jay.On_parse.parse_string
let parse_odefa = Jayil_parser.Parser.parse_string
let read_lines file = file |> In_channel.create |> In_channel.input_lines

let read_src file =
  file |> read_lines |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> not String.(prefix line 1 = "#"))
  |> String.concat ~sep:"\n"
*)

(*
let src_text = read_src testname in
let src =
  if Dbmc.File_util.is_natodefa_ext testname
  then parse_natodefa src_text
  else parse_odefa src_text
     in *)