open! Core

let is_odefa_ext s = Filename.check_suffix s "odefa"
(* || Filename.check_suffix filename "natodefa"  *)
(* String.is_suffix s ~suffix:"odefa" *)

let is_natodefa_ext s = Filename.check_suffix s "natodefa"
(* String.is_suffix s ~suffix:"natodefa" *)

let read_source ?(is_instrumented = false) filename =
  let program =
    if is_natodefa_ext filename
    then
      failwith "TBI!"
      (* let natast =
        In_channel.with_file filename
          ~f:Odefa_natural.On_parse.parse_program_raw
      in
      let on_expr, ton_on_maps =
        Odefa_natural.Ton_to_on.transform_natodefa natast
      in
      Odefa_natural.On_to_odefa.translate ~is_instrumented ton_on_maps on_expr
      |> fst *)
    else if is_odefa_ext filename
    then
      let ast =
        In_channel.with_file filename ~f:Odefa_parser.Parse.parse_program_raw
      in
      if is_instrumented
      then Odefa_natural.Odefa_instrumentation.instrument_odefa ast |> fst
      else ast
    else failwith "file extension must be .odefa or .natodefa"
  in
  ignore @@ Global_config.check_wellformed_or_exit program ;
  program

(*
let parse_natodefa = Odefa_natural.On_parse.parse_string
let parse_odefa = Odefa_parser.Parser.parse_string
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