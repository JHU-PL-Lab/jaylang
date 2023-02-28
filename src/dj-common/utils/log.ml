open Core

let src_lookup = Logs.Src.create "lookup"
let src_solver = Logs.Src.create "solver"
let src_interpreter = Logs.Src.create "interpreter"
let src_search = Logs.Src.create "search"

module Export = struct
  module LLog = (val Logs.src_log src_lookup : Logs.LOG)
  module SLog = (val Logs.src_log src_solver : Logs.LOG)
  module ILog = (val Logs.src_log src_interpreter : Logs.LOG)
  module S2Log = (val Logs.src_log src_search : Logs.LOG)

  (* Message tags are arbitrary named and typed values that can be associated to log messages.
     See https://erratique.ch/software/logs/doc/Logs/index.html#ex1 *)
  let rule_tag : _ Logs.Tag.def = Logs.Tag.def "Rule" ~doc:"rules" String.pp
end

let saved_oc = ref None
let filename_of_now () = Core.Time.(now () |> to_filename_string ~zone:Zone.utc)

let init (cfg : Global_config.t) =
  Logs.set_level cfg.log_level ;
  Logs.Src.set_level src_lookup cfg.log_level_lookup ;
  Logs.Src.set_level src_solver cfg.log_level_solver ;
  Logs.Src.set_level src_interpreter cfg.log_level_interpreter ;
  Logs.Src.set_level src_search cfg.log_level_search ;

  let levels =
    [
      cfg.log_level;
      cfg.log_level_lookup;
      cfg.log_level_solver;
      cfg.log_level_interpreter;
      cfg.log_level_search;
    ]
  in
  let enable_logging = List.exists levels ~f:Option.is_some in

  if enable_logging
  then (
    let suffix =
      String.Search_pattern.(
        replace_all (create "/") ~in_:cfg.filename ~with_:"_")
    in

    let log_file =
      Filename.of_parts [ "logs"; filename_of_now () ^ "_" ^ suffix ^ ".log" ]
    in
    let oc = Out_channel.create log_file in
    saved_oc := Some oc ;
    let fmter = Format.formatter_of_out_channel oc in
    let reporter =
      Logs.format_reporter ~pp_header:Logs.pp_header ~app:Format.std_formatter
        ~dst:fmter ()
    in
    Logs.set_reporter reporter)
  else
    (* Logs.set_reporter (Logs_fmt.reporter ()); *)
    (* Logs.set_reporter (reporter (Format.err_formatter)) *)
    ()

let dot_file_oc_of_now () =
  let dot_file = Filename.of_parts [ "dot"; filename_of_now () ^ ".dot" ] in
  Out_channel.create dot_file

let close () =
  match !saved_oc with Some oc -> Out_channel.close oc | None -> ()

let log_choices_complete debug choices_complete_z3 =
  if debug
  then
    Logs.debug ~src:src_solver (fun m ->
        m "Z3_choices_complete: %a"
          Fmt.(Dump.list string)
          (List.map ~f:Z3.Expr.to_string choices_complete_z3))
