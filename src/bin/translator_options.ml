open Batteries

exception Argument_parse_failure

module Translator_constants = struct
  let version = "0.1"
end

(** Determines the type of translation to perform. *)
type translator_mode =
  | Bluejay_to_jayil
  | Jay_to_jayil
  | Bluejay_to_jay
  | Scheme_to_jay

let named_translator_modes =
  [
    (Bluejay_to_jayil, "bluejay-to-jayil");
    (Jay_to_jayil, "jay-to-jayil");
    (Bluejay_to_jay, "bluejay-to-jay");
    (Scheme_to_jay, "scheme-to-jay");
  ]

type translator_args = { ta_mode : translator_mode; ta_instrument : bool }
(** Describes arguments passed to the translator executable as structured data. *)

let logging_option_parser : unit BatOptParse.Opt.t =
  {
    (* Called whenever e.g. "--log debug" appears in the argument list *)
    option_set =
      (let open Jhupllib.Logger_utils in
      fun option_name args ->
        let match_string_with_level level_str =
          match level_of_string level_str with
          | Some level -> level
          | None -> failwith ("Invalid log level \"" ^ level_str ^ "\".")
        in
        match args with
        | [ arg ] -> (
            let module_name_option, module_level =
              if BatString.exists arg "="
              then
                let module_name, module_level = String.split ~by:"=" arg in
                (Some module_name, module_level)
              else (None, arg)
            in
            let level' = match_string_with_level module_level in
            match module_name_option with
            | Some module_name -> set_logging_level_for module_name level'
            | None -> set_default_logging_level level')
        | _ ->
            raise
            @@ BatOptParse.Opt.Option_error (option_name, "Invalid argument"));
    option_set_value = (fun _ -> ());
    option_get = (fun () -> Some ());
    option_metavars = [ "LOG_INSTR" ];
    option_defhelp = Some "Sets the logging level.";
  }

type parsers = {
  parse_mode : translator_mode BatOptParse.Opt.t;
  parse_logging : unit BatOptParse.Opt.t;
  parse_instrument : bool BatOptParse.Opt.t;
}

let make_parsers () : parsers =
  {
    parse_mode =
      BatOptParse.Opt.value_option "MODE" (Some Jay_to_jayil)
        (fun s ->
          try List.assoc_inv s named_translator_modes
          with Not_found -> raise Argument_parse_failure)
        (fun _ arg ->
          "Could not understand mode: " ^ arg ^ "\n" ^ "Valid modes are:\n  "
          ^ (named_translator_modes |> List.map snd
            |> List.map (fun s -> "* " ^ s)
            |> String.concat "\n  "));
    parse_logging = logging_option_parser;
    parse_instrument = BatOptParse.StdOpt.store_true ();
  }

exception ParseFailure of string

let insist name parser =
  match parser.BatOptParse.Opt.option_get () with
  | None -> raise @@ ParseFailure (Printf.sprintf "%s is required." name)
  | Some x -> x

let parse_args () : translator_args =
  let cli_parser =
    BatOptParse.OptParser.make ~version:Translator_constants.version ()
  in
  let parsers = make_parsers () in
  (* **** Add options **** *)
  BatOptParse.OptParser.add cli_parser ~short_name:'m' ~long_name:"mode"
    parsers.parse_mode ;
  BatOptParse.OptParser.add cli_parser ~short_name:'l' ~long_name:"log"
    parsers.parse_logging ;
  BatOptParse.OptParser.add cli_parser ~short_name:'a' ~long_name:"instrument"
    parsers.parse_instrument ;
  (* **** Perform parse **** *)
  let positional_args = BatOptParse.OptParser.parse_argv cli_parser in
  try
    match positional_args with
    | [] ->
        {
          ta_mode = insist "mode" parsers.parse_mode;
          ta_instrument =
            Option.default false @@ parsers.parse_instrument.option_get ();
        }
    | _ ->
        raise
        @@ ParseFailure
             (Printf.sprintf "Spurious arguments: %s"
                (String.join " " positional_args))
  with ParseFailure msg ->
    BatOptParse.OptParser.error cli_parser @@ msg ;
    raise
    @@ Jhupllib.Utils.Invariant_failure
         "BatOptParse.OptParser.error was supposed to terminate the program!"
