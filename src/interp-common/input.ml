
open Core

(* can be packed into a list *)
include Utils.Pack.Make (Utils.Identity)

let to_string = function
  | I i -> Int.to_string i
  | B b -> Bool.to_string b

let input_conv : t Cmdliner.Arg.conv =
  let parser = function
    | "true" -> `Ok (B true)
    | "false" -> `Ok (B false)
    | s ->
      match Int.of_string_opt s with
      | Some i -> `Ok (I i)
      | None -> `Error (Format.sprintf "Failed to parse an input from '%s'." s)
  in
  let printer formatter i = 
    Format.fprintf formatter "%s" (to_string i)
  in
  parser, printer

let parse_list =
  let open Cmdliner.Arg in
  value & opt (list ~sep:' ' input_conv) [] & info ["inputs"] ~doc:"Input list for interpreter"
