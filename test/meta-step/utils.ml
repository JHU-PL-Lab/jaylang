open Batteries
open Odefa_parser

open Odefa_ast
open Ast

open Odefa_symbolic_interpreter.Middle_step
open Tracelet
open Tunnel

let parse s = 
  s
  |> IO.input_string
  |> Parser.parse_program

let ident_list_of_trace trace map =
  trace
  |> List.fold_left (fun acc (Frame (tid, pt)) ->
      let tl = Ident_map.find tid map in
      match tl.source_block with
      | Main _ -> pt :: tl.point :: acc
      | Fun _ -> pt:: tl.point :: acc
      | _ -> pt :: acc
    ) []
  |> List.rev

let print_trace trace = 
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list pp_ident) trace)