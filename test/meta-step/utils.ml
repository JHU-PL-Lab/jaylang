open Batteries
open Odefa_parser

let parse s = 
  s
  |> IO.input_string
  |> Parser.parse_program