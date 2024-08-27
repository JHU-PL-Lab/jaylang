
open Core

let texttt (s : string) : string =
  "\\texttt{" ^ String.substr_replace_all ~pattern:"_" ~with_:"\\_" s ^ "}"