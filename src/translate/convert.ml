
open Lang
open Ast

let[@landmark] bjy_to_emb (bjy : Bluejay.pgm) ~(do_wrap : bool) : Embedded.pgm =
  let module Names = Translation_tools.Fresh_names.Make () in
  bjy
  |> Desugar.desugar_pgm (module Names)
  |> Embed.embed_pgm (module Names) ~do_wrap

let[@landmark] bjy_to_des (bjy : Bluejay.pgm) : Desugared.pgm =
  let module Names = Translation_tools.Fresh_names.Make () in
  Desugar.desugar_pgm (module Names) bjy
