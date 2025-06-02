
open Lang
open Ast

let[@landmark] bjy_to_emb (bjy : Bluejay.pgm) ~(do_wrap : bool) ~(do_type_splay : bool) : Embedded.pgm =
  let module Names = Translation_tools.Fresh_names.Make () in
  bjy
  |> Desugar.desugar_pgm (module Names) ~do_type_splay
  |> Embed.embed_pgm (module Names) ~do_wrap ~do_type_splay

let[@landmark] bjy_to_many_emb (bjy : Bluejay.pgm) ~(do_wrap : bool) ~(do_type_splay : bool) : Embedded.pgm Preface.Nonempty_list.t =
  let module Names = Translation_tools.Fresh_names.Make () in
  bjy
  |> Desugar.desugar_pgm (module Names) ~do_type_splay
  |> Embed.embed_fragmented (module Names) ~do_wrap ~do_type_splay

let[@landmark] bjy_to_des (bjy : Bluejay.pgm) ~(do_type_splay : bool) : Desugared.pgm =
  let module Names = Translation_tools.Fresh_names.Make () in
  Desugar.desugar_pgm (module Names) bjy ~do_type_splay
