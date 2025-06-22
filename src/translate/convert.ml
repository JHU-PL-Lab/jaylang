
open Lang
open Ast

let cmd_arg_term =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ do_wrap = value & opt (enum ["yes", true ; "no", false]) true & info ["w"] ~doc:"Wrap flag: yes or no. Default is yes."
  and+ do_type_splay = value & flag & info ["s"] ~doc:"Splay types on recursive functions" in
  (`Do_wrap do_wrap, `Do_type_splay do_type_splay)

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

let bjy_to_erased (bjy : Bluejay.pgm) : Type_erased.pgm =
  Type_erasure.erase bjy
