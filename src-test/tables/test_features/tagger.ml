
open Core

let () =
  let open List.Let_syntax in
  let path = "./test/concolic/bjy/" in
  let dirs =
    [ "deep-type-error"
    ; "oopsla-24-tests-ill-typed"
    ; "oopsla-24-benchmarks-ill-typed"
    ; "sato-bjy-ill-typed"
    ; "scheme-pldi-2015-ill-typed" ]
  in
    
  let _ = 
    dirs
    >>| String.append path
    |> Tag_table.Counts_table.make_of_dirs
    |> Latex_tbl.show
    |> print_endline
  in

  (* let _ =
    dirs
    >>| String.append path
    |> Tag_table.Full_table.make_of_dirs
    |> Latex_tbl.show
    |> print_endline
  in *)

  ()