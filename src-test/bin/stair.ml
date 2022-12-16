open Core
open Z3

open Utils

let up_stair i =
  Boolean.mk_implies ctx (vb i) @@ Boolean.mk_and ctx [ vb (i + 1); vb (i + 2) ]

let experiment  k =
  Printf.printf "\n[%d] " k ;
  Solver.reset solver ;
  List.range 0 k |> List.iter ~f:(fun i -> Solver.add solver [ up_stair i ]) ;

  let result = Utils.time_work (fun () -> Z3API.check_with_assumption solver []) in
  print_result result;

  Solver.add solver [ Boolean.mk_not ctx (up_stair 0) ];

  let result = Utils.time_work (fun () -> Z3API.check_with_assumption solver []) in
  print_result result


let () =
  List.range ~stride:1000 10000 15000 |> List.iter ~f:experiment;
  (* List.range ~stride:1000 60000 65000 |> List.iter ~f:experiment; *)

  ()
