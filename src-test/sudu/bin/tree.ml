open Core
open Z3
open Utils

let leafs i =
  Boolean.mk_implies ctx (vb i)
  @@ Boolean.mk_and ctx [ vb ((i * 2) + 1); vb ((i * 2) + 2) ]

let set_true i = Boolean.[ mk_eq ctx (mk_true ctx) (vb i) ]
let set_false i = Boolean.[ mk_eq ctx (mk_false ctx) (vb i) ]

let experiment k =
  Printf.printf "\n[%d] " k ;
  Solver.reset solver ;
  List.range 0 k |> List.iter ~f:(fun i -> Solver.add solver [ leafs i ]) ;
  Solver.add solver (set_true 0) ;

  let result =
    Utils.time_work (fun () -> Z3API.check_with_assumption solver [])
  in
  print_result result ;

  Solver.add solver (set_false 2) ;

  let result =
    Utils.time_work (fun () -> Z3API.check_with_assumption solver [])
  in
  print_result result

let () =
  List.range 10 20 |> List.iter ~f:(fun k -> experiment (Int.pow 2 k)) ;
  ()