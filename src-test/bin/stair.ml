open Core
open Z3

let ctx = Z3.mk_context []

module Z3API = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

let bool_sort = Boolean.mk_sort ctx
let vb i = Expr.mk_const_s ctx ("b" ^ string_of_int i) bool_sort

let up_stair i =
  Boolean.mk_implies ctx (vb i) @@ Boolean.mk_and ctx [ vb (i + 1); vb (i + 2) ]

let solver = Z3.Solver.mk_solver ctx None

let experiment ?(cut = false) k =
  Printf.printf "\n" ;
  Solver.reset solver ;
  if cut then Solver.add solver [ Boolean.mk_not ctx (up_stair 0) ] else () ;
  List.range 0 k |> List.iter ~f:(fun i -> Solver.add solver [ up_stair i ]) ;

  let start_time = Time_ns.now () in
  let result = Z3API.check_with_assumption solver [] in
  (match result with
  | Result.Ok _model -> Printf.printf "[%d] SAT" k
  | Result.Error _ -> Printf.printf "[%d] UNSAT" k) ;
  let span = Time_ns.(diff (now ()) start_time) in
  Printf.printf " in %f" (Time_ns.Span.to_sec span)

let () =
  List.range ~stride:1000 100000 105000 |> List.iter ~f:experiment ;
  List.range ~stride:1000 100000 105000 |> List.iter ~f:(experiment ~cut:true) ;
  ()
