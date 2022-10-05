open Core

let ctx = Z3.mk_context []

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module To_test = struct
  (* open SuduZ3 *)

  let _solver = Z3.Solver.mk_solver SuduZ3.ctx None
  let heavy () = 
    
end

let () =
  let open Alcotest in
  run "Sudu"
    [
      ( "heavy",
        [
          test_case "heavy" `Quick (fun () ->
              Alcotest.(check bool) "same bool" true @@ To_test.heavy ());
        ] );
    ]
