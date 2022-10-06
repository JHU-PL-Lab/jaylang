open Core

let ctx = Z3.mk_context []

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module To_test = struct
  (* open SuduZ3 *)

  let solver = Z3.Solver.mk_solver SuduZ3.ctx None

  let heavy () =
    let es =
      List.range 0 50000
      |> List.map ~f:(fun i ->
             SuduZ3.(
               eq
                 (mk_string_s (string_of_int i))
                 (mk_string_s (string_of_int (i + 1)))))
    in
    let _ = Z3.Solver.check solver es in
    let _ = Z3.Solver.check solver es in
    true
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
