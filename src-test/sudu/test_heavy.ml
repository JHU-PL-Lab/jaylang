open Core

let ctx = Z3.mk_context [] ;;

Z3.Params.update_param_value ctx "timeout" "500"

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

module To_test = struct
  (* open SuduZ3 *)

  let solver = Z3.Solver.mk_solver SuduZ3.ctx None ;;

  Z3.Params.update_param_value SuduZ3.ctx "timeout" "15000"

  let syme_of_int i =
    Z3.Arithmetic.Integer.mk_const_s SuduZ3.ctx (string_of_int i)

  let zplus e1 e2 = Z3.Arithmetic.mk_add ctx [ e1; e2 ]

  let heavy () =
    let es =
      List.range 0 200000
      |> List.map ~f:(fun i ->
             let a_i = syme_of_int i in
             let a_ip1 = syme_of_int (i + 1) in
             let a_ip2 = syme_of_int (i + 2) in
             SuduZ3.(eq a_i (zplus a_ip1 a_ip2)))
    in
    let _ = Z3.Solver.add solver es in

    let status = Z3.Solver.check solver [] in
    (* let dump_string = Z3.Solver.to_string solver in
       Out_channel.with_file "heavy.smt" ~f:(fun oc ->
           Out_channel.output_string oc dump_string ;
           Out_channel.close oc) ; *)
    SuduZ3.is_sat status
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
