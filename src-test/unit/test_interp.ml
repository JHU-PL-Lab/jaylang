open Dbmc
open Program_samples

module To_test = struct
  let eval e =
    let session = Interpreter.make_default_session () in
    try Interpreter.eval session e with
    | Interpreter.Terminate v -> Interpreter.value_of_dvalue v
    | ex -> raise ex

  let eval_int e =
    let open Jayil.Ast in
    match eval e with Value_int v -> v | _ -> failwith "not int"
end

let test_eval v1 v2 () = Alcotest.(check int) "equal" v1 v2

let () =
  Alcotest.run "Interpreter"
    [
      ( "int-case",
        [
          Alcotest.test_case "One" `Quick @@ test_eval 1 (To_test.eval_int e1);
          Alcotest.test_case "Plus" `Quick @@ test_eval 1 (To_test.eval_int e2);
          Alcotest.test_case "Cond" `Quick @@ test_eval 1 (To_test.eval_int e3);
          Alcotest.test_case "Record" `Quick
          @@ test_eval 1 (To_test.eval_int e4);
        ] );
    ]
