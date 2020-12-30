open Odefa_interpreter
open Program_samples

module To_test = struct
  let eval e t = 
    let target = (Odefa_ast.Ast.Ident t),[] in
    Naive_interpreter.eval ~target e

  let eval_int e t =
    let open Odefa_ast.Ast in
    match eval e t with
    | Value_int v -> v
    | _ -> failwith "not int"

end

let test_eval v1 v2 () =

  Alcotest.(check int) "equal" v1 v2

let () =
  Alcotest.run "Interpreter"
    [
      ("int-case"),
      [
        Alcotest.test_case "One"    `Quick @@ test_eval 1 (To_test.eval_int e1 "t") ;
        Alcotest.test_case "Plus"   `Quick @@ test_eval 1 (To_test.eval_int e2 "t") ;
        Alcotest.test_case "Cond"   `Quick @@ test_eval 1 (To_test.eval_int e3 "t") ;
        Alcotest.test_case "Record" `Quick @@ test_eval 1 (To_test.eval_int e4 "t") ;
      ]
    ]