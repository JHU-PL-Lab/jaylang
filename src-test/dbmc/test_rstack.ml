open Core

module To_test = struct
  module R = Dbmc.Rstack

  let empty = R.empty
  let test_empty = R.equal empty empty
  let pop_exn r f = Option.value_exn (R.pop r f)
  let r0 = empty
  let x = Dj_common.Id.Ident "x"
  let y = Dj_common.Id.Ident "y"
  let f = Dj_common.Id.Ident "f"
  let g = Dj_common.Id.Ident "g"
  let xf : R.frame = (x, f)
  let r1 = R.push r0 xf
  let r2 = pop_exn r1 xf
  let testcase_true op () = Alcotest.(check bool) "true" true op
  let testcase_false op () = Alcotest.(check bool) "false" false op
  let testcase_empty = testcase_true (R.equal empty empty)
  let test_eq r1 r2 = testcase_true (R.equal r1 r2)
  let test_neq r1 r2 = testcase_false (R.equal r1 r2)
end

let () =
  let open Alcotest in
  run "Rstack"
    [
      ( "basic",
        [
          test_case "empty" `Quick To_test.testcase_empty;
          test_case "push then pop" `Quick To_test.(test_eq r0 r2);
          test_case "push and empty" `Quick To_test.(test_neq r0 r1)
          (* test_case "see printed" `Quick To_test.test_expect_show; *);
        ] );
    ]
