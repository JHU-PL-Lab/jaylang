open OUnit2

let all_tests =
  [ Test_files.tests
  ; Test_utils.tests
  ];;

run_test_tt_main ("Tests" >::: all_tests);;