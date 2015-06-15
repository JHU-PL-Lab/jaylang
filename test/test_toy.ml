open OUnit2

let all_tests =
  [ Test_toy_utils.tests
  ; Test_toy_files.tests
  ];;

run_test_tt_main ("Toy" >::: all_tests);;
