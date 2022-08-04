open Core

type test_group = string list

and t = {
  testcases_to_time : test_group;
  testcases_not_time : test_group;
  repeat : int;
  timeout : string;
  test_path : string; [@default "benchmark/cases"]
  working_path : string; [@default "benchmark/_working"]
  result_path : string; [@default "benchmark/result"]
}
[@@deriving sexp]
