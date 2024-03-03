open Core

type test_group = string list

and t = {
  testcases_to_time : test_group;
  testcases_not_time : test_group;
  repeat : int;
  timeout : string;
  engine : string; [@default "concolic"]
  bin : string; [@default "./cj.exe"]
  test_path : string; [@default "test/dbmc/concolic/bjy_tests"]
  working_path : string; [@default "benchmark/dbmc/_working"]
  result_path : string; [@default "benchmark/dbmc/result"]
}
[@@deriving sexp]
