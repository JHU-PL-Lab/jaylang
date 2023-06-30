open Core

type test_group = string list

and t = {
  testcases_to_time : test_group;
  testcases_not_time : test_group;
  repeat : int;
  timeout : string;
  engine : string; [@default "dbmc"]
  test_path : string; [@default "benchmark/dbmc/cases/ddse"]
  working_path : string; [@default "benchmark/dbmc/_working"]
  result_path : string; [@default "benchmark/dbmc/result"]
}
[@@deriving sexp]
