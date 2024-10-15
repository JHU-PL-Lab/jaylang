open Core

type test_group = string list

and t = {
  testcases_to_time : test_group;
  testcases_not_time : test_group;
  repeat : int;
  timeout : string;
  engine : string; [@default "concolic"]
  bin : string; [@default "./cj.exe"]
  test_path : string; [@default "benchmark/concolic/jil_wrap"]
  working_path : string; [@default "benchmark/concolic/_working"]
  result_path : string; [@default "benchmark/concolic/result"]
}
[@@deriving sexp]
