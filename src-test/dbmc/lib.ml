open Core
open Dj_common

type 'arg test_one = string -> Lwt_switch.t -> 'arg -> unit Lwt.t

let group_tests_lwt root (test_one_lwt : 'arg test_one) =
  Directory_utils.map_in_groups
    ~f:(fun _ test_name test_path ->
      Alcotest_lwt.test_case test_name `Quick @@ test_one_lwt test_path)
    root
