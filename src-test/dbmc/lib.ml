open Core
open Dj_common

type 'arg test_one_lwt = string -> Lwt_switch.t -> 'arg -> unit Lwt.t

let group_tests_lwt root (test_one : 'arg test_one_lwt) =
  Directory_utils.map_in_groups
    ~f:(fun _ test_name test_path ->
      Alcotest_lwt.test_case test_name `Quick @@ test_one test_path)
    root

type 'arg test_one = string -> 'arg -> unit

let group_tests root (test_one : 'arg test_one) =
  Directory_utils.map_in_groups
    ~f:(fun _ test_name test_path ->
      Alcotest.test_case test_name `Quick @@ test_one test_path)
    root
