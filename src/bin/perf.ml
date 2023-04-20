open Core
open Dj_common

let perf_one_file filename =
  let raw_config = { Global_config.default_config with filename } in
  let config =
    match Test_expect.load_sexp_expectation_for filename with
    | Some eps ->
        let ep = List.hd_exn eps in
        { raw_config with target = Id.Ident ep.target }
    | None -> raw_config
  in
  let is_instrumented = config.is_instrumented in
  Fmt.pr "\n%s" filename ;
  let src = File_utils.read_source ~is_instrumented filename in

  ignore @@ Dbmc.Main.main ~config src

let () =
  let perf_path = "test/dbmc" in

  let grouped_testfiles = Directory_utils.group_all_files perf_path in
  let simplify path = Directory_utils.chop_parent_dir perf_path path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        (simplify group_name, test_names))
  in
  List.iter grouped_tests ~f:(fun (_group_name, test_names) ->
      List.iter test_names ~f:(fun test_name -> perf_one_file test_name)) ;

  ()
