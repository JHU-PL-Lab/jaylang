open Core
open Dj_common

let perf_one_file filename =
  let raw_config =
    {
      Global_config.default_config with
      filename;
      timeout = Some (Time.Span.of_int_sec 5);
    }
  in
  let config =
    match Test_expect.load_sexp_expectation_for filename with
    | Some eps ->
        let ep = List.hd_exn eps in
        {
          raw_config with
          target = Id.Ident ep.target;
          log_level_perf = Some Logs.Debug;
        }
    | None -> raw_config
  in
  let is_instrumented = config.is_instrumented in
  Log.init config ;
  Fmt.pr "\n%s" filename ;
  let src = File_utils.read_source ~is_instrumented filename in

  ignore @@ Dbmc.Main.main ~config src

let () =
  let perf_path = "test/dbmc/simple" in
  Directory_utils.iter_in_groups
    ~f:(fun _ _ test_path -> perf_one_file test_path)
    perf_path
