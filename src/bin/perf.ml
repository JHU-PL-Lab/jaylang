open Core
open Dj_common
open Log.Export

let logfile = Log.filename_with "perf.csv"

let perf_one_file short_name filename =
  let raw_config =
    {
      Global_config.default_config with
      filename;
      timeout = Some (Time.Span.of_int_sec 5);
      global_logfile = Some logfile;
      (* global_logfile = None; *)
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
  let src = File_utils.read_source ~is_instrumented filename in
  let result = Dbmc.Main.main ~config src in
  Dbmc.Observe.dump_check_info short_name result.state

let () =
  let perf_path = "test/dbmc/simple" in
  (* let perf_path = "test/dbmc" in *)
  Log.init_global logfile ;
  PLog.debug (fun m -> m "file,total,resource") ;
  Directory_utils.iter_in_groups
    ~f:(fun _ short_name test_path -> perf_one_file short_name test_path)
    perf_path
