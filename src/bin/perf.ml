open Core
open Dj_common
open Log.Export

let dummy_name = ""

let perf_one_file (config : Global_config.t) short_name filename =
  let config = { config with filename } in
  let config = Global_config.load_expect config in
  let result = Dbmc.Main.main_config config in
  Dbmc.Observe.dump_check_info short_name result.state

let perf_group log_name encode_policy perf_path =
  let logfile = Log.filename_with log_name in
  let raw_config =
    {
      Global_config.default_config with
      filename = dummy_name;
      timeout = Some (Time_float.Span.of_int_sec 5);
      mode = Global_config.Dbmc_perf;
      encode_policy;
      global_logfile = Some logfile;
    }
  in
  Log.init_global logfile ;
  PLog.debug (fun m -> m "file,total,resource") ;

  Directory_utils.iter_in_groups
    ~f:(fun _ short_name test_path ->
      perf_one_file raw_config short_name test_path)
    perf_path ;
  Log.close ()

let () =
  let perf_path = "test/dbmc/simple" in

  perf_group "perf-inc.csv" Only_incremental perf_path ;
  perf_group "perf-shr.csv" Always_shrink perf_path
