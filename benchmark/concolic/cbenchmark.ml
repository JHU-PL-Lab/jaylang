open Core
open Shexp_process
open Shexp_process.Infix

(* open Shexp_process.Logged *)
(* let _make_result_file () =
   run "date" [ "-u"; "+%Y_%m_%d_%H_%M_%S" ]
   |- run "xargs" [ "-I"; "%"; "mkdir"; "-p"; "result/%" ] *)

let is_mac =
  let ic = Caml_unix.open_process_in "uname" in
  let uname = In_channel.input_line ic in
  let () = In_channel.close ic in
  Option.value_map uname ~default:false ~f:(fun os -> String.equal os "Darwin")

let time_bin = if is_mac then "gtime" else "/usr/bin/time"
let timeout_bin = if is_mac then "gtimeout" else "/usr/bin/timeout"
let testcase_path (cfg : Cconfig.t) test =
  Filename.concat cfg.test_path test

let escape_path s = String.substr_replace_all ~pattern:"/" ~with_:"_" s

let result_path (cfg : Cconfig.t) test n =
  Filename.concat cfg.working_path
    ((escape_path test ^ "_" ^ string_of_int n) ^ ".txt")

let time_result_path (cfg : Cconfig.t) test =
  Filename.concat cfg.working_path (escape_path test ^ ".time.txt")

let prepare (cfg : Cconfig.t) : unit t =
  run "rm" [ "-rf"; cfg.working_path ]
  >> run "mkdir" [ "-p"; cfg.working_path ]
  >> run "mkdir" [ "-p"; cfg.result_path ]

let benchmark_time (cfg : Cconfig.t) test n : unit t =
  let file = testcase_path cfg test
  and test_time_result = time_result_path cfg test in
  print test
  (* time gtimeout --foreground 1m ls *)
  >> stdout_to ~append:() (result_path cfg test n)
       (call
          [
            time_bin;
            "-o";
            test_time_result;
            "-a";
            "-f";
            "%e\n\
             %Uuser %Ssystem %Eelapsed %PCPU (%Xtext+%Ddata %Mmax)k \
             %Iinputs+%Ooutputs (%Fmajor+%Rminor)pagefaults %Wswaps";
            timeout_bin;
            "--foreground";
            cfg.timeout;
            cfg.bin;
            "-t";
            "90.0"; (* single test timeout *)
            "-q"; (* quits when finds abort *)
            "-i";
            file;
            (* Note I don't add the `r` flag for randomness. This way benchmarks are reproducable. *)
          ])
  >> echo @@ " done - " ^ string_of_int n
(* |- run "tee" ["-a"; result_file] *)

let stat (cfg : Cconfig.t) : unit t =
  let working_path = cfg.working_path in
  let testcases = cfg.testcases_to_time in
  let table = working_path ^ "/0table.txt" in
  stdout_to ~append:() table (echo @@ "Testing files from " ^ cfg.test_path ^ "\n")
  >> List.iter testcases ~f:(fun testcase ->
      stdout_to ~append:() table
        (run "echo" [ testcase ]
        >> run "awk"
             [
               "BEGIN {sum=0;count=0} {if (NR%2==1) {sum=sum+$1; \
                count=count+1}}  END {print sum/count} ";
               time_result_path cfg testcase;
             ]
        >> run "awk" [ "FNR == 2"; result_path cfg testcase 1 ]
           (* >> run "awk" ["BEGIN {sum=0;count=0} {FNR==NR; if (NR==2) {sum=sum+$1; count=count+1}} {next} {print sum/count}' result/evaluate-2020-05-22-23:55:15Z/*.result.txt"] *)
        ))

let benchmark_not_time (cfg : Cconfig.t) : unit t =
  let extra_result = cfg.working_path ^ "/1extra.txt" in
  let testcases = cfg.testcases_not_time in
  List.iter testcases ~f:(fun test ->
      let file = testcase_path cfg test in
      stdout_to ~append:() extra_result
        (call [ cfg.bin; "-i"; file; "-q"; "-t"; "10.0" ]))
  >> echo @@ "done - extra"

let collect_result (cfg : Cconfig.t) : unit t =
  (* let awk_job = "awk 'BEGIN {sum=0;count=0} {if (NR%2==1) {sum=sum+$1;count=count+1}}  END {print sum/count}' $f"
     in
     in (run ("for f in " ^ working_path ^ "*.time.txt; do " ^ awk_job ^ "; done") [])
     (run "./script/echo.sh" [])
     >> *)
  run "date" [ "-u"; "+%Y-%m-%d-%H:%M:%S" ]
  |- run "xargs"
       [
         "-I";
         "%";
         "mv";
         cfg.working_path;
         Filename.concat cfg.result_path (cfg.engine ^ "-%");
       ]

let rec countdown n task : unit t =
  if n > 0 then task n >> countdown (n - 1) task else echo "done"

let read_config () =
  let engine = ref "concolic" in
  let config_path = ref "benchmark/concolic/config.s" in

  Arg.parse
    [
      ("-e", Arg.Set_string engine, "Engine");
      ("-f", Arg.Set_string config_path, "Config path");
    ]
    (fun _ -> ())
    "Please use `make cbenchmark`." ;
  assert (Core.List.mem [ "concolic" ] !engine ~equal:String.equal) ;

  let config = Sexp.load_sexp_conv_exn !config_path Cconfig.t_of_sexp in
  { config with engine = !engine }

let () =
  let cfg = read_config () in
  (* let s = cfg |> Cconfig.sexp_of_t |> Sexp.to_string_hum in
     print_endline s ; *)
  let testcases = cfg.testcases_to_time in
  ignore
  @@ eval
       (prepare cfg
       >> List.iter testcases ~f:(fun task ->
              countdown cfg.repeat (benchmark_time cfg task))
       >> stat cfg >> benchmark_not_time cfg >> collect_result cfg)
