
open Core
open Concolic_common
open Csv

(*write basic test with only basic stats: 
test_name, interpc, solvec, totalc, interpd, solved, totald, loc

then have another test that includes the obscure stats 
(polymorphic types, etc)*)

module Basic_test (* : Latex_table.ROW *) = struct
  module Trial = struct 
    type t =
      | Number of int
      | Average
  end

  type t =
    { testname           : Filename.t
    ; ctest_result       : Status.Terminal.t
    ; dtest_result       : Status.Terminal.t
    ; ctime_to_interpret : Time_float.Span.t
    ; ctime_to_solve     : Time_float.Span.t
    ; ctotal_time        : Time_float.Span.t
    ; dtime_to_interpret : Time_float.Span.t
    ; dtime_to_solve     : Time_float.Span.t
    ; dtotal_time        : Time_float.Span.t
    ; trial              : Trial.t
    (* ; lines_of_code     : int *) (* not needed because is derived from the testname *)
    ; metadata           : Metadata.t }

  let to_strings x =
    let span_to_ms_string =
      fun span ->
        let fl = Time_float.Span.to_ms span in
        Float.to_string @@
        if Float.(fl < 1.)
        then Float.round_decimal fl ~decimal_digits:2
        else Float.round_significant fl ~significant_digits:2
    in
    [ Filename.chop_extension x.testname
    ; span_to_ms_string x.ctime_to_interpret
    ; span_to_ms_string x.ctime_to_solve
    ; span_to_ms_string x.ctotal_time
    ; span_to_ms_string x.dtime_to_interpret
    ; span_to_ms_string x.dtime_to_solve
    ; span_to_ms_string x.dtotal_time
    ; Int.to_string (Utils.Cloc_lib.count_bjy_lines x.testname) ]
    @ (
      match Metadata.tags_of_t x.metadata with
      | `Sorted_list ls ->
        List.map ls ~f:(function
          | `Absent -> "--"
          | `Feature tag -> Ttag.V2.to_char tag |> Char.to_string
          | `Reason tag ->
            Ttag.V2.to_char tag
            |> Char.to_string
            |> Latex_format.red
          )
    )

  let of_testname (n_trials : int) (runtest1 : Lang.Ast.some_program -> Status.Terminal.t) (runtest2 : Lang.Ast.some_program -> Status.Terminal.t) (testname : Filename.t) : string list list =
    assert (n_trials > 0);
    let metadata = Metadata.of_bjy_file testname in
    let test_one (n : int) : t =
      let source = Lang.Parser.parse_program_from_file testname
      in
      let cinterp0 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let csolve0 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in
      let ct0 = Caml_unix.gettimeofday () in
      let ctest_result = runtest1 source in
      let ct1 = Caml_unix.gettimeofday () in
      let cinterp1 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let csolve1 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in

      let dinterp0 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let dsolve0 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in
      let dt0 = Caml_unix.gettimeofday () in
      let dtest_result = runtest2 source in
      let dt1 = Caml_unix.gettimeofday () in
      let dinterp1 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let dsolve1 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in

      let row =
        { testname
        ; ctest_result
        ; dtest_result
        ; ctime_to_interpret = Time_float.Span.of_sec (cinterp1 -. cinterp0)
        ; ctime_to_solve = Time_float.Span.of_sec (csolve1 -. csolve0)
        ; ctotal_time = Time_float.Span.of_sec (ct1 -. ct0)
        ; dtime_to_interpret = Time_float.Span.of_sec (dinterp1 -. dinterp0)
        ; dtime_to_solve = Time_float.Span.of_sec (dsolve1 -. dsolve0)
        ; dtotal_time = Time_float.Span.of_sec (dt1 -. dt0)
        ; trial = Number n
        ; metadata }
      in
      row
    in
    let trials = List.init n_trials ~f:(test_one) in
    let avg_trial =
      List.fold
        trials
        ~init:{
          testname
          ; ctest_result = Status.Exhausted_pruned_tree (* just arbitrary initial result *)
          ; dtest_result = Status.Exhausted_pruned_tree
          ; ctime_to_interpret = Time_float.Span.of_sec 0.0
          ; ctime_to_solve = Time_float.Span.of_sec 0.0
          ; ctotal_time = Time_float.Span.of_sec 0.0
          ; dtime_to_interpret = Time_float.Span.of_sec 0.0
          ; dtime_to_solve = Time_float.Span.of_sec 0.0
          ; dtotal_time = Time_float.Span.of_sec 0.0
          ; trial = Average
          ; metadata
        }
        ~f:(fun acc x ->
          { acc with (* sum up *)
            ctest_result = x.ctest_result (* keeps most recent test result *)
          ; dtest_result = x.dtest_result
          ; ctime_to_interpret = Time_float.Span.(acc.ctime_to_interpret + x.ctime_to_interpret)
          ; ctime_to_solve = Time_float.Span.(acc.ctime_to_solve + x.ctime_to_solve)
          ; ctotal_time = Time_float.Span.(acc.ctotal_time + x.ctotal_time)
          ; dtime_to_interpret = Time_float.Span.(acc.dtime_to_interpret + x.dtime_to_interpret)
          ; dtime_to_solve = Time_float.Span.(acc.dtime_to_solve + x.dtime_to_solve)
          ; dtotal_time = Time_float.Span.(acc.dtotal_time + x.dtotal_time)
          })
      |> fun r ->
        { r with (* average out *)
          ctime_to_interpret = Time_float.Span.(r.ctime_to_interpret / (Int.to_float n_trials))
        ; ctime_to_solve = Time_float.Span.(r.ctime_to_solve / (Int.to_float n_trials))
        ; dtime_to_interpret = Time_float.Span.(r.dtime_to_interpret / (Int.to_float n_trials))
        ; dtime_to_solve = Time_float.Span.(r.dtime_to_solve / (Int.to_float n_trials))
        ; ctotal_time = Time_float.Span.(r.ctotal_time / (Int.to_float n_trials))
        ; dtotal_time = Time_float.Span.(r.dtotal_time / (Int.to_float n_trials))
        }
    in
    List.map (trials @ [ avg_trial ]) ~f:(to_strings)
end

let of_dirs ?(avg_only : bool = true) (n_trials : int) (dirs : Filename.t list) (runtest1 : Lang.Ast.some_program -> Status.Terminal.t) (runtest2 : Lang.Ast.some_program -> Status.Terminal.t): string list list =
      let all_data = List.sort (Utils.File_utils.get_all_bjy_files dirs) ~compare:(fun a b -> String.compare (Filename.basename a) (Filename.basename b))
      |> List.map ~f:(fun file -> let test_run = Basic_test.of_testname n_trials runtest1 runtest2 file in 
      if avg_only then [List.hd_exn (List.rev test_run)] else test_run) in
      List.concat all_data  

let cdbench_args =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ n_trials = value & opt int 50 & info ["trials"] ~doc:"Number of trials"
  and+ dirs = value & opt (list ~sep:' ' dir) [ "test/bjy/oopsla-24-benchmarks-ill-typed" ] & info ["dirs"] ~doc:"Directories to benchmark" in
  n_trials, dirs

let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cdbenchmark") @@
  let+ options = Options.cmd_arg_term
  and+ n_trials, dirs = cdbench_args in
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest1 pgm =
      Concolic.Driver.test_some_program
      ~options
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
      pgm
  in
  let runtest2 pgm =
    Deferred.Cmain.test_some_program
      ~options
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
      pgm
  in
  let results = of_dirs n_trials dirs runtest1 runtest2 in
  let ctimes = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 5)) in
  let dtimes = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 8)) in
  let cinterps = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 3)) in
  let csolves = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 4)) in
  let dinterps = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 6)) in
  let dsolves = List.map results ~f:(fun ls -> float_of_string (List.nth_exn ls 7)) in
  let cmean =
    let total = List.fold ctimes ~init:0.0 ~f:(+.) in
    total /. Int.to_float (List.length ctimes)
  in
  let dmean =
    let total = List.fold dtimes ~init:0.0 ~f:(+.) in
    total /. Int.to_float (List.length ctimes)
  in
  let cmedian =
    List.sort ctimes ~compare:Float.compare
    |> Fn.flip List.nth_exn (List.length ctimes / 2)
  in
  let dmedian =
    List.sort dtimes ~compare:Float.compare
    |> Fn.flip List.nth_exn (List.length ctimes / 2)
  in
  Format.set_formatter_out_channel Out_channel.stdout;
  Format.printf "Mean time of concolic tests: %fms\nMedian time of concolic tests: %fms\n" 
    cmean 
    cmedian;
  Format.printf "Mean time of deferred tests: %fms\nMedian time of deferred tests: %fms\n" 
    dmean 
    dmedian;
  Format.printf "Total concolic interpretation time: %fs\nTotal concolic solving time: %fs\n"
    (List.fold cinterps ~init:0.0 ~f:(+.))
    (List.fold csolves ~init:0.0 ~f:(+.));
  Format.printf "Total deferred interpretation time: %fs\nTotal deferred solving time: %fs\n"
    (List.fold dinterps ~init:0.0 ~f:(+.))
    (List.fold dsolves ~init:0.0 ~f:(+.));

  save "testname.csv" results

(*
  Common directories to benchmark include

    "test/bjy/soft-contract-ill-typed"
    "test/bjy/deep-type-error"
    "test/bjy/oopsla-24-tests-ill-typed"; "test/bjy/sato-bjy-ill-typed"
    "test/bjy/interp-ill-typed"

  To test multiple directories, put them in single quotes and separate by spaces.
*)

let () =
  match Cmdliner.Cmd.eval_value' @@ run () with
  | `Ok _ -> ()
  | `Exit i -> exit i