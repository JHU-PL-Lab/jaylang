
open Core
open Concolic.Common

module Driver = Concolic.Driver.Of_logger (Utils.Logger.Transformer_of_builder (Utils.Dlist.Specialize (Stat)))

type tape = Driver.tape

module Basic_test = struct
  module Trial = struct
    type t =
      | Number of int
      | Average
  end

  type[@warning "-69"] t =
    { testname : Filename.t
    (* ; test_result : Status.Terminal.t *)
    ; interp_time : Mtime.Span.t
    ; solve_time : Mtime.Span.t
    ; total_time : Mtime.Span.t
    ; trial : Trial.t
    ; mode : string }

  let names =
    [ " Test Name" ; "Interp Time" ; "Solve Time" ; "Total" ; "Mode" ]

  let to_strings x =
    let span_to_ms_string =
      fun span ->
        let fl = Utils.Time.span_to_ms span in
        Float.to_string @@
        if Float.(fl < 1.)
        then Float.round_decimal fl ~decimal_digits:2
        else Float.round_significant fl ~significant_digits:2
    in
    [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.')
    ; span_to_ms_string x.interp_time
    ; span_to_ms_string x.solve_time
    ; span_to_ms_string x.total_time
    ; x.mode ]

  let make
    (trial : Trial.t)
    (mode : string)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape) (* promises to update interp and solve time *)
    (testname : Filename.t)
    : t =
    let source = Lang.Parser.parse_program_from_file testname in (* span should maybe include this *)
    let span, (_, tape) = Utils.Time.time runtest source in
    let stat_list = tape [] in
      { testname
      ; interp_time = Stat.sum_time Stat.Interp_time stat_list
      ; solve_time = Stat.sum_time Stat.Solve_time stat_list
      ; total_time = span (* ignores stats measured total time *)
      ; trial
      ; mode }

  let average (tests : t list) : t =
    match tests with
    | [] -> failwith "Cannot take average of empty test"
    | [ test ] -> { test with trial = Average }
    | hd :: tl ->
      let n_trials = List.length tests in
      let init = { hd with trial = Average } in
      let sum =
        List.fold tl ~init ~f:(fun acc x ->
          { acc with
            interp_time = Mtime.Span.add acc.interp_time x.interp_time
          ; solve_time = Mtime.Span.add acc.solve_time x.solve_time
          ; total_time = Mtime.Span.add acc.total_time x.total_time }
        )
      in
      { sum with
        interp_time = Utils.Time.divide_span sum.interp_time n_trials
      ; solve_time = Utils.Time.divide_span sum.solve_time n_trials
      ; total_time = Utils.Time.divide_span sum.total_time n_trials
      }
end 

module Result_table = struct
  type t = Basic_test.t Latex_tbl.t

  let empty : t =
    { row_module = (module Basic_test)
    ; rows = []
    ; columns = [] }

  (* TODO: we should interweave the computations so as not to favor one *)
  (* Then should have a tester type, which comes with a "mode" name.
    And Basic_test can put many results in the same row *)
  let of_testname 
    ?(avg_only : bool = true)
    (mode : string)
    (n_trials : int)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
    (testname : Filename.t)
    : t =
    Latex_tbl.append_rows empty (
      let results =
        List.init n_trials ~f:(fun n ->
          Basic_test.make (Number n) mode runtest testname
        )
      in
      let avg = Basic_test.average results in
      if avg_only
      then [ avg ]
      else avg :: results
    )
  let of_dirs
    ?(avg_only : bool = true)
    (mode : string)
    (n_trials : int)
    (dirs : Filename.t list)
    (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
    : t =
    let open List.Let_syntax in
    dirs
    |> Utils.File_utils.get_all_bjy_files
    |> List.sort ~compare:(fun a b -> String.compare (Filename.basename a) (Filename.basename b))
    >>| of_testname ~avg_only mode n_trials runtest
    |> List.reduce_exn ~f:Latex_tbl.concat

  let add_average
    (mode : string)
    (tbl : t)
    : t =
    let t0 = Mtime.Span.zero in
    let init = t0, t0, t0, 0 in
    let interp_sum, solve_sum, total_sum, n =
      List.fold tbl.rows ~init ~f:(fun ((acc_interp_time, acc_solve_time, acc_total_time, n) as acc) row_or_hline ->
        match row_or_hline with
        | Row row ->
          Mtime.Span.add acc_interp_time row.interp_time
          , Mtime.Span.add acc_solve_time row.solve_time
          , Mtime.Span.add acc_total_time row.total_time
          , Int.(n + 1)
        | Hline -> acc
      )
    in
    Latex_tbl.append_rows tbl [
      Basic_test.{
          testname = mode ^ " average" 
        ; trial = Average
        ; mode
        ; interp_time = Utils.Time.divide_span interp_sum n
        ; solve_time = Utils.Time.divide_span solve_sum n
        ; total_time = Utils.Time.divide_span total_sum n
      }
    ]
end

open Result_table

let cdbench_args =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ n_trials = value & opt int 50 & info ["trials"] ~doc:"Number of trials"
  and+ dirs = value & opt (list ~sep:' ' dir) [ "test/bjy/oopsla-24-benchmarks-ill-typed" ] & info ["dirs"] ~doc:"Directories to benchmark" in
  n_trials, dirs

(* let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cbenchmark") @@
  let+ options = Options.cmd_arg_term
  and+ n_trials, dirs = cdbench_args in
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest pgm =
    let test_program = 
      match mode with
      | `Eager -> Concolic.Driver.test_some_program
      | `Deferred -> Deferred.Cmain.test_some_program
    in
    test_program
      ~options
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
      pgm
  in
  let tbl = Result_table.of_dirs n_trials dirs runtest in
  let times =
    List.filter_map tbl.rows ~f:(function
        | Row row -> Some (Time_float.Span.to_ms row.total_time)
        | Hline -> None
      )
  in
  let mean =
    let total = List.fold times ~init:0.0 ~f:(+.) in
    total /. Int.to_float (List.length times)
  in
  let median =
    List.sort times ~compare:Float.compare
    |> Fn.flip List.nth_exn (List.length times / 2)
  in
  Format.set_formatter_out_channel Out_channel.stdout;
  tbl
  |> Latex_tbl.show ~hum
  |> Format.printf "%s\n";
  Format.printf "Mean time of all tests: %fms\nMedian time of all tests: %fms\n" 
    mean 
    median;
  Format.printf "Total interpretation time: %fs\nTotal solving time: %fs\n"
    (Utils.Safe_cell.get Concolic.Evaluator.global_runtime) 
    (Utils.Safe_cell.get Concolic.Evaluator.global_solvetime) *)

(*
  Common directories to benchmark include

    "test/bjy/soft-contract-ill-typed"
    "test/bjy/deep-type-error"
    "test/bjy/oopsla-24-tests-ill-typed"; "test/bjy/sato-bjy-ill-typed"
    "test/bjy/interp-ill-typed"

  To test multiple directories, put them in single quotes and separate by spaces.
*)

(* let () =
  match Cmdliner.Cmd.eval_value' @@ run () with
  | `Ok _ -> ()
  | `Exit i -> exit i *)

let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cdbenchmark") @@
  let+ options = Options.cmd_arg_term
  and+ n_trials, dirs = cdbench_args in
  (* prepare channels to capture and discard all testing output *)
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest_eager pgm =
    Driver.Eager.test_some_program
      ~options:{ options with random = true }
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
      pgm
  in
  let runtest_deferred pgm =
    Driver.Deferred.test_some_program
      ~options:{ options with random = true }
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
      pgm
  in
  let eager_results = of_dirs "Eager" n_trials dirs runtest_eager |> Result_table.add_average "Eager" in
  let deferred_results = of_dirs "Deferred" n_trials dirs runtest_deferred |> Result_table.add_average "Deferred" in
  let results = Latex_tbl.concat eager_results deferred_results in
  (* let results = of_dirs n_trials dirs runtest1 runtest2 in
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
  in *)
  (* Testing is done, so we can set back the stdout channel *)
  Format.set_formatter_out_channel Out_channel.stdout;
  results 
  |> Latex_tbl.show ~hum:true
  |> Format.printf "\n%s\n"
  (* Format.printf "Mean time of concolic tests: %fms\nMedian time of concolic tests: %fms\n" 
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

  save "testname.csv" results *)

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