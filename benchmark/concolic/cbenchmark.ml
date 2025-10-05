
open Core
open Concolic.Common

module Driver = Concolic.Driver.Of_logger (Utils.Logger.Transformer_of_builder (Utils.Dlist.Specialize (Stat)))

type tape = Driver.tape

module Report_row (* : Latex_table.ROW *) = struct
  module Trial = struct
    type t =
      | Number of int
      | Average
  end

  type t =
    { testname    : Filename.t
    ; test_result : Status.Terminal.t
    ; interp_time : Mtime.Span.t
    ; solve_time  : Mtime.Span.t
    ; total_time  : Mtime.Span.t
    ; trial       : Trial.t
    ; metadata     : Metadata.t[@warning "-69"] }

  let names =
    [ "Test Name" ; "Interp" ; "Solve" ; "Total" ; "LOC" ]

  let to_strings x =
    let span_to_ms_string =
      fun span ->
        let fl = Utils.Time.span_to_ms span in
        Float.to_string @@ Float.round_decimal fl ~decimal_digits:2
        (* if Float.(fl < 1.)
        then Float.round_decimal fl ~decimal_digits:2
        else Float.round_significant fl ~significant_digits:2 *)
    in
    [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_format.texttt
    ; span_to_ms_string x.interp_time
    ; span_to_ms_string x.solve_time
    ; span_to_ms_string x.total_time
    ; Int.to_string (Utils.Cloc_lib.count_bjy_lines x.testname) ]

  let of_testname
      (n_trials : int)
      (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
      (testname : Filename.t)
    : t list =
    assert (n_trials > 0);
    let metadata = Metadata.of_bjy_file testname in
    let _parse_time, source = Utils.Time.time Lang.Parser.parse_program_from_file testname in
    let test_one (n : int) : t =
      Gc.minor ();
      (* let parse_time, source = Utils.Time.time Lang.Parser.parse_program_from_file testname in *)
      let run_time, (test_result, tape) = Utils.Time.time runtest source in
      (* begin
        match test_result with
        | Concolic.Common.Status.Exhausted_full_tree -> ()
        | _ -> assert false
      end; *)
      let stat_list = tape [] in
      let row =
        { testname
        ; test_result
        ; interp_time = Stat.sum_time Stat.Interp_time stat_list
        ; solve_time = Stat.sum_time Stat.Solve_time stat_list
        ; total_time = run_time
        (* Mtime.Span.add parse_time run_time ignores stats measured total time *)
        ; trial = Number n
        ; metadata }
      in
      row
    in
    let trials = List.init n_trials ~f:test_one in
    let avg_trial =
      List.fold
        trials
        ~init:{
          testname
        ; test_result = Status.Exhausted_pruned_tree (* just arbitrary initial result *)
        ; interp_time = Mtime.Span.zero
        ; solve_time = Mtime.Span.zero
        ; total_time = Mtime.Span.zero
        ; trial = Average
        ; metadata
        }
        ~f:(fun acc x ->
            { acc with (* sum up *)
              test_result = x.test_result (* keeps most recent test result *)
            ; interp_time = Mtime.Span.add acc.interp_time x.interp_time
            ; solve_time = Mtime.Span.add acc.solve_time x.solve_time
            ; total_time = Mtime.Span.add acc.total_time x.total_time
            })
      |> fun r ->
      { r with (* average out *)
        interp_time = Utils.Time.divide_span r.interp_time n_trials
      ; solve_time = Utils.Time.divide_span r.solve_time n_trials
      ; total_time = Utils.Time.divide_span r.total_time n_trials
      }
    in
    trials @ [ avg_trial ]
end

module Result_table = struct
  type t = Report_row.t Latex_tbl.t

  let of_dirs
      ?(avg_only : bool = true)
      (n_trials : int)
      (dirs : Filename.t list)
      (runtest : Lang.Ast.some_program -> Status.Terminal.t * tape)
    : t =
    let open List.Let_syntax in
    { row_module = (module Report_row)
    ; rows =
        dirs
        |> Utils.File_utils.get_all_bjy_files
        |> List.sort ~compare:(fun a b -> String.compare (Filename.basename a) (Filename.basename b))
        >>= Report_row.of_testname n_trials runtest
        |> List.filter ~f:(fun (row : Report_row.t) ->
            not avg_only || match row.trial with Average -> true | _ -> false
          )
        >>| Latex_tbl.Row_or_hline.return
        |> List.cons Latex_tbl.Row_or_hline.Hline
    ; columns =
        let little_space = Latex_tbl.Col_option.Little_space { point_size = 3 } in
        [ [ Latex_tbl.Col_option.Right_align ; Vertical_line_to_right ]
        ; [ little_space ] (* interp time *)
        ; [ little_space ] (* solve time *)
        ; [ little_space ;  Vertical_line_to_right ] (* total time *)
        ; [ little_space ; Vertical_line_to_right ] (* loc *) ]
    }
end

let cbench_args =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ n_trials = value & opt int 50 & info ["trials"] ~doc:"Number of trials"
  and+ dirs = value & opt (list ~sep:' ' dir) [ "test/bjy/oopsla-24-benchmarks-ill-typed" ] & info ["dirs"] ~doc:"Directories to benchmark"
  and+ mode = value & opt (enum ["eager", `Eager ; "deferred", `Deferred]) `Eager & info ["mode"] ~doc:"Mode: eager or deferred. Default is eager"
  and+ hum = value & flag & info ["h"] ~doc:"Show human readable output" in
  n_trials, dirs, mode, hum

let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cbenchmark") @@
  let+ options = Options.cmd_arg_term
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ n_trials, dirs, mode, hum = cbench_args in
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest pgm =
    (* let test_program = 
      match mode with
      | `Eager -> Driver.Eager.test_some_program
      | `Deferred -> Driver.Eager.test_some_program
    in *)
    Driver.Eager.test_some_program
      ~options
      ~do_wrap
      ~do_type_splay
      pgm
  in
  let tbl = Result_table.of_dirs n_trials dirs runtest in
  let times =
    List.filter_map tbl.rows ~f:(function
        | Row row -> Some (Utils.Time.span_to_ms row.total_time)
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
    median
  (* Format.printf "Total interpretation time: %fs\nTotal solving time: %fs\n"
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

let () =
  match Cmdliner.Cmd.eval_value' @@ run () with
  | `Ok _ -> ()
  | `Exit i -> exit i
