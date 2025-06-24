
open Core

module Report_row (* : Latex_table.ROW *) = struct
  module Trial = struct
    type t =
      | Number of int
      | Average
  end

  type t =
    { testname          : Filename.t
    ; test_result       : Concolic.Status.Terminal.t
    ; time_to_interpret : Time_float.Span.t
    ; time_to_solve     : Time_float.Span.t
    ; total_time        : Time_float.Span.t
    ; trial             : Trial.t
    (* ; lines_of_code     : int *) (* not needed because is derived from the testname *)
    ; metadata          : Metadata.t }

  let names =
    [ "Test Name" ; "Interp" ; "Solve" ; "Total" ; "LOC" ]
    @ (List.map Ttag.V2.all ~f:(fun tag ->
        Latex_format.rotate_90
        @@ Ttag.V2.to_name_with_underline tag
      )
    )

  let to_strings x =
    let span_to_ms_string =
      fun span ->
        let fl = Time_float.Span.to_ms span in
        Float.to_string @@
        if Float.(fl < 1.)
        then Float.round_decimal fl ~decimal_digits:2
        else Float.round_significant fl ~significant_digits:2
    in
    [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_format.texttt
    ; span_to_ms_string x.time_to_interpret
    ; span_to_ms_string x.time_to_solve
    ; span_to_ms_string x.total_time
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

  let of_testname (n_trials : int) (runtest : Lang.Ast.Bluejay.pgm -> Concolic.Status.Terminal.t) (testname : Filename.t) : t list =
    assert (n_trials > 0);
    let metadata = Metadata.of_bjy_file testname in
    let test_one (n : int) : t =
      let interp0 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let solve0 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in
      let t0 = Caml_unix.gettimeofday () in
      let source =
        In_channel.read_all testname
        |> Lang.Parse.parse_single_pgm_string
      in
      let test_result = runtest source in
      let t1 = Caml_unix.gettimeofday () in
      let interp1 = Utils.Safe_cell.get Concolic.Evaluator.global_runtime in
      let solve1 = Utils.Safe_cell.get Concolic.Evaluator.global_solvetime in
      let row =
        { testname
        ; test_result
        ; time_to_interpret = Time_float.Span.of_sec (interp1 -. interp0)
        ; time_to_solve = Time_float.Span.of_sec (solve1 -. solve0)
        ; total_time = Time_float.Span.of_sec (t1 -. t0)
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
          ; test_result = Concolic.Status.Exhausted_pruned_tree (* just arbitrary initial result *)
          ; time_to_interpret = Time_float.Span.of_sec 0.0
          ; time_to_solve = Time_float.Span.of_sec 0.0
          ; total_time = Time_float.Span.of_sec 0.0
          ; trial = Average
          ; metadata
        }
        ~f:(fun acc x ->
          { acc with (* sum up *)
            test_result = x.test_result (* keeps most recent test result *)
          ; time_to_interpret = Time_float.Span.(acc.time_to_interpret + x.time_to_interpret)
          ; time_to_solve = Time_float.Span.(acc.time_to_solve + x.time_to_solve)
          ; total_time = Time_float.Span.(acc.total_time + x.total_time)
          })
      |> fun r ->
        { r with (* average out *)
          time_to_interpret = Time_float.Span.(r.time_to_interpret / (Int.to_float n_trials))
        ; time_to_solve = Time_float.Span.(r.time_to_solve / (Int.to_float n_trials))
        ; total_time = Time_float.Span.(r.total_time / (Int.to_float n_trials))
        }
    in
    trials @ [ avg_trial ]
end

module Result_table = struct
  type t = Report_row.t Latex_tbl.t

  let of_dirs ?(avg_only : bool = true) (n_trials : int) (dirs : Filename.t list) (runtest : Lang.Ast.Bluejay.pgm -> Concolic.Status.Terminal.t) : t =
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
      @
      List.init (List.length Ttag.V2.all) ~f:(fun _ -> [ little_space ]) 
    }
end

let cbench_args =
  let open Cmdliner.Term.Syntax in
  let open Cmdliner.Arg in
  let+ n_trials = value & opt int 50 & info ["trials"] ~doc:"Number of trials"
  and+ dirs = value & opt (list ~sep:' ' dir) [ "test/bjy/oopsla-24-benchmarks-ill-typed" ] & info ["dirs"] ~doc:"Directories to benchmark" in
  n_trials, dirs

let run () =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  Cmd.v (Cmd.info "cbenchmark") @@
  let+ concolic_args = Concolic.Options.cmd_arg_term
  and+ n_trials, dirs = cbench_args in
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let runtest pgm =
    Concolic.Options.Arrow.appl
      Concolic.Driver.test_bjy
      concolic_args
      pgm
      ~do_wrap:true        (* always wrap during benchmarking *)
      ~do_type_splay:false (* never type splay during benchmarking *)
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
  |> Latex_tbl.show
  |> Format.printf "%s\n";
  Format.printf "Mean time of all tests: %fms\nMedian time of all tests: %fms\n" 
    mean 
    median;
  Format.printf "Total interpretation time: %fs\nTotal solving time: %fs\n"
    (Utils.Safe_cell.get Concolic.Evaluator.global_runtime) 
    (Utils.Safe_cell.get Concolic.Evaluator.global_solvetime)

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

