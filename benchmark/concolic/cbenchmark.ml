
open Core

module Report_row (* : Latex_table.ROW *) =
  struct
    module Trial =
      struct
        type t =
          | Number of int
          | Average
      end

    type t =
      { testname                    : Filename.t
      ; test_result                 : Concolic.Status.Terminal.t
      ; time_to_only_run_on_jil     : Time_float.Span.t
      ; time_to_parse_and_translate : Time_float.Span.t
      ; total_time                  : Time_float.Span.t
      ; trial                       : Trial.t
      ; lines_of_code               : int
      ; metadata                    : Metadata.t }

    let names =
      [ "Test Name" ; "Run" ; "Transl" ; "Total" ; "LOC" ]
      @ (List.map Ttag.all ~f:(fun tag ->
          Latex_format.rotate_90
          @@ Ttag.to_string_with_underline tag
        )
      )

    let to_strings x =
      let span_to_ms_string =
        fun span ->
          span
          |> Time_float.Span.to_ms
          |> Float.round_up
          |> Float.to_int
          |> Int.to_string
      in
      [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_format.texttt
      ; span_to_ms_string x.time_to_only_run_on_jil
      ; span_to_ms_string x.time_to_parse_and_translate
      ; span_to_ms_string x.total_time
      ; Int.to_string x.lines_of_code ]
      @ (
        match Metadata.tags_of_t x.metadata with
        | `Sorted_list ls ->
          List.map ls ~f:(function
            | `Absent -> "--"
            | `Feature tag -> Ttag.to_string_short tag
            | `Reason tag ->
              Latex_format.red
              @@ Ttag.to_string_short tag
            )
      )

    let of_testname (n_trials : int) (testname : Filename.t) : t list =
      assert (n_trials > 0);
      let metadata = Metadata.of_bjy_file testname in
      let test_one (n : int) : t =
        let t0 = Caml_unix.gettimeofday () in
        let source =
          In_channel.read_all testname
          |> Lang.Parse.parse_single_pgm_string
        in
        let t1 = Caml_unix.gettimeofday () in
        let test_result =
          Concolic.Driver.test_bjy source ~global_timeout_sec:90.0 ~do_wrap:true ~in_parallel:false (* parallel computation off by default *)
        in
        let t2 = Caml_unix.gettimeofday () in
        let row =
          { testname
          ; test_result
          ; time_to_only_run_on_jil = Time_float.Span.of_sec (t2 -. t1)
          ; time_to_parse_and_translate = Time_float.Span.of_sec (t1 -. t0)
          ; total_time = Time_float.Span.of_sec (t2 -. t0)
          ; trial = Number n
          ; lines_of_code = Utils.Cloc_lib.count_bjy_lines testname
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
            ; time_to_only_run_on_jil = Time_float.Span.of_sec 0.0
            ; time_to_parse_and_translate = Time_float.Span.of_sec 0.0
            ; total_time = Time_float.Span.of_sec 0.0
            ; trial = Average
            ; lines_of_code = Utils.Cloc_lib.count_bjy_lines testname (* won't even average the remaining fields out. Just pre-calculate it *)
            ; metadata
          }
          ~f:(fun acc x ->
            { acc with (* sum up *)
              test_result = x.test_result (* keeps most recent test result *)
            ; time_to_only_run_on_jil = Time_float.Span.(acc.time_to_only_run_on_jil + x.time_to_only_run_on_jil)
            ; time_to_parse_and_translate = Time_float.Span.(acc.time_to_parse_and_translate + x.time_to_parse_and_translate)
            ; total_time = Time_float.Span.(acc.total_time + x.total_time)
            })
        |> fun r ->
          { r with (* average out *)
            time_to_only_run_on_jil = Time_float.Span.(r.time_to_only_run_on_jil / (Int.to_float n_trials))
          ; time_to_parse_and_translate = Time_float.Span.(r.time_to_parse_and_translate / (Int.to_float n_trials))
          ; total_time = Time_float.Span.(r.total_time / (Int.to_float n_trials))
          }
      in
      trials @ [ avg_trial ]
  end

module Result_table =
  struct
    type t = Report_row.t Latex_tbl.t

    let of_dirs ?(avg_only : bool = true)  (n_trials : int) (dirs : Filename.t list) : t =
      let open List.Let_syntax in
      { row_module = (module Report_row)
      ; rows =
        dirs
        |> Utils.File_utils.get_all_bjy_files
        |> List.sort ~compare:(fun a b -> String.compare (Filename.basename a) (Filename.basename b))
        >>= Report_row.of_testname n_trials
        |> List.filter ~f:(fun (row : Report_row.t) ->
          not avg_only || match row.trial with Average -> true | _ -> false
          )
        >>| Latex_tbl.Row_or_hline.return
        |> List.cons Latex_tbl.Row_or_hline.Hline
      ; columns =
        let little_space = Latex_tbl.Col_option.Little_space { point_size = 3 } in
        [ [ Latex_tbl.Col_option.Right_align ; Vertical_line_to_right ]
        ; [ little_space ] (* run time *)
        ; [ little_space ] (* translation time *)
        ; [ little_space ;  Vertical_line_to_right ] (* total time *)
        ; [ little_space ; Vertical_line_to_right ] (* loc *) ]
        @
        List.init (List.length Ttag.all) ~f:(fun _ -> [ little_space ]) 
      }
  end

let run dirs =
  let oc_null = Out_channel.create "/dev/null" in
  Format.set_formatter_out_channel oc_null;
  let tbl = Result_table.of_dirs 10 dirs in
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
  Format.printf "Mean time of all tests: %fms\nMedian time of all tests: %fms\n" mean median

let () =
  run [ 
    "test/bjy/oopsla-24-benchmarks-ill-typed";
    (* "test/bjy/scheme-pldi-2015-ill-typed"; *)
    (* "test/bjy/deep-type-error"; *)
    (* "test/bjy/oopsla-24-tests-ill-typed"; "test/bjy/sato-bjy-ill-typed"; *)
    (* "test/bjy/interp-ill-typed"; *)
  ]

