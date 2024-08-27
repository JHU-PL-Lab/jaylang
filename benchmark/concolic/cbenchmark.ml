
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
      ; test_result                 : Concolic.Driver.Test_result.t
      ; time_to_only_run_on_jil     : Time_float.Span.t
      ; time_to_parse_and_translate : Time_float.Span.t
      ; total_time                  : Time_float.Span.t
      ; trial                       : Trial.t
      ; lines_of_code               : int
      ; features                    : Ttag.t list
      ; reasons                     : Ttag.t list }

    let names =
      [ "Test Name" ; "Run" ; "Transl" ; "Total" ; "LOC" ]
      @ (List.map Ttag.all ~f:(fun tag ->
          Format.sprintf "\\rot{%s}" (* assume \rot has been defined to rotate column headers 90 degrees *)
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
        Ttag.all
        |> List.map ~f:(fun tag ->
          if List.mem x.reasons tag ~equal:Ttag.equal
          then
            (* sanity check that reasons is subset of features *)
            let _ = assert (List.mem x.features tag ~equal:Ttag.equal) in
            Format.sprintf "\\red{%s}" (* assume \red{%s} is \textcolor{red}{%s} *)
            @@ Ttag.to_string_short tag
          else
            if List.mem x.features tag ~equal:Ttag.equal
            then Ttag.to_string_short tag
            else "--"
        )
      )

    let of_testname (n_trials : int) (testname : Filename.t) : t list =
      assert (n_trials > 0);
      let test_one (n : int) : t =
        let t0 = Caml_unix.gettimeofday () in
        let source =  
          Dj_common.Convert.jil_ast_of_convert
          @@ Dj_common.File_utils.read_source_full ~do_wrap:true ~do_instrument:true testname
        in
        let t1 = Caml_unix.gettimeofday () in
        let test_result =
          Concolic.Driver.test_expr source ~global_timeout_sec:90.0
        in
        let t2 = Caml_unix.gettimeofday () in
        let row =
          { testname
          ; test_result
          ; time_to_only_run_on_jil = Time_float.Span.of_sec (t2 -. t1)
          ; time_to_parse_and_translate = Time_float.Span.of_sec (t1 -. t0)
          ; total_time = Time_float.Span.of_sec (t2 -. t0)
          ; trial = Number n
          ; lines_of_code = Cloc_lib.count_bjy_lines testname
          ; features = Ttag.features testname
          ; reasons = Ttag.reasons testname }
        in
        row
      in
      let trials = List.init n_trials ~f:test_one in
      let avg_trial =
        List.fold
          trials
          ~init:{
            testname
            ; test_result = Concolic.Driver.Test_result.Exhausted_pruned_tree
            ; time_to_only_run_on_jil = Time_float.Span.of_sec 0.0
            ; time_to_parse_and_translate = Time_float.Span.of_sec 0.0
            ; total_time = Time_float.Span.of_sec 0.0
            ; trial = Average
            ; lines_of_code = Cloc_lib.count_bjy_lines testname (* won't even average the remaining fields out. Just pre-calculate it *)
            ; features = Ttag.features testname
            ; reasons = Ttag.reasons testname
          }
          ~f:(fun acc x ->
            { acc with (* sum up *)
              test_result = Concolic.Driver.Test_result.merge acc.test_result x.test_result (* keeps best test result *)
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
      Format.printf "Tested %s -- avg : %d ms\n" testname (Time_float.Span.to_ms avg_trial.time_to_only_run_on_jil |> Float.to_int);
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
        |> Ttag.get_all_files ~filter:(Fn.flip Filename.check_suffix ".bjy")
        |> List.sort ~compare:(fun a b -> String.compare (Filename.basename a) (Filename.basename b))
        >>= Report_row.of_testname n_trials
        |> List.filter ~f:(fun row ->
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
  dirs
  |> Result_table.of_dirs 2
  |> Latex_tbl.show
  |> Format.printf "%s\n"

let () =
  run [ "test/concolic/bjy/scheme-pldi-2015-ill-typed" ];
  (* run [ "test/concolic/bjy/oopsla-24-tests-ill-typed" ]; *)
  (* run [ "test/concolic/bjy/oopsla-24-benchmarks-ill-typed" ]; *)
  (* run [ "test/concolic/bjy/deep-type-error" ] *)
  (* run [ "test/concolic/bjy/oopsla-24-tests-ill-typed" ; "test/concolic/bjy/sato-bjy-ill-typed" ] *)

