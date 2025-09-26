
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
      ; total_time                  : Time_float.Span.t
      ; trial                       : Trial.t
      ; lines_of_code               : int
      }

    let names =
      [ "Test Name" ; "Total" ; "LOC" ]

    let to_strings x =
      let span_to_ms_string =
            fun span ->
              let fl = Time_float.Span.to_ms span in
              Float.to_string @@ Float.round_decimal fl ~decimal_digits:2
              (* if Float.(fl < 1.)
              then Float.round_decimal fl ~decimal_digits:2
              else Float.round_significant fl ~significant_digits:2 *)
      in
      [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_format.texttt
      ; span_to_ms_string x.total_time
      ; Int.to_string x.lines_of_code ]

    let of_testname (n_trials : int) (testname : Filename.t) : t list =
      assert (n_trials > 0);
      (* Do not measure translation time, and only translate once *)
      let source = 
        Dj_common.Convert.jil_ast_of_convert
        @@ Dj_common.File_utils.read_source_full ~do_wrap:true ~do_instrument:true testname
      in
      let test_one (n : int) : t =
        let t0 = Caml_unix.gettimeofday () in
        let test_result =
          Concolic.Driver.test_expr source ~global_timeout_sec:90.0 ~max_tree_depth:1000 ~global_max_step:1000000 ~random:true
        in
        begin
          match test_result with
          | Exhausted -> ()
          | _ -> assert false (* program was not proven well-typed *)
        end;
        let t1 = Caml_unix.gettimeofday () in
        let row =
          { testname
          ; test_result
          ; total_time = Time_float.Span.of_sec (t1 -. t0)
          ; trial = Number n
          ; lines_of_code = Cloc_lib.count_bjy_lines testname
           }
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
            ; total_time = Time_float.Span.of_sec 0.0
            ; trial = Average
            ; lines_of_code = Cloc_lib.count_bjy_lines testname (* won't even average the remaining fields out. Just pre-calculate it *)
          }
          ~f:(fun acc x ->
            { acc with (* sum up *)
              test_result = Concolic.Driver.Test_result.merge acc.test_result x.test_result (* keeps best test result *)
            ; total_time = Time_float.Span.(acc.total_time + x.total_time)
            })
        |> fun r -> { r with total_time = Time_float.Span.(r.total_time / (Int.to_float n_trials)) } (* average out *)
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
        ; [ little_space ; Vertical_line_to_right ] (* total time *)
        ; [ little_space ; Vertical_line_to_right ] (* loc *) ]
      }
  end

let run dirs =
  let table_s =
    let oc_null = Out_channel.create "/dev/null" in
    Format.set_formatter_out_channel oc_null;
    dirs
    |> Result_table.of_dirs 10
    |> Latex_tbl.show
  in
  Format.set_formatter_out_channel Out_channel.stdout;
  Format.printf "%s\n" table_s

let () =
  (* run [ "test/concolic/bjy/oopsla-24-benchmarks-ill-typed" ]; *)
  (* run [ "test/concolic/bjy/scheme-pldi-2015-ill-typed" ]; *)
  run [ "test/concolic/bjy/oopsla-terminating" ];
  (* run [ "test/concolic/bjy/soft-contract-terminating" ]; *)
  (* run [ "test/concolic/bjy/oopsla-24-tests-ill-typed" ]; *)
  (* run [ "test/concolic/bjy/deep-type-error" ] *)
  (* run [ "test/concolic/bjy/oopsla-24-tests-ill-typed" ; "test/concolic/bjy/sato-bjy-ill-typed" ] *)

