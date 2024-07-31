
open Core

let get_all_files ?(filter : Filename.t -> bool = fun _ -> true) (dirs : Filename.t list) : Filename.t list =
  let rec loop outlist = function
    | [] -> outlist
    | f :: fs -> begin
      match Sys_unix.is_directory f with
      | `Yes ->
          f
          |> Sys_unix.ls_dir
          |> List.map ~f:(( ^ ) (f ^ "/"))
          |> List.append fs
          |> loop outlist
      | _ when filter f -> loop (f :: outlist) fs
      | _ -> loop outlist fs
    end
  in
  loop [] dirs

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
      ; trial                       : Trial.t
      ; lines_of_code               : int }

    let names =
      [ "Testname" ; "Result" ; "Runtime (ms)" ; "Translation time (ms)" ; "Trial" ; "LOC" ]

    let to_strings x =
      let span_to_ms_string =
        fun span ->
          span
          |> Time_float.Span.to_ms
          |> Float.round_up
          |> Float.to_int
          |> Int.to_string
      in
      [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_table.texttt
      ; Concolic.Driver.Test_result.to_string x.test_result |> Latex_table.texttt
      ; span_to_ms_string x.time_to_only_run_on_jil
      ; span_to_ms_string x.time_to_parse_and_translate
      ; (match x.trial with Number i -> Int.to_string i | Average -> "avg")
      ; Int.to_string x.lines_of_code ]

    (* Duplicate code from src-test/concolic/bjy_cloc.ml *)
    let cloc filename =
      In_channel.with_file filename ~f:(fun file ->
          In_channel.fold_lines file ~init:0 ~f:(fun count line ->
              if String.is_empty (String.strip line) || String.is_prefix line ~prefix:"#" then
                count
              else
                count + 1))

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
          Concolic.Driver.test_expr source ~quit_on_abort:true ~global_timeout_sec:90.0
        in
        let t2 = Caml_unix.gettimeofday () in
        let row =
          { testname
          ; test_result
          ; time_to_only_run_on_jil = Time_float.Span.of_sec (t2 -. t1)
          ; time_to_parse_and_translate = Time_float.Span.of_sec (t1 -. t0)
          ; trial = Number n
          ; lines_of_code = cloc testname }
        in
        Format.printf "Tested %s -- %d : %d ms\n" testname n (Time_float.Span.to_ms row.time_to_only_run_on_jil |> Float.to_int);
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
            ; trial = Average
            ; lines_of_code = cloc testname (* won't even average this out. Just pre-calculate it *)
          }
          ~f:(fun acc x ->
            { acc with
              test_result = Concolic.Driver.Test_result.merge acc.test_result x.test_result (* keeps best test result *)
            ; time_to_only_run_on_jil = Time_float.Span.(acc.time_to_only_run_on_jil + x.time_to_only_run_on_jil)
            ; time_to_parse_and_translate = Time_float.Span.(acc.time_to_parse_and_translate + x.time_to_parse_and_translate)
            })
        |> fun r ->
          { r with
            time_to_only_run_on_jil = Time_float.Span.(r.time_to_only_run_on_jil / (Int.to_float n_trials))
          ; time_to_parse_and_translate = Time_float.Span.(r.time_to_parse_and_translate / (Int.to_float n_trials))
          }
      in
      Format.printf "Tested %s -- avg : %d ms\n" testname (Time_float.Span.to_ms avg_trial.time_to_only_run_on_jil |> Float.to_int);
      trials @ [ avg_trial ]
  end

module Result_table =
  struct

    type t = Report_row.t Latex_table.t

    (* TODO: add show_result so that we don't have the see the `FOUND_ABORT` *)
    let of_dirs ?(avg_only : bool = true) (*?(show_result : bool = false)*) (n_trials : int) (dirs : Filename.t list) : t =
      let open List.Let_syntax in
      { row_module = (module Report_row)
      ; rows =
        dirs
        |> get_all_files ~filter:(fun fname -> Filename.check_suffix fname ".bjy")
        |> List.sort ~compare:String.compare
        >>= Report_row.of_testname n_trials
        |> List.filter ~f:(fun row ->
          not avg_only || match row.trial with Average -> true | _ -> false
          )
        >>| Latex_table.Row_or_hline.return
        |> List.cons Latex_table.Row_or_hline.Hline
      ; col_options = [ Some { right_align = true ; vertical_line_to_right = true } ; Some { right_align = false ; vertical_line_to_right = true } ]
      }
  end

let run dirs =
  dirs
  |> Result_table.of_dirs 10 (* run 10 trials of each test *)
  |> Latex_table.show
  |> Format.printf "%s\n"

let () =
  (* run [ "test/concolic/bjy/scheme-pldi-2015-ill-typed" ]; *)
  run [ "test/concolic/bjy/oopsla-24a-additional-tests-ill-typed" ];
  (* run [ "test/concolic/bjy/sato-bjy-ill-typed" ] *)

