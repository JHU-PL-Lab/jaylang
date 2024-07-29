
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
    type t =
      { testname : Filename.t
      ; test_result : Concolic.Driver.Test_result.t
      ; time_to_only_run_on_jil : Time_float.Span.t
      ; time_to_parse_and_translate : Time_float.Span.t }

    let names =
      [ "Testname" ; "Result" ; "Runtime (ms)" ; "Parsetime (ms)"]

    let to_strings x =
      let span_to_ms_string =
        fun span ->
          span
          |> Time_float.Span.to_ms
          |> Float.to_int
          |> Int.to_string
        in
      [ Filename.basename x.testname |> String.take_while ~f:(Char.(<>) '.') |> Latex_table.texttt
      ; Concolic.Driver.Test_result.to_string x.test_result |> Latex_table.texttt
      ; span_to_ms_string x.time_to_only_run_on_jil
      ; span_to_ms_string x.time_to_parse_and_translate ]

    let of_testname (testname : Filename.t) : t =

      Format.printf "Testing %s\n" testname;
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
      { testname
      ; test_result
      ; time_to_only_run_on_jil = Time_float.Span.of_sec (t2 -. t1)
      ; time_to_parse_and_translate = Time_float.Span.of_sec (t1 -. t0) }
  end

module Result_table =
  struct

    type t = Report_row.t Latex_table.t

    let of_dirs (dirs : Filename.t list) : t =
      let open List.Let_syntax in
      { row_module = (module Report_row)
      ; rows =
        dirs
        |> get_all_files ~filter:(fun fname -> Filename.check_suffix fname ".bjy")
        >>| Report_row.of_testname
        >>| Latex_table.Row_or_hline.return
        |> List.cons Latex_table.Row_or_hline.Hline
      ; col_options = [ Some { right_align = true ; vertical_line_to_right = true } ; Some { right_align = false ; vertical_line_to_right = true } ]
      }
  end

let run dirs =
  dirs
  |> Result_table.of_dirs
  |> Latex_table.show
  |> Format.printf "%s\n"

let () =
  run [ "test/concolic/bjy/scheme-pldi-2015-ill-typed" ];

