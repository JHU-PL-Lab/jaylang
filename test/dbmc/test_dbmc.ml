open Core

module To_test = struct end

let parse = Odefa_parser.Parser.parse_string

let read_lines file = file |> In_channel.create |> In_channel.input_lines

let read_src file =
  file |> read_lines |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> not String.(prefix line 1 = "#"))
  |> String.concat ~sep:"\n"

let all_files dir =
  let rec loop dir =
    let acc_f, acc_p =
      Sys.fold_dir ~init:([], [])
        ~f:(fun (acc_f, acc_p) path ->
          match String.get path 0 with
          | '.' | '_' -> (acc_f, acc_p)
          | _ -> (
              let path = Filename.concat dir path in
              match Sys.is_directory path with
              | `Yes -> (acc_f, loop path @ acc_p)
              | `No -> (path :: acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    acc_p @ acc_f
  in
  List.rev (loop dir)

let test_one_file testname =
  let src_text = read_src testname in
  let src = parse src_text in
  (* print_endline @@ Odefa_ast.Ast_pp.show_expr src; *)
  let inputs = Dbmc.Main.lookup_main src Dbmc.Std.default_target in
  List.hd_exn inputs

let test_unit testname () =
  Alcotest.(check (list int)) "equal" [] (test_one_file testname)

let () =
  Dbmc.Log.init ();

  print_endline @@ Sys.getcwd ();
  (* let path = "../../../../test2" in *)
  let path = "test2" in
  let all_tests = all_files path in
  Fmt.(pr "%a" (Dump.list string) all_tests);

  Alcotest.run "Test_file"
    [
      ( "dummy",
        List.mapi all_tests ~f:(fun _i testname ->
            Alcotest.test_case testname `Quick @@ test_unit testname) );
    ];
  Dbmc.Log.close ();
  ()
