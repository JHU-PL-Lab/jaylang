open Core

module To_test = struct end

let parse = Odefa_parser.Parser.parse_string

let read_lines file = file |> In_channel.create |> In_channel.input_lines

let read_src file =
  file |> read_lines |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> not String.(prefix line 1 = "#"))
  |> String.concat ~sep:"\n"

(* treat the path as the group name and filename as the test name *)
let group_all_files dir =
  let rec loop dir =
    let acc_f, acc_p =
      Sys.fold_dir ~init:([], [])
        ~f:(fun (acc_f, acc_p) path ->
          match String.get path 0 with
          (* including "." ".." *)
          | '.' | '_' -> (acc_f, acc_p)
          | _ -> (
              let fullpath = Filename.concat dir path in
              match Sys.is_directory fullpath with
              | `Yes -> (acc_f, loop fullpath @ acc_p)
              | `No -> (fullpath :: acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

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
  let grouped_testfiles = group_all_files path in
  (* Fmt.(pr "%a" Dump.(list (pair string (list string))) grouped_tests); *)
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest.test_case testname `Quick @@ test_unit testname) ))
  in

  Alcotest.run "DBMC" grouped_tests;
  Dbmc.Log.close ();
  ()
