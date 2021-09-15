open Core

module To_test = struct end

let parse_natodefa = Odefa_natural.On_parse.parse_string

let parse_odefa = Odefa_parser.Parser.parse_string

let read_lines file = file |> In_channel.create |> In_channel.input_lines

let read_src file =
  file |> read_lines |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> not String.(prefix line 1 = "#"))
  |> String.concat ~sep:"\n"

let is_source_ext filename = Filename.check_suffix filename "odefa"
(* || Filename.check_suffix filename "natodefa"  *)

let is_natodefa_source filename = Filename.check_suffix filename "natodefa"

(* treat the path as the group name and filename as the test name *)
let group_all_files dir =
  let rec loop dir =
    let acc_f, acc_p =
      Sys.fold_dir ~init:([], [])
        ~f:(fun (acc_f, acc_p) path ->
          match String.get path 0 with
          | '.' (* including "." ".." *) | '_' -> (acc_f, acc_p)
          | _ -> (
              let fullpath = Filename.concat dir path in
              match Sys.is_directory fullpath with
              | `Yes -> (acc_f, loop fullpath @ acc_p)
              | `No when is_source_ext fullpath -> (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

let int_option_checker : int option Alcotest.testable =
  let eq ii jj =
    match (ii, jj) with
    | Some i, Some j -> i = j
    | None, Some _ -> true
    | _, _ -> failwith "impossible"
  in
  Alcotest.testable Fmt.(Dump.option int) eq

let test_one_file testname () =
  let src_text = read_src testname in
  let src =
    if is_natodefa_source testname then
      parse_natodefa src_text
    else
      parse_odefa src_text
  in
  let config = Dbmc.Top_config.default_config_with_filename testname in
  let inputss = Dbmc.Main.lookup_main ~config src Dbmc.Std.default_target in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  match expectation with
  | None -> Alcotest.(check unit) "unit" () ()
  | Some expectation ->
      let inputs = List.hd_exn inputss in
      let expected_inputs = List.hd_exn expectation.inputs in
      let expected_inputs_ext = List.map ~f:Option.some expected_inputs in
      Alcotest.(check (list int_option_checker))
        "equal" inputs expected_inputs_ext

let () =
  Dbmc.Log.init ();
  let path = "test2" in
  let grouped_testfiles = group_all_files path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest.test_case testname `Quick @@ test_one_file testname) ))
  in

  Alcotest.run "DBMC" grouped_tests;
  Dbmc.Log.close ();
  ()
