open Core
open Dbmc

let testing_step = 1

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
              | `No when File_util.is_odefa_ext fullpath ->
                  (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

let int_option_checker : int option Alcotest.testable =
  let eq ii jj = match (ii, jj) with Some i, Some j -> i = j | _, _ -> true in
  Alcotest.testable Fmt.(Dump.option int) eq

let test_one_file testname () =
  let src = File_util.read_source testname in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  let config : Global_config.t =
    let filename = testname in
    let steps = testing_step in
    let timeout = Some (Time.Span.of_int_sec 10) in
    let default_config = Global_config.default_config in
    { default_config with filename; steps; timeout }
  in
  Dbmc.Log.init config ;
  match expectation with
  | None ->
      let _ = Main.main ~config src in
      Alcotest.(check unit) "unit" () ()
  | Some expectations ->
      List.iter expectations ~f:(fun expectation ->
          let config = { config with target = Id.Ident expectation.target } in
          let inputss = Main.main ~config src in
          match List.hd inputss with
          | Some inputs ->
              let expected_inputs = List.hd_exn expectation.inputs in
              Alcotest.(check (list int_option_checker))
                "equal" inputs expected_inputs ;
              Alcotest.(check (list int_option_checker))
                "equal" inputs expected_inputs
          | None ->
              Alcotest.(check int) "equal" 0 (List.length expectation.inputs))

exception Timeout

let sigalrm_handler = Caml.Sys.Signal_handle (fun _ -> raise Timeout)

(* TODO: it works strangely. Maybe because this timeout won't break a Lwt.t. *)
let timeout f arg time =
  let old_behavior = Caml.Sys.signal Caml.Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Caml.Sys.set_signal Caml.Sys.sigalrm old_behavior in
  ignore (Unix.alarm time) ;
  try
    let res = f arg in
    reset_sigalrm () ;
    res
  with exc ->
    reset_sigalrm () ;
    raise exc

let test_one_file_lwt testname _switch () =
  try%lwt
    Lwt_unix.with_timeout 5.0 (fun () -> Lwt.return (test_one_file testname ()))
  with Lwt_unix.Timeout -> Lwt.return (Alcotest.fail "timeout")

(* let test_one_file_lwt testname _switch () =
   Lwt.return (test_one_file testname ()) *)

let test_one_file_timed testname () = test_one_file testname ()
(* try timeout (test_one_file testname) () 5 with Timeout -> raise Timeout *)

let () =
  let path = "test-sources" in
  let grouped_testfiles = group_all_files path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest_lwt.test_case testname `Quick
              @@ test_one_file_lwt testname
              (* Alcotest.test_case testname `Quick @@ test_one_file_timed testname *))
        ))
  in

  (* Alcotest.run "DBMC" grouped_tests; *)
  Lwt_main.run @@ Alcotest_lwt.run "DBMC" grouped_tests ;
  Dbmc.Log.close () ;
  ()
