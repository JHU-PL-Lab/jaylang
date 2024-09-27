open Core
open Options.Fun.Infix (* expose infix operators *)

module CLog = Dj_common.Log.Export.CLog


module Test_result =
  struct
    type t =
      | Found_abort of Branch.t * Jil_input.t list (* Found an abort at this branch using these inputs *)
      | Type_mismatch of Jayil.Ast.Ident_new.t * Jil_input.t list (* Proposed addition for removing instrumentation *)
      | Exhausted               (* Ran all possible tree paths, and no paths were too deep *)
      | Exhausted_pruned_tree   (* Ran all possible tree paths up to the given max step *)
      | Timeout                 (* total evaluation timeout *)

    let to_string = function
    | Found_abort _ ->         "FOUND_ABORT"
    | Type_mismatch _ ->       "TYPE_MISMATCH"
    | Exhausted ->             "EXHAUSTED"
    | Exhausted_pruned_tree -> "EXHAUSTED_PRUNED_TREE"
    | Timeout ->               "TIMEOUT"

    let merge a b =
      match a, b with
      (* When abort and type mismatch are found, we can conclusively end *)
      | (Found_abort _ as x), _ | _, (Found_abort _ as x)
      | (Type_mismatch _ as x), _ | _, (Type_mismatch _ as x) -> x
      (* Similarly, if we exhausted a tree, then we tried literally everything, so can end *)
      | Exhausted,_  | _, Exhausted -> Exhausted
      (* For the following results, we want to keep the one with the least information to be conservative *)
      | Timeout, _ | _, Timeout -> Timeout
      | Exhausted_pruned_tree, Exhausted_pruned_tree -> Exhausted_pruned_tree

    let of_session_status = function
      | Session.Status.Found_abort (branch, inputs) -> Found_abort (branch, inputs)
      | Type_mismatch inputs -> Type_mismatch (Ident "placeholder branch name", inputs)
      | Exhausted { pruned = true } -> Exhausted_pruned_tree
      | Exhausted { pruned = false } -> Exhausted
      | In_progress _ -> failwith "session status unfinished"

  end

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

let[@landmark] lwt_test_one : (Jayil.Ast.expr, Test_result.t Lwt.t) Options.Fun.a =
  let open Lwt.Syntax in
  Evaluator.lwt_eval
  ^>> fun res_status_lwt ->
    let t0 = Caml_unix.gettimeofday () in
    let+ res_status = res_status_lwt in
    CLog.app (fun m -> m "\nFinished concolic evaluation in %fs.\n" (Caml_unix.gettimeofday () -. t0));
    Test_result.of_session_status res_status

(* runs [lwt_test_one] and catches lwt timeout *)
let test_with_timeout : (Jayil.Ast.expr, Test_result.t) Options.Fun.a =
  lwt_test_one
  ^>> Options.Fun.make
  @@ fun r tr -> 
    try Lwt_main.run tr with
    | Lwt_unix.Timeout ->
      CLog.app (fun m -> m "Quit due to total run timeout in %0.3f seconds.\n" r.global_timeout_sec);
      Test_result.Timeout

let[@landmark] test_expr : (Jayil.Ast.expr, Test_result.t) Options.Fun.a =
  test_with_timeout
  ^>> fun res -> Format.printf "\n%s\n" (Test_result.to_string res); res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test_jil : (string, Test_result.t) Options.Fun.a =
  test_expr
  <<^ Dj_common.File_utils.read_source

let test_bjy : (string, Test_result.t) Options.Fun.a =
  test_expr
  <<^ fun filename ->
    filename
    |> Dj_common.File_utils.read_source_full ~do_instrument:false ~do_wrap:true
    |> Dj_common.Convert.jil_ast_of_convert

let test : (string, Test_result.t) Options.Fun.a =
  Options.Fun.make
  @@ fun r ->
      fun filename ->
        match Core.Filename.split_extension filename with 
        | _, Some "jil" -> Options.Fun.appl test_jil r filename
        | _, Some "bjy" -> Options.Fun.appl test_bjy r filename
        | _ -> failwith "expected jil or bjy file"