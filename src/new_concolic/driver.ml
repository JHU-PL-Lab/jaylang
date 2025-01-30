
open Core
open Options.Fun.Infix (* expose infix operators *)

(* module CLog = Dj_common.Log.Export.CLog *)

module Test_result = struct
  type t =
    | Found_abort of Input.t list (* Found an abort using these inputs *)
    | Type_mismatch of Input.t list (* Found a type mismatch using these inputs *)
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
    | Session.Status.Found_abort inputs -> Found_abort inputs
    | Type_mismatch inputs -> Type_mismatch inputs
    | Exhausted { pruned = true } -> Exhausted_pruned_tree
    | Exhausted { pruned = false } -> Exhausted
    | In_progress _ -> failwith "session status unfinished"

  let is_error_found = function
    | Timeout
    | Exhausted_pruned_tree
    | Exhausted -> false
    | Found_abort _
    | Type_mismatch _ -> true

end

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

let[@landmark] lwt_test_one : (Lang.Ast.Embedded.t, Test_result.t Lwt.t) Options.Fun.a =
  let open Lwt.Syntax in
  Evaluator.lwt_eval
  ^>> fun res_status_lwt ->
    (* let t0 = Caml_unix.gettimeofday () in *)
    let+ res_status = res_status_lwt in
    (* CLog.app (fun m -> m "\nFinished concolic evaluation in %fs.\n" (Caml_unix.gettimeofday () -. t0)); *)
    Test_result.of_session_status res_status

(* runs [lwt_test_one] and catches lwt timeout *)
let test_with_timeout : (Lang.Ast.Embedded.t, Test_result.t) Options.Fun.a =
  lwt_test_one
  ^>> Options.Fun.make
  @@ fun _r tr -> 
    try Lwt_main.run tr with
    | Lwt_unix.Timeout ->
      (* CLog.app (fun m -> m "Quit due to total run timeout in %0.3f seconds.\n" r.global_timeout_sec); *)
      Test_result.Timeout

let[@landmark] test_expr : (Lang.Ast.Embedded.t, Test_result.t) Options.Fun.a =
  test_with_timeout
  ^>> fun res -> Format.printf "\n%s\n" (Test_result.to_string res); res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test : (string, Test_result.t) Options.Fun.a =
  test_expr
  <<^ (fun s ->
    In_channel.read_all s
    |> Lang.Parse.parse_single_expr_string
    |> Translate.Convert.bjy_to_emb
  )