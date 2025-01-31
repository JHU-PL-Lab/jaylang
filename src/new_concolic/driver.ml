
open Core
open Options.Fun.Infix (* expose infix operators *)

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

(* runs [lwt_eval] and catches lwt timeout *)
let test_with_timeout : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Fun.a =
  Evaluator.lwt_eval
  ^>> fun res_status ->
    try Lwt_main.run res_status with
    | Lwt_unix.Timeout -> Timeout

let[@landmark] test_expr : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Fun.a =
  test_with_timeout
  ^>> fun res -> Format.printf "\n%s\n" (Status.to_loud_string res); res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test : (string, Status.Terminal.t) Options.Fun.a =
  test_expr
  <<^ (fun s ->
    In_channel.read_all s
    |> Lang.Parse.parse_single_expr_string
    |> Translate.Convert.bjy_to_emb
  )