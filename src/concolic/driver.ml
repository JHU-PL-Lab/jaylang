
open Core
open Options.Arrow.Infix (* expose infix operators *)

(*
  ----------------------
  TESTING BY EXPRESSIONS   
  ----------------------
*)

(* runs [lwt_eval] and catches lwt timeout *)
let test_with_timeout : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Arrow.t =
  Evaluator.lwt_eval
  >>^ fun res_status ->
    try Lwt_main.run res_status with
    | Lwt_unix.Timeout -> Timeout

let[@landmark] test_expr : (Lang.Ast.Embedded.t, Status.Terminal.t) Options.Arrow.t =
  test_with_timeout
  >>^ fun res -> Format.printf "\n%s\n" (Status.to_loud_string res); res

(*
  -------------------
  TESTING BY FILENAME
  -------------------
*)

let test : (string, do_wrap:bool -> Status.Terminal.t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun r -> fun s -> fun ~do_wrap ->
    In_channel.read_all s
    |> Lang.Parse.parse_single_expr_string
    |> Translate.Convert.bjy_to_emb ~do_wrap
    |> Options.Arrow.appl test_expr r