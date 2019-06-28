(**
   A front-end for the parser library.
*)
open Batteries;;

open Odefa_ast;;
open Odefa_natural;;

open Ast_pp;;

let main () : unit =
  (* Jhupllib.Logger_utils.set_default_logging_level `trace; *)
  let on_expr = On_parse.parse_program IO.stdin in
  let odefa_expr = On_to_odefa.translate on_expr in
  let expr_string = show_expr odefa_expr in
  print_endline expr_string
;;

main ();;
