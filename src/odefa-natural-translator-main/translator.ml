(**
   A front-end for the parser library.
*)

open Batteries;;

open Odefa_natural;;

let main () =
  let on_expr = On_parse.parse_program IO.stdin in
  let odefa_expr = On_to_odefa.translate on_expr in
  odefa_expr
;;

main ();;
