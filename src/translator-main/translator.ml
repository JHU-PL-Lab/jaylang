open Batteries
(** A front-end for the parser library. *)

open Odefa_ast
open Odefa_natural
open Ast_pp
open Ast_tools
open Translator_options

(** Removes from a variable any special symbols introduced by the translation
    process which are not parseable identifiers. *)
let purge_special_symbols (x : Ast.Var.t) : Ast.Var.t =
  let (Ast.Var (Ast.Ident s, fs)) = x in
  let s' =
    s
    |> String.replace_chars (fun c ->
           match c with '~' -> "___" | _ -> String.of_char c)
  in
  Ast.Var (Ast.Ident s', fs)

let main () : unit =
  let options = parse_args () in
  match options.ta_mode with
  | Odefa_natural_to_odefa ->
      let on_expr = On_parse.parse_program IO.stdin in
      let odefa_expr = On_to_odefa.translate on_expr |> fst in
      let result_expr =
        if options.ta_parseable
        then map_expr_vars purge_special_symbols odefa_expr
        else odefa_expr
      in
      let expr_string = show_expr result_expr in
      print_endline expr_string
  | Scheme_to_odefa_natural ->
      raise @@ Jhupllib.Utils.Not_yet_implemented "scheme-to-odefa-natural"
;;

main ()
