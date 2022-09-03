open Batteries
(** A front-end for the parser library. *)

open Jayil
open Jay
open Ast_pp
open Ast_tools
open Translator_options

(** Removes from a variable any special symbols introduced by the translation
    process which are not parseable identifiers. *)
let purge_special_symbols_jayil (x : Ast.Var.t) : Ast.Var.t =
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
  | Bluejay_to_jayil ->
    let bluejay_ast = 
      Bluejay.Bluejay_ast.new_expr_desc @@ Bluejay.Bluejay_parse.parse_program IO.stdin 
    in
    let bluejay_ast_internal =
      Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
    in
    let core_ast, _ton_on_maps =
        (* Typed -> Untyped *)
        Bluejay.Bluejay_to_jay.transform_bluejay bluejay_ast_internal.body
    in
    let translation_ctx = 
      if options.ta_parseable
      then
          Jay.Jay_to_jayil_monad.new_translation_context ~is_natodefa:true
          ~suffix:"___" ()
      else
          Jay.Jay_to_jayil_monad.new_translation_context ()
    in
    let jay_ast = Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast in
    let is_instrumented = options.ta_instrument in
    let post_inst_ast, _odefa_inst_maps, _on_odefa_maps =
      Jay_to_jayil.translate ~is_instrumented:is_instrumented ~translation_context:(Some translation_ctx) jay_ast
    in
    let result_expr =
      if options.ta_parseable
      then map_expr_vars purge_special_symbols_jayil post_inst_ast
      else post_inst_ast
    in
    let expr_string = show_expr result_expr in
    print_endline expr_string
  | Bluejay_to_jay ->
    let bluejay_ast = 
      Bluejay.Bluejay_ast.new_expr_desc @@ Bluejay.Bluejay_parse.parse_program IO.stdin 
    in
    let bluejay_ast_internal =
      Bluejay.Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
    in
    let core_ast, _ton_on_maps =
        Bluejay.Bluejay_to_jay.transform_bluejay bluejay_ast_internal.body
    in
    let jay_ast = Bluejay.Bluejay_ast_internal.to_jay_expr_desc core_ast in
    let expr_string = Jay.Jay_ast_pp.show_expr_desc jay_ast in
    print_endline expr_string
  | Jay_to_jayil ->
    let translation_ctx = 
      if options.ta_parseable
      then
          Jay.Jay_to_jayil_monad.new_translation_context ~is_natodefa:true
          ~suffix:"___" ()
      else
          Jay.Jay_to_jayil_monad.new_translation_context ()
    in
    let jay_ast = 
      Jay_ast.new_expr_desc @@ Jay_parse.parse_program IO.stdin 
    in
    let is_instrumented = options.ta_instrument in
    let post_inst_ast, _odefa_inst_maps, _on_odefa_maps =
      Jay_to_jayil.translate ~is_instrumented:is_instrumented ~translation_context:(Some translation_ctx) jay_ast
    in
    let result_expr =
      if options.ta_parseable
      then map_expr_vars purge_special_symbols_jayil post_inst_ast
      else post_inst_ast
    in
    let expr_string = show_expr result_expr in
    print_endline expr_string
  | Scheme_to_jay ->
      raise @@ Jhupllib.Utils.Not_yet_implemented "scheme-to-jay"
;;

main ()
