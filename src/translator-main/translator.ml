open Jayil
open Ast_pp
open Translator_options
open Dj_common

(*
open Ast_tools

  (** Removes from a variable any special symbols introduced by the translation
      process which are not parseable identifiers. *)
   let purge_special_symbols_jayil (x : Ast.Var.t) : Ast.Var.t =
     let (Ast.Var (Ast.Ident s, fs)) = x in
     let s' =
       s
       |> String.replace_chars (fun c ->
              match c with '~' -> "___" | _ -> String.of_char c)
     in
     Ast.Var (Ast.Ident s', fs) *)
(* let result_expr = map_expr_vars purge_special_symbols_jayil post_inst_ast in
   result_expr *)

let bluejay_to_jay () =
  let bluejay_ast =
    Bluejay.Bluejay_ast.new_expr_desc
    @@ File_utils.parse_bluejay In_channel.stdin
  in
  Convert.bluejay_edesc_to_jay bluejay_ast

let main () : unit =
  let options = parse_args () in
  let is_instrumented = options.ta_instrument in
  match options.ta_mode with
  | Bluejay_to_jayil ->
      (* let jay_edesc = bluejay_to_jay () in *)
      let bluejay_ast =
        Bluejay.Bluejay_ast.new_expr_desc
        @@ File_utils.parse_bluejay In_channel.stdin
      in
      let init_consts =
        Bluejay.Bluejay_ast_tools.defined_vars_of_expr_desc bluejay_ast
        |> Bluejay.Bluejay_ast.Ident_set.to_list
        |> List.map (fun x -> Jayil.Ast.Var (x, None))
        |> Jayil.Ast.Var_set.of_list
      in
      let jayil_ast =
        Convert.bluejay_edesc_to_jayil ~is_instrumented bluejay_ast
          ~consts:init_consts
      in
      let expr_string = show_expr jayil_ast in
      print_endline expr_string
  | Bluejay_to_jay ->
      let jay_ast = bluejay_to_jay () in
      let expr_string = Jay.Jay_ast_pp.show_expr_desc jay_ast in
      print_endline expr_string
  | Jay_to_jayil ->
      let jay_ast =
        Jay.Jay_ast.new_expr_desc @@ File_utils.parse_jay In_channel.stdin
      in
      let consts =
        Jay.Jay_ast_tools.defined_vars_of_expr_desc jay_ast
        |> Jay.Jay_ast.Ident_set.to_list
        |> List.map (fun x -> Jayil.Ast.Var (x, None))
        |> Jayil.Ast.Var_set.of_list
      in
      let jayil_ast =
        Convert.jay_edesc_to_jayil ~is_instrumented jay_ast ~consts
      in
      Fmt.pr "%a" Jayil.Pp.expr jayil_ast
  | Scheme_to_jay -> failwith "scheme-to-jay"

let () = main ()
