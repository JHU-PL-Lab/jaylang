open Batteries
open Jayil
open Jay
open Jay_instrumentation
open Jay_to_jayil_monad
open TranslationMonad
open Lazy_logger
open Translation_context

(** In this module we will translate from jay to jayil in the following order: *
    Desugar let rec, lists, variants, and list/variant patterns * Alphatize
    program again (do this after to allow above to introduce dupes) * Flatten
    jay expressions to jayil expressions * Instrument jayil with type/error
    constriants *)

let debug_transform_jay (trans_name : string)
    (transform : 'a -> Jay_ast.expr_desc m) (e : 'a) : Jay_ast.expr_desc m =
  let%bind e' = transform e in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name
        (Jay_ast_pp.show_expr e'.body)) ;
  return e'

let debug_transform_jayil (trans_name : string)
    (transform : 'a -> Ast.clause list m) (e : 'a) : Ast.clause list m =
  let%bind c_list = transform e in
  let e' = Ast.Expr c_list in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Pp.show_expr e')) ;
  return c_list

let debug_transform_jayil_inst (trans_name : string)
    (transform :
      'a -> Ast.clause list Jay_to_jayil_monad_inst.TranslationMonad.m) (e : 'a)
    : Ast.clause list Jay_to_jayil_monad_inst.TranslationMonad.m =
  let open Jay_to_jayil_monad_inst.TranslationMonad in
  let%bind c_list = transform e in
  let e' = Ast.Expr c_list in
  lazy_logger `debug (fun () ->
      Printf.sprintf "Result of %s:\n%s" trans_name (Pp.show_expr e')) ;
  return c_list

let do_translate ?(is_instrumented : bool = true) context
    (consts : Ast.Var_set.t) (bluejay_instruments : int list)
    (e : Jay_ast.expr_desc) :
    Ast.expr
    * Jay_instrumentation.Jayil_instrumentation_maps.t
    * Jay_to_jayil_maps.t =
  let (e_m_with_info, ctx)
        : Ast.expr Jay_to_jayil_monad_inst.TranslationMonad.m
          * translation_context =
    (* Phase one - translation *)
    (* Step one: Encode lists, variants, match, and let rec
       Step two: Alphatize the expressions
       Step three: Flatten to a-normalized form (Jay -> JayIL)
    *)
    lazy_logger `debug (fun () ->
        Printf.sprintf "Initial program:\n%s" (Jay_ast_pp.show_expr e.body)) ;
    let (translation_result_p1_m : Ast.clause list m) =
      let%bind () = update_instrumented_tags bluejay_instruments in
      return e
      >>= debug_transform_jay "desugaring" Jay_desugar.desugar
      >>= debug_transform_jay "alphatization" Jay_alphatize.alphatize
      >>= debug_transform_jayil "flattening" Jay_to_jayil_flatten.flatten
      >>= debug_transform_jayil "eliminating"
            (Jayil_eliminate_alias.eliminate_alias consts)
    in
    let translation_result_p1, ctx =
      run_verbose context translation_result_p1_m
    in
    (* End of phase one *)
    (* Phase two: Instrumentation *)
    (* In this phase, if the instrumentation flag is set to true, we will add
       first-order instrumentation checks, as well as a first and a final
       variable.
    *)
    let open Jay_to_jayil_monad_inst.TranslationMonad in
    let instrument c_list : Ast.clause list m =
      if is_instrumented
      then Instrumentation.instrument_clauses c_list
      else return c_list
    in
    let add_first_result c_list : Ast.clause list m =
      return c_list >>= Instrumentation.add_first_var
      >>= Instrumentation.add_result_var
    in
    let (translation_result_p2_m : Ast.clause list m) =
      return translation_result_p1
      >>= debug_transform_jayil_inst "instrumentation" instrument
      >>= debug_transform_jayil_inst "adding ~result" add_first_result
    in
    let res = translation_result_p2_m >>= fun m -> return (Ast.Expr m) in
    (res, ctx)
  in
  (* Set up context and run *)
  let jay_inst_map =
    Jay_to_jayil_maps.get_jay_inst_map ctx.tc_jayil_jay_mappings
  in
  let init_ctx_ph2 =
    Jay_to_jayil_monad_inst.new_translation_context_from_jay jay_inst_map
  in
  let ctx' =
    { init_ctx_ph2 with tc_fresh_name_counter = ctx.tc_fresh_name_counter }
  in
  let res = Jay_to_jayil_monad_inst.TranslationMonad.run ctx' e_m_with_info in
  let jayil_jay_maps = ctx.tc_jayil_jay_mappings in
  let inst_maps = ctx'.tc_jayil_instrumentation_mappings in
  (res, inst_maps, jayil_jay_maps)
