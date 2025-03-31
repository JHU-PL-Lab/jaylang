
open Core
open Lang
open Ast
open Constraints
open Expr
open Pattern
open Translation_tools
open Ast_tools
open Ast_tools.Utils

module LetMonad = struct
  module Binding = struct
    module Ty = struct
      type t = 
        | Untyped
        | Typed of { do_check : bool ; tau : Desugared.t }

      let typed_of_tau_opt ?(do_check : bool = true) (tau_opt : Desugared.t option) : t =
        match tau_opt with
        | Some tau -> Typed { do_check ; tau }
        | None -> Untyped
    end

    type t = 
      { ty   : Ty.t 
      ; var  : Ident.t 
      ; body : Desugared.t
      }

    type a = Constraints.desugared

    let t_to_expr { ty ; var ; body } ~cont =
      match ty with
      | Untyped -> ELet { var ; body ; cont }
      | Typed { tau ; do_check } -> ELetTyped { typed_var = { var ; tau } ; body ; cont ; do_check ; do_wrap = true }
  end

  include Let_builder (Binding)

  (*
    Assign the expression the given name with optional typing.
  *)
  let assign ?(ty : Binding.Ty.t = Untyped) (var : Ident.t) (body : Desugared.t) : unit m =
    tell { ty ; var ; body }
end

let desugar_pgm (names : (module Fresh_names.S)) (pgm : Bluejay.pgm) : Desugared.pgm =
  let module Names = (val names) in
  let open LetMonad in

  let rec desugar (expr : Bluejay.t) : Desugared.t =
    match expr with
    (* Base cases *)
    | (EInt _ | EBool _ | EVar _ | EPick_i | ETypeInt | ETypeBool | EType | ETypeTop | ETypeBottom) as e -> e
    (* Simple propogation *)
    | EBinop { left ; binop ; right } -> begin
      match binop with
      | BAnd -> EIf { cond = desugar left ; true_body = desugar right ; false_body = EBool false }
      | BOr -> EIf { cond = desugar left ; true_body = EBool true ; false_body = desugar right }
      | BDivide | BModulus -> 
        build @@
          let v = Names.fresh_id () in
          let%bind () = assign v @@ desugar right in
          return @@ EIf 
            { cond = EBinop { left = EVar v ; binop = BEqual ; right = EInt 0 }
            ; true_body = EAbort
            ; false_body = EBinop { left = desugar left ; binop ; right = EVar v } }
      | _ -> EBinop { left = desugar left ; binop ; right = desugar right }
    end
    | EIf { cond ; true_body ; false_body } ->
      EIf { cond = desugar cond ; true_body = desugar true_body ; false_body = desugar false_body }
    | ELet { var ; body ; cont } ->
      ELet { var ; body = desugar body ; cont = desugar cont }
    | EAppl { func ; arg } ->
      EAppl { func = desugar func ; arg = desugar arg }
    | EProject { record ; label } ->
      EProject { record = desugar record ; label }
    | ENot e ->
      ENot (desugar e)
    | EFunction { param ; body } ->
      EFunction { param ; body = desugar body }
    | EVariant { label ; payload } ->
      EVariant { label ; payload = desugar payload }
    | ERecord m ->
      ERecord (Map.map m ~f:desugar)
    | ETypeArrow { domain ; codomain } ->
      ETypeArrow { domain = desugar domain ; codomain = desugar codomain }
    | ETypeArrowD { binding ; domain ; codomain } ->
      ETypeArrowD { binding ; domain = desugar domain ; codomain = desugar codomain }
    | ETypeRecord m ->
      ETypeRecord (Map.map m ~f:desugar)
    | ETypeModule m ->
      ETypeModule (List.map m ~f:(fun (label, e) -> label, desugar e))
    | ETypeRefinement { tau ; predicate } ->
      ETypeRefinement { tau = desugar tau ; predicate = desugar predicate }
    | ETypeMu { var ; body } ->
      ETypeMu { var ; body = desugar body }
    | ETypeVariant ls_e ->
      ETypeVariant (List.map ls_e ~f:(fun (label, e) -> label, desugar e))
    | ELetTyped { typed_var = { var ; tau } ; body ; cont ; do_wrap ; do_check } ->
      ELetTyped { typed_var = { var ; tau = desugar tau } ; body = desugar body ; cont = desugar cont ; do_wrap ; do_check }
    | ETypeSingle tau ->
      ETypeSingle (desugar tau)
    (* monads *)
    | ELetBind { var ; body ; cont } ->
      desugar @@ EAppl
        { func = EAppl { func = EVar (Ident "bind") ; arg = body}
        ; arg = EFunction { param = var ; body = cont }
        }
    (* Assert/assume *)
    | EAssert assert_expr ->
      EIf
        { cond = desugar assert_expr
        ; true_body = unit_value
        ; false_body = EAbort
        }
    | EAssume assume_expr ->
      EIf
        { cond = desugar assume_expr
        ; true_body = unit_value
        ; false_body = EDiverge
        }
    (* Dependent records / modules *)
    | EModule stmts -> desugar (pgm_to_module stmts)
    (* Patterns *)
    | EMatch { subject ; patterns } ->
      EMatch { subject = desugar subject ; patterns =
        List.fold_until patterns ~init:[] ~f:(fun acc (pat, e) ->
          match pat with
          | PAny
          | PVariable _ ->
            Stop (List.rev @@
              desugar_pattern pat e
              :: (PVariant
                  { variant_label = Reserved.untouched
                  ; payload_id = Reserved.catchall }
                , EAbort)
              :: acc
            )
          | _ -> Continue (desugar_pattern pat e :: acc)
        ) ~finish:List.rev
      }
    (* Lists *)
    | EList [] ->
      EVariant { label = Reserved.nil ; payload = unit_value }
    | EList ls_e ->
      desugar
      @@ List.fold_right ls_e ~init:(EList []) ~f:(fun e acc ->
        EListCons (e, acc)
      )
    | EListCons (e_hd, e_tl) ->
      EVariant { label = Reserved.cons ; payload =
        ERecord (Parsing_tools.record_of_list
          [ (Reserved.hd, desugar e_hd)
          ; (Reserved.tl, EAppl { func = Desugared_functions.filter_list ; arg = desugar e_tl })
          ]
        )
      }
    | ETypeList e_tau ->
      let t = Names.fresh_id ~suffix:"list_t" () in
      ETypeMu { var = t ; body =
        ETypeVariant
          [ (Reserved.nil_type, unit_type)
          ; (Reserved.cons_type,
            ETypeRecord (Parsing_tools.record_of_list
              [ (Reserved.hd, desugar e_tau)
              ; (Reserved.tl, EVar t)
              ]
            )
          )
          ]
      }
    (* Forall *)
    | ETypeForall { type_variables ; tau } ->
      List.fold_right type_variables ~init:(desugar tau) ~f:(fun alpha acc ->
        ETypeArrowD { binding = alpha ; domain = EType ; codomain = acc }
      )
    (* Intersection type *)
    | ETypeIntersect ls_e ->
      desugar @@
        let x = Names.fresh_id ~suffix:"x_match_type" () in
        let open List.Let_syntax in
        ETypeArrowD
          { binding = x 
          ; domain = ETypeVariant (ls_e >>| fun (label, tau, _) -> label, tau)
          ; codomain = EMatch { subject = EVar x ; patterns = 
              ls_e >>| fun (label, _, tau') ->
                PVariant { variant_label = VariantTypeLabel.to_variant_label label ; payload_id = Reserved.catchall }
                , tau'
            }
          }
    (* Functions *)
    | EMultiArgFunction { params ; body } ->
      abstract_over_ids params (desugar body)
    | ELetFun { func ; cont } -> begin
      let { func_id ; body ; params ; tau_opt } : desugared Function_components.t = funsig_to_components func in
      build @@
        let%bind () =
          assign ~ty:(Binding.Ty.typed_of_tau_opt tau_opt) func_id (
            abstract_over_ids params body
          )
        in
        return (desugar cont)
    end
    | ELetFunRec { funcs ; cont } ->
      (* just roll up the statements and tell it to finish with cont *)
      pgm_to_expr_with_cont (desugar cont)
      @@ desugar_rec_funs_to_stmt_list funcs

  (*
    Breaks a function signature into its id, type (optional), parameter names, and function body, all desugared.
  *)
  and funsig_to_components (fsig : Bluejay.funsig) : desugared Function_components.t =
    Function_components.map ~f:desugar
    @@ Funsig.to_components fsig

  (*
    Desugars list patterns into the associated variants and also returns the
    new body because some projections are necessary.
  *)
  and desugar_pattern (pat : Bluejay.pattern) (e : Bluejay.t) : Desugared.pattern * Desugared.t =
    match pat with
    | PEmptyList ->
      PVariant
        { variant_label = Reserved.nil
        ; payload_id = Reserved.catchall
        }
      , desugar e
    | PDestructList { hd_id ; tl_id } ->
      let r = Names.fresh_id () in
      PVariant
        { variant_label = Reserved.cons
        ; payload_id = r
        }
      , build @@
        let%bind () = assign hd_id (EProject { record = EVar r ; label = Reserved.hd }) in
        let%bind () = assign tl_id (EProject { record = EVar r ; label = Reserved.tl }) in
        return (desugar e)
    | (PAny | PVariable _ | PVariant _) as p -> p, desugar e

  (*
    Turns mutually recursive functions into many statements.
    This is useful for both desugaring statements and expressions.
  *)
  and desugar_rec_funs_to_stmt_list (fsigs : bluejay funsig list) : Desugared.statement list =
    let (>>|) = List.Let_syntax.(>>|) in
    let func_comps = fsigs >>| funsig_to_components in
    let f_names = func_comps >>| fun r -> r.func_id in
    let r = Names.fresh_id ~suffix:"r" () in
    let bodies =
      List.map func_comps ~f:(fun comps ->
        abstract_over_ids f_names @@
          build @@
            let f_id = Names.fresh_id ~suffix:(Ident.to_string comps.func_id) () in
            let%bind () = assign ~ty:(Binding.Ty.typed_of_tau_opt ~do_check:false comps.tau_opt) f_id (
              abstract_over_ids comps.params comps.body
            )
            in
            return (EVar f_id)
      )
    in
    Expr.SUntyped { var = r ; body = appl_list (Desugared_functions.y_n f_names) bodies }
    :: (func_comps >>| fun comps ->
      (* do_check and do_wrap are unused arguments in this case because we don't provide the type *)
      make_stmt ~do_wrap:true ~do_check:true ~tau_opt:None comps.func_id
      @@ proj (EVar r) (RecordLabel.RecordLabel comps.func_id)
    ) @ (func_comps >>| fun comps ->
      make_stmt ~do_wrap:false ~do_check:true ~tau_opt:comps.tau_opt comps.func_id (EVar comps.func_id)
    )

  and desugar_statement (stmt : Bluejay.statement) : Desugared.statement list =
    let open List.Let_syntax in
    match stmt with
    | SUntyped { var ; body } ->
      return @@ Expr.SUntyped { var ; body = desugar body }
    | STyped { typed_var = { var ; tau } ; body ; do_wrap ; do_check } ->
      return @@ Expr.STyped { typed_var = { var ; tau = desugar tau } ; body = desugar body ; do_wrap ; do_check }
    | SFun fsig -> begin
      let { func_id ; body ; params ; tau_opt } : desugared Function_components.t = funsig_to_components fsig in
      let body = abstract_over_ids params body in
      return @@ make_stmt ~do_wrap:true ~do_check:true ~tau_opt func_id body
    end
    | SFunRec fsigs -> desugar_rec_funs_to_stmt_list fsigs

  and make_stmt ~(do_wrap : bool) ~(do_check : bool) ~(tau_opt : Desugared.t option) (var : Ident.t) (body : Desugared.t) : Desugared.statement =
    match tau_opt with
    | None ->
      SUntyped { var ; body }
    | Some tau ->
      STyped { typed_var = { var ; tau } ; body ; do_wrap ; do_check }
  in

  List.bind pgm ~f:desugar_statement