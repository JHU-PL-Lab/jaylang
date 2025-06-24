
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
      ; defn : Desugared.t
      }

    type a = Constraints.desugared

    let t_to_expr { ty ; var ; defn } ~body =
      match ty with
      | Untyped -> ELet { var ; defn ; body }
      | Typed { tau ; do_check } -> ELetTyped { typed_var = { var ; tau } ; defn ; body ; do_check ; do_wrap = true }
  end

  include Let_builder (Binding)

  (*
    Assign the expression the given name with optional typing.
  *)
  let assign ?(ty : Binding.Ty.t = Untyped) (var : Ident.t) (defn : Desugared.t) : unit m =
    tell { ty ; var ; defn }
end

let desugar_pgm (names : (module Fresh_names.S)) (pgm : Bluejay.pgm) ~(do_type_splay : bool) : Desugared.pgm =
  let module Names = (val names) in
  let open LetMonad in

  if do_type_splay then assert (Bluejay.is_deterministic_pgm pgm);

  let rec desugar (expr : Bluejay.t) : Desugared.t =
    match expr with
    (* Base cases *)
    | (EInt _ | EBool _ | EVar _ | EPick_i () | ETypeInt | ETypeBool
    | EType | ETypeTop | ETypeBottom | ETypeSingle | ETypeUnit | EUnit) as e -> e
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
            ; true_body = EAbort "Divide or modulo by 0"
            ; false_body = EBinop { left = desugar left ; binop ; right = EVar v } }
      | _ -> EBinop { left = desugar left ; binop ; right = desugar right }
    end
    | EIf { cond ; true_body ; false_body } ->
      EIf { cond = desugar cond ; true_body = desugar true_body ; false_body = desugar false_body }
    | ELet { var ; defn ; body } ->
      ELet { var ; defn = desugar defn ; body = desugar body }
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
    | ETypeFun { domain ; codomain ; det ; dep } ->
      ETypeFun { domain = desugar domain ; codomain = desugar codomain ; det ; dep }
    | ETypeRecord m ->
      ETypeRecord (Map.map m ~f:desugar)
    | ETypeModule m ->
      ETypeModule (List.map m ~f:(fun (label, e) -> label, desugar e))
    | ETypeRefinement { tau ; predicate } ->
      ETypeRefinement { tau = desugar tau ; predicate = desugar predicate }
    | ETypeMu { var ; params ; body } ->
      ETypeMu { var ; params ; body = desugar body }
    | ETypeVariant ls_e ->
      ETypeVariant (List.map ls_e ~f:(fun (label, e) -> label, desugar e))
    | ELetTyped { typed_var = { var ; tau } ; defn ; body ; do_wrap ; do_check } ->
      ELetTyped { typed_var = { var ; tau = desugar tau } ; defn = desugar defn ; body = desugar body ; do_wrap ; do_check }
    (* Assert/assume *)
    | EAssert assert_expr ->
      EIf
        { cond = desugar assert_expr
        ; true_body = EUnit
        ; false_body = EAbort "Failed assertion"
        }
    | EAssume assume_expr ->
      EIf
        { cond = desugar assume_expr
        ; true_body = EUnit
        ; false_body = EDiverge ()
        }
    (* Dependent records / modules *)
    | EModule stmts -> EModule (List.bind stmts ~f:desugar_statement)
    (* Patterns *)
    | EMatch { subject ; patterns } ->
      EMatch { subject = desugar subject ; patterns = 
        List.map patterns ~f:(Tuple2.uncurry desugar_pattern)
      }
    (* Lists *)
    | EList [] ->
      EVariant { label = Reserved.nil ; payload = EUnit }
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
    | ETypeList ->
      let tau = Names.fresh_id ~suffix:"tau_list" () in
      let t = Names.fresh_id ~suffix:"list_t" () in
      abstract_over_ids [tau] @@ 
        ETypeMu { var = t ; params = [] ; body =
          ETypeVariant
            [ (Reserved.nil_type, ETypeUnit)
            ; (Reserved.cons_type,
              ETypeRecord (Parsing_tools.record_of_list
                [ (Reserved.hd, EVar tau)
                ; (Reserved.tl, EVar t)
                ]
              )
            )
            ]
        }
    (* Intersection type *)
    | ETypeIntersect ls_e ->
      desugar @@
        let x = Names.fresh_id ~suffix:"x_match_type" () in
        let open List.Let_syntax in
        ETypeFun
          { domain = ETypeVariant (ls_e >>| fun (label, tau, _) -> label, tau)
          ; codomain = EMatch { subject = EVar x ; patterns = 
              ls_e >>| fun (label, _, tau') ->
                PVariant { variant_label = VariantTypeLabel.to_variant_label label ; payload_id = Reserved.catchall }
                , tau'
            }
          ; dep = `Binding x
          ; det = false
          }
    (* Functions *)
    | EMultiArgFunction { params ; body } ->
      abstract_over_ids params (desugar body)
    | ELetFun { func ; body } -> begin
      let { func_id ; defn ; params ; tau_opt } : desugared Function_components.t = funsig_to_components func in
      build @@
        let%bind () =
          assign ~ty:(Binding.Ty.typed_of_tau_opt tau_opt) func_id (
            abstract_over_ids params defn
          )
        in
        return (desugar body)
    end
    | ELetFunRec { funcs ; body } ->
      (* just roll up the statements and tell it to finish with body *)
      pgm_to_expr_with_body (desugar body)
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
        ; payload_id = Reserved.catchall }
      , desugar e
    | PDestructList { hd_id ; tl_id } -> 
      let r = Names.fresh_id () in
      PVariant
        { variant_label = Reserved.cons
        ; payload_id = r }
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
    let defns =
      List.map func_comps ~f:(fun comps ->
        abstract_over_ids f_names @@
          match comps.tau_opt with
          | Some tau when do_type_splay -> EGen tau
          | _ ->
            (* default behavior uses the actual function body *)
            build @@
              let%bind () = assign ~ty:(Binding.Ty.typed_of_tau_opt ~do_check:false comps.tau_opt) comps.func_id (
                abstract_over_ids comps.params comps.defn
              )
              in
              return (EVar comps.func_id)
      )
    in
    Expr.SUntyped { var = r ; defn = appl_list (Desugared_functions.y_n f_names) defns }
    :: (func_comps >>| fun comps ->
      (* do_check and do_wrap are unused arguments in this case because we don't provide the type *)
      make_stmt ~do_wrap:true ~do_check:true ~tau_opt:None comps.func_id
      @@ proj (EVar r) (RecordLabel.RecordLabel comps.func_id)
    ) @ (func_comps >>| fun comps ->
      if Option.is_some comps.tau_opt && do_type_splay
      then
        make_stmt ~do_wrap:true ~do_check:true ~tau_opt:comps.tau_opt comps.func_id (
          abstract_over_ids comps.params comps.defn (* actual definition of function *)
        )
      else
        (* no type or not splaying, so the actual definition was used above, so just project out from the record *)
        make_stmt ~do_wrap:false ~do_check:true ~tau_opt:comps.tau_opt comps.func_id (EVar comps.func_id)
    )

  and desugar_statement (stmt : Bluejay.statement) : Desugared.statement list =
    let open List.Let_syntax in
    match stmt with
    | SUntyped { var ; defn } ->
      return @@ Expr.SUntyped { var ; defn = desugar defn }
    | STyped { typed_var = { var ; tau } ; defn ; do_wrap ; do_check } ->
      return @@ Expr.STyped { typed_var = { var ; tau = desugar tau } ; defn = desugar defn ; do_wrap ; do_check }
    | SFun fsig -> begin
      let { func_id ; defn ; params ; tau_opt } : desugared Function_components.t = funsig_to_components fsig in
      let defn = abstract_over_ids params defn in
      return @@ make_stmt ~do_wrap:true ~do_check:true ~tau_opt func_id defn
    end
    | SFunRec fsigs -> desugar_rec_funs_to_stmt_list fsigs

  and make_stmt ~(do_wrap : bool) ~(do_check : bool) ~(tau_opt : Desugared.t option) (var : Ident.t) (defn : Desugared.t) : Desugared.statement =
    match tau_opt with
    | None ->
      SUntyped { var ; defn }
    | Some tau ->
      STyped { typed_var = { var ; tau } ; defn ; do_wrap ; do_check }
  in

  List.bind pgm ~f:desugar_statement