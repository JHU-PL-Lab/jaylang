
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
    module Kind = struct
      type t = 
        | Untyped
        | Typed of Desugared.t
        | Typed_with_flags of LetFlag.Set.t * Desugared.t

      let flags_if_some (tau_opt : Desugared.t option) (flags : LetFlag.Set.t) : t =
        match tau_opt with
        | Some tau -> Typed_with_flags (flags, tau)
        | None -> Untyped

      let rec_of_tau_opt (tau_opt : Desugared.t option) : t =
        flags_if_some tau_opt
        @@ LetFlag.Set.singleton TauKnowsBinding

      let rec_no_check_of_tau_opt (tau_opt : Desugared.t option) : t =
        flags_if_some tau_opt
        @@ LetFlag.Set.of_list [ NoCheck ; TauKnowsBinding ]

      let typed_of_tau_opt (tau_opt : Desugared.t option) : t =
        match tau_opt with
        | Some tau -> Typed tau
        | None -> Untyped
    end
    type t = (Kind.t * Ident.t * Desugared.t)
  end

  module State = struct
    type t = Binding.t list
  end

  module T = struct
    module M = Preface.State.Over (State)
    include M
    type 'a m = 'a M.t
    let bind x f = M.bind f x
  end

  include T

  (*
    Assign the expression the given name with optional typing.
  *)
  let assign ?(kind : Binding.Kind.t = Untyped) (id : Ident.t) (e : Desugared.t) : unit m =
    modify (List.cons (kind, id, e))

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold ls ~init:(return ()) ~f:(fun acc_m a ->
      let%bind () = acc_m in
      f a
    )

  (*
    Build an expression (of many nested let-expressions) using the assignments
    in the monad's state.
  *)
  let build (m : Desugared.t m) : Desugared.t =
    let cont, resulting_bindings = M.run_identity m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont (kind, id, body) ->
      match kind with
      | Untyped ->
        ELet { var = id ; body ; cont }
      | Typed_with_flags (flags, tau) ->
        ELetFlagged { flags ; typed_var = { var = id ; tau } ; body ; cont }
      | Typed tau ->
        ELetTyped { typed_var = { var = id ; tau } ; body ; cont }
    )
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
    | ETypeRecordD m ->
      ETypeRecordD (List.map m ~f:(fun (label, e) -> label, desugar e))
    | ETypeRefinement { tau ; predicate } ->
      ETypeRefinement { tau = desugar tau ; predicate = desugar predicate }
    | ETypeMu { var ; body } ->
      ETypeMu { var ; body = desugar body }
    | ETypeVariant ls_e ->
      ETypeVariant (List.map ls_e ~f:(fun (label, e) -> label, desugar e))
    | ELetTyped { typed_var = { var ; tau } ; body ; cont } ->
      ELetTyped { typed_var = { var ; tau = desugar tau } ; body = desugar body ; cont = desugar cont }
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
          assign ~kind:(Binding.Kind.typed_of_tau_opt tau_opt) func_id (
            abstract_over_ids params body
          )
        in
        return (desugar cont)
    end
    | ELetFunRec { funcs ; cont } ->
      (* just roll up the statements and tell it to finish with cont *)
      Program.to_expr_with_cont (desugar cont)
      @@ desugar_rec_funs_to_stmt_list funcs

  (*
    Breaks a function signature into its id, type (optional), parameter names, and function body, all desugared.
  *)
  and funsig_to_components (fsig : Bluejay.funsig) : desugared Function_components.t =
    Function_components.map ~f:desugar
    @@ Funsig.to_components fsig

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
    let tmp_name_of_name =
      let m =
        List.fold f_names ~init:Ident.Map.empty ~f:(fun acc func_id ->
          Map.set acc ~key:func_id ~data:(let Ident suffix = func_id in Names.fresh_id ~suffix ())
          )
      in
      Map.find_exn m 
    in
    let tmp_names = f_names >>| tmp_name_of_name in
    (*
      Creates
        let f1 = $f1 $f1 $f2 ... $fn in
        ...
        let fn = $fn $f1 $f2 ... $fn in
    *)
    let create_functions (binding_fun : Desugared.t option -> Binding.Kind.t) : unit m =
      iter func_comps ~f:(fun { func_id ; tau_opt ; _ } ->
        assign ~kind:(binding_fun tau_opt) func_id (
          appl_list
            (EVar (tmp_name_of_name func_id))
            (tmp_names >>| fun id -> EVar id)
        )
      )
    in
    (func_comps >>| fun { func_id ; params ; body ; _ } ->
      Program.SUntyped { var = tmp_name_of_name func_id ; body = 
        abstract_over_ids tmp_names (
          abstract_over_ids params (
            build @@ 
              let%bind () = create_functions Binding.Kind.rec_no_check_of_tau_opt in
              return body
          )
        )
      }
    )
    @ (func_comps >>| fun { func_id ; tau_opt ; _ } ->
      let body =
        appl_list
          (EVar (tmp_name_of_name func_id))
          (tmp_names >>| fun id -> EVar id)
      in
      match tau_opt with
      | None ->
        Program.SUntyped { var = func_id ; body }
      | Some tau ->
        Program.SFlagged { flags = LetFlag.Set.singleton TauKnowsBinding ; typed_var = { var = func_id ; tau } ; body }
    )

  and desugar_statement (stmt : Bluejay.statement) : Desugared.statement list =
    let open List.Let_syntax in
    match stmt with
    | SUntyped { var ; body } ->
      return @@ Program.SUntyped { var ; body = desugar body }
    | STyped { typed_var = { var ; tau } ; body } ->
      return @@ Program.STyped { typed_var = { var ; tau = desugar tau } ; body = desugar body }
    | SFun fsig -> begin
      let { func_id ; body ; params ; tau_opt } : desugared Function_components.t = funsig_to_components fsig in
      let body = abstract_over_ids params body in
      match tau_opt with
      | None ->
        return @@ Program.SUntyped { var = func_id ; body }
      | Some tau ->
        return @@ Program.STyped { typed_var = { var = func_id ; tau } ; body }
      end
    | SFunRec fsigs -> desugar_rec_funs_to_stmt_list fsigs
  in

  List.bind pgm ~f:desugar_statement