
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
        let _ = tau_opt in
        Untyped (* FIXME: wrap is turned off for recursion until the 123456789 trick is fully baked in *)
        (* flags_if_some tau_opt
        @@ LetFlag.Set.of_list [ NoCheck ; TauKnowsBinding ] *)

      let typed_of_tau_opt (tau_opt : Desugared.t option) : t =
        match tau_opt with
        | Some tau -> Typed tau
        | None -> Untyped
    end
    type t = (Kind.t * Ident.t * Desugared.t)
  end

  module State = struct
    type s = Binding.t list
  end

  include Monadlib.State.Make (State)

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
    let resulting_bindings, cont = run m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont (kind, id, body) ->
      match kind with
      | Untyped ->
        ELet { var = id ; body ; cont }
      | Typed_with_flags (flags, tau) ->
        (* ELet { var = id ; body ; cont } FIXME: turned this off for now to test ill-typed-interpreter *)
        ELetFlagged { flags ; typed_var = { var = id ; tau } ; body ; cont }
      | Typed tau ->
        ELetTyped { typed_var = { var = id ; tau } ; body ; cont }
    )
end

let desugar_bluejay (names : (module Fresh_names.S)) (expr : Bluejay.t) : Desugared.t =
  let module Names = (val names) in
  let open LetMonad in

  let rec desugar (expr : Bluejay.t) : Desugared.t =
    match expr with
    (* Base cases *)
    | (EInt _ | EBool _ | EVar _ | EPick_i | ETypeInt | ETypeBool) as e -> e
    (* Simple propogation *)
    | EBinop { left ; binop ; right } -> begin
      match binop with
      | BAnd -> EIf { cond = desugar left ; true_body = desugar right ; false_body = EBool false }
      | BOr -> EIf { cond = desugar left ; true_body = EBool true ; false_body = desugar right }
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
    | ETypeRefinement { tau ; predicate } ->
      ETypeRefinement { tau = desugar tau ; predicate = desugar predicate }
    | ETypeIntersect ls_e ->
      ETypeIntersect (List.map ls_e ~f:(fun (label, e_tau1, e_tau2) -> label, desugar e_tau1, desugar e_tau2))
    | ETypeMu { var ; body } ->
      ETypeMu { var ; body = desugar body }
    | ETypeVariant ls_e ->
      ETypeVariant (List.map ls_e ~f:(fun (label, e) -> label, desugar e))
    | ELetTyped { typed_var = { var ; tau } ; body ; cont } ->
      ELetTyped { typed_var = { var ; tau = desugar tau } ; body = desugar body ; cont = desugar cont }
    (* Assert/assume *)
    | EAssert assert_expr ->
      EIf
        { cond = desugar assert_expr
        ; true_body = ERecord Parsing_tools.empty_record
        ; false_body = EAbort
        }
    | EAssume assume_expr ->
      EIf
        { cond = desugar assume_expr
        ; true_body = ERecord Parsing_tools.empty_record
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
                  { variant_label = Reserved_labels.Variants.untouched
                  ; payload_id = Reserved_labels.Idents.catchall }
                , EAbort)
              :: acc
            )
          | _ -> Continue (desugar_pattern pat e :: acc)
        ) ~finish:List.rev
      }
    (* Lists *)
    | EList [] ->
      EVariant { label = Reserved_labels.Variants.nil ; payload = Utils.dummy_value }
    | EList ls_e ->
      desugar
      @@ List.fold_right ls_e ~init:(EList []) ~f:(fun e acc ->
        EListCons (e, acc)
      )
    | EListCons (e_hd, e_tl) ->
      EVariant { label = Reserved_labels.Variants.cons ; payload =
        ERecord (Parsing_tools.record_of_list
          [ (Reserved_labels.Records.hd, desugar e_hd)
          ; (Reserved_labels.Records.tl, EAppl { func = Desugared_functions.filter_list ; arg = desugar e_tl })
          ]
        )
      }
    | ETypeList e_tau ->
      let t = Names.fresh_id ~suffix:"list_t" () in
      ETypeMu { var = t ; body =
        ETypeVariant
          [ (Reserved_labels.VariantTypes.nil, ETypeInt)
          ; (Reserved_labels.VariantTypes.cons,
            ETypeRecord (Parsing_tools.record_of_list
              [ (Reserved_labels.Records.hd, desugar e_tau)
              ; (Reserved_labels.Records.tl, EVar t)
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
      let func_comps = List.map funcs ~f:funsig_to_components in
      let f_names = List.map func_comps ~f:(fun r -> r.func_id) in
      let tmp_name_of_name =
        let m =
          List.fold f_names ~init:Ident.Map.empty ~f:(fun acc func_id ->
            Map.set acc ~key:func_id ~data:(let Ident suffix = func_id in Names.fresh_id ~suffix ())
            )
        in
        Map.find_exn m 
      in
      let tmp_names = List.map f_names ~f:tmp_name_of_name in
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
              (List.map tmp_names ~f:(fun id -> EVar id))
          )
        )
      in
      build @@
        let%bind () =
          iter func_comps ~f:(fun { func_id ; params ; body ; _ } ->
            assign (tmp_name_of_name func_id) (
              abstract_over_ids tmp_names (
                abstract_over_ids params (
                  build @@ 
                    let%bind () = create_functions Binding.Kind.rec_no_check_of_tau_opt in
                    return body
                )
              )
            )
          )
        in
        let%bind () = create_functions Binding.Kind.rec_of_tau_opt in
        return (desugar cont)

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
        { variant_label = Reserved_labels.Variants.nil
        ; payload_id = Reserved_labels.Idents.catchall
        }
      , desugar e
    | PDestructList { hd_id ; tl_id } ->
      let r = Names.fresh_id () in
      PVariant
        { variant_label = Reserved_labels.Variants.cons
        ; payload_id = r
        }
      , build @@
        let%bind () = assign hd_id (EProject { record = EVar r ; label = Reserved_labels.Records.hd }) in
        let%bind () = assign tl_id (EProject { record = EVar r ; label = Reserved_labels.Records.tl }) in
        return (desugar e)
    | (PAny | PVariable _ | PVariant _) as p -> p, desugar e
  in

  desugar expr
