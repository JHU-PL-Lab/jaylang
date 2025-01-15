
open Core
open Ast
open Expr
open Pattern
open Translation_tools

module Names = Fresh_names ()

module LetMonad = struct
  module Binding = struct
    type s = (Ident.t * Desugared.t) list
  end

  include Monadlib.State.Make (Binding)

  let capture ?(prefix : string option) (e : Desugared.t) : Ident.t m =
    let v = Names.fresh_id ?prefix () in
    let%bind () = modify (List.cons (v, e)) in
    return v

  let assign (id : Ident.t) (e : Desugared.t) : unit m =
    modify (List.cons (id, e))

  let ignore (e : Desugared.t) : unit m =
    assign Reserved_labels.Idents.catchall e

  let build (m : Desugared.t m) : Desugared.t =
    let resulting_bindings, cont = run m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont (id, body) ->
      ELet { var = id ; body ; cont }
    )
end

open LetMonad

(*
  ([x1 ; ... ; xn], e) |->
    fun x1 -> ... -> fun xn -> e
*)
let abstract_over_ids (type a) (ids : Ident.t list) (body : a Expr.t) : a Expr.t =
  List.fold_right ids ~init:body ~f:(fun param body ->
    Expr.EFunction { param ; body }
  )

(* 'a is not fully abstract. It is constrained to be bluejay or desugared. I just don't feel like writing all that *)
let tau_list_to_arrow_type (taus : 'a Expr.t list) (codomain : 'a Expr.t) : 'a Expr.t =
  List.fold_right taus ~init:codomain ~f:(fun domain codomain ->
    Expr.ETypeArrow { domain ; codomain }
    )

let desugar_bluejay (expr : Bluejay.t) : Desugared.t =
  let rec desugar (expr : Bluejay.t) : Desugared.t =
    match expr with
    (* Base cases *)
    | (EInt _ | EBool _ | EVar _ | EPick_i | ETypeInt | ETypeBool) as e -> e
    (* Simple propogation *)
    | EBinop { left ; binop ; right } ->
      EBinop { left = desugar left ; binop ; right = desugar right }
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
    | ETypeArrow { domain ; codomain } ->
      ETypeArrow { domain = desugar domain ; codomain = desugar codomain }
    | ETypeArrowD { binding ; domain ; codomain } ->
      ETypeArrowD { binding ; domain = desugar domain ; codomain = desugar codomain }
    | ERecord m ->
      ERecord (Map.map m ~f:desugar)
    | ETypeRecord m ->
      ETypeRecord (Map.map m ~f:desugar)
    | ETypeRefinement { tau ; predicate } ->
      ETypeRefinement { tau = desugar tau ; predicate = desugar predicate }
    | ETypeIntersect (e, e') ->
      ETypeIntersect (desugar e, desugar e')
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
      EVariant { label = Reserved_labels.Variants.nil ; payload = Values.dummy }
    | EList ls_e ->
      desugar
      @@ List.fold_right ls_e ~init:(EList []) ~f:(fun e acc ->
        EListCons (e, acc)
      )
    | EListCons (e_hd, e_tl) ->
      EVariant { label = Reserved_labels.Variants.cons ; payload =
        ERecord (
          Parsing_tools.new_record Reserved_labels.Records.hd (desugar e_hd)
          |> Parsing_tools.add_record_entry Reserved_labels.Records.hd (
              build @@
                let%bind v = capture ~prefix:"tl" @@ desugar e_tl in
                let%bind () = ignore @@ EAppl { func = Desugared_functions.assert_list ; arg = EVar v } in
                return (EVar v)
          )
        )
      }
    | ETypeList e_tau ->
      let t = Names.fresh_id ~prefix:"list_t" () in
      ETypeMu { var = t ; body =
        ETypeVariant
          [ (Reserved_labels.VariantTypes.nil, ETypeInt)
          ; (Reserved_labels.VariantTypes.cons, ETypeRecord (
              Parsing_tools.new_record Reserved_labels.Records.hd (desugar e_tau)
              |> Parsing_tools.add_record_entry Reserved_labels.Records.tl (EVar t)
            ))
          ]
      }
    (* Forall *)
    | ETypeForall { type_variables ; tau } ->
      List.fold_right type_variables ~init:(desugar tau) ~f:(fun alpha acc ->
        ETypeArrowD { binding = alpha ; domain = EKind ; codomain = acc }
      )
    (* Functions *)
    | EMultiArgFunction { params ; body } ->
      abstract_over_ids params (desugar body)
    | ELetFun { func ; cont } -> begin
      match func with
      | FUntyped { func_id ; params ; body } ->
        ELet
          { var = func_id
          ; body = abstract_over_ids params (desugar body)
          ; cont = desugar cont
          }
      | FTyped { func_id ; params ; body ; ret_type } ->
        let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
        desugar_funsig
          ~func_id ~body ~cont
          ~func_tau:(tau_list_to_arrow_type param_taus ret_type)
          ~parameters:param_ids
      | FPolyTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
        let param_ids, param_taus = List.unzip @@ List.map params ~f:(fun { var ; tau } -> var, tau) in
        desugar_funsig
          ~func_id ~body ~cont
          ~func_tau:(ETypeForall { type_variables = type_vars ; tau = tau_list_to_arrow_type param_taus ret_type })
          ~parameters:(type_vars @ param_ids)
      | FDepTyped { func_id ; params ; ret_type ; body } ->
        desugar_funsig
          ~func_id ~body ~cont
          ~func_tau:(ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type })
          ~parameters:[ params.var ]
      | FPolyDepTyped { func = { func_id ; params ; ret_type ; body } ; type_vars } ->
        desugar_funsig
          ~func_id ~body ~cont
          ~func_tau:(
            ETypeForall
              { type_variables = type_vars
              ; tau = ETypeArrowD { binding = params.var ; domain = params.tau ; codomain = ret_type } }
          )
          ~parameters:(type_vars @ [ params.var ])
    end
    | ELetFunRec _ -> failwith "unimplemented let fun rec"

  (*
    Makes abstracted typed let-expression:

      let (func_id : [| func_tau |]) =
        fun [| parameters |] -> [| body |]
      in
      [| cont ||]

    This is used when desugaring let-funs into let-expressions of anonymous functions
  *)
  and desugar_funsig ~(func_id : Ident.t) ~(func_tau : Bluejay.t) ~(parameters : Ident.t list) ~(body : Bluejay.t) ~(cont : Bluejay.t) : Desugared.t =
    ELetTyped
      { typed_var = 
        { var = func_id
        ; tau = desugar func_tau }
      ; body = abstract_over_ids parameters @@ desugar body
      ; cont = desugar cont
      }

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
