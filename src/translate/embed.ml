
open Core
open Lang
open Ast
open Pattern
open Expr
open Translation_tools
open Ast_tools
open Ast_tools.Utils

module LetMonad (Names : Fresh_names.S)= struct
  module Binding = struct
    type t =
      | Bind of Ident.t * Embedded.t
      | Ignore of Embedded.t
  end

  module State = struct
    type s = Binding.t list
  end

  include Monadlib.State.Make (State)

  open Binding

  let capture ?(suffix : string option) (e : Embedded.t) : Ident.t m =
    let v = Names.fresh_id ?suffix () in
    let%bind () = modify (List.cons (Bind (v, e))) in
    return v

  let assign (id : Ident.t) (e : Embedded.t) : unit m =
    modify (List.cons (Bind (id, e)))

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold ls ~init:(return ()) ~f:(fun acc_m a ->
      let%bind () = acc_m in
      f a
    )

  let ignore (e : Embedded.t) : unit m =
    modify (List.cons (Ignore e))

  let build (m : Embedded.t m) : Embedded.t =
    let resulting_bindings, cont = run m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont -> function
      | Bind (id, body) ->
        ELet { var = id ; body ; cont }
      | Ignore ignored ->
        EIgnore { ignored ; cont }
    )
end

module Embedded_type = struct
  (*
    Note: then gen body gets frozen
  *)
  let make ~(gen : Embedded.t) ~(check : Embedded.t) ~(wrap : Embedded.t) : Embedded.t =
    ERecord (
      Parsing_tools.record_of_list
        [ (Reserved_labels.Records.gen, Expr.EFreeze gen)
        ; (Reserved_labels.Records.check, check)
        ; (Reserved_labels.Records.wrap, wrap)
        ]
    )

  (*
    Applies arg to tau's check, which is partially evaluated if possible.
  *)
  let check ~(tau : Embedded.t) (arg : Embedded.t) : Embedded.t =
    apply
      (proj tau Reserved_labels.Records.check)
      arg

  (*
    Thaws the gen from the given tau, where the record projection on "gen" might be partially evaluated.
  *)
  let gen (tau : Embedded.t) : Embedded.t =
    EThaw (proj tau Reserved_labels.Records.gen)

  (*
    Applies arg to tau's wrap, which is partially evaluated if possible.
  *)
  let wrap ~(tau : Embedded.t) (x : Embedded.t) : Embedded.t =
    apply
      (proj tau Reserved_labels.Records.wrap)
      x
end

let embed_desugared (names : (module Fresh_names.S)) (expr : Desugared.t) : Embedded.t =
  let module Names = (val names) in
  let open LetMonad (Names) in

  let fresh_abstraction (type a) (suffix : string) (e : Ident.t -> a Expr.t) : a Expr.t =
    let id = Names.fresh_id ~suffix () in
    EFunction { param = id ; body = e id }
  in

  let rec embed (expr : Desugared.t) : Embedded.t =
    match expr with
    (* base cases *)
    | (EInt _ | EBool _ | EVar _ | EPick_i | EAbort | EDiverge) as e -> e
    (* Simple propogation *)
    | EBinop { left ; binop ; right } ->
      EBinop { left = embed left ; binop ; right = embed right }
    | EIf { cond ; true_body ; false_body } ->
      EIf { cond = embed cond ; true_body = embed true_body ; false_body = embed false_body }
    | ELet { var ; body ; cont } ->
      ELet { var ; body = embed body ; cont = embed cont }
    | EAppl { func ; arg } ->
      EAppl { func = embed func ; arg = embed arg }
    | EProject { record ; label } ->
      EProject { record = embed record ; label }
    | ENot e ->
      ENot (embed e)
    | EFunction { param ; body } ->
      EFunction { param ; body = embed body }
    | EVariant { label ; payload } ->
      EVariant { label ; payload = embed payload }
    | ERecord m ->
      ERecord (Map.map m ~f:embed)
    | EMatch { subject ; patterns } ->
      EMatch { subject = embed subject ; patterns =
        List.map patterns ~f:(fun (pat, e) -> (embed_pattern pat, embed e))
      }
    (* Let *)
    | ELetTyped { typed_var = { var = x ; tau } ; body = e ; cont = e' } ->
      build @@
        let%bind () =
          assign x (
            build @@
              let%bind r = capture ~suffix:"r" @@ embed tau in
              let%bind v = capture ~suffix:"v" @@ embed e in
              let%bind () = ignore @@ Embedded_type.check ~tau:(EVar r) (EVar v) in
              return @@ Embedded_type.wrap ~tau:(EVar r) (EVar v)
          )
        in
        return @@ embed e'
    | ELetWrap { typed_var = { var = x ; tau } ; body = e ; cont = e' } ->
      build @@
        let%bind () = assign x @@ wrap tau (embed e)
        in
        return @@ embed e'
    (* types *)
    | ETypeInt ->
      Embedded_type.make
        ~gen:EPick_i
        ~check:(
          fresh_abstraction "e_int_check" (fun e ->
            build @@
              let%bind () = ignore (EBinop { left = EVar e ; binop = BPlus ; right = EInt 0 }) in
              return (EBool true)
          )
        )
        ~wrap:EId
    | ETypeBool ->
      Embedded_type.make
        ~gen:EPick_b
        ~check:(
          fresh_abstraction "e_bool_check" (fun e ->
            build @@
              let%bind () = ignore (ENot (EVar e)) in
              return (EBool true)
          
          )
        )
        ~wrap:EId
    | ETypeArrow { domain = tau1 ; codomain = tau2 } ->
      Embedded_type.make
        ~gen:(
          fresh_abstraction "arg_arrow_gen" (fun arg ->
            EIf
              { cond = check tau1 (EVar arg)
              ; true_body = gen tau2
              ; false_body = EAbort
              }
          )
        )
        ~check:(
          fresh_abstraction "e_arrow_check" (fun e ->
            check tau2 (
              apply (EVar e) (gen tau1)
            )
          )
        )
        ~wrap:(
          fresh_abstraction "e_arrow_wrap" (fun e ->
            fresh_abstraction "x_arrow_wrap" (fun x ->
              EIf
                { cond = check tau1 (EVar x)
                ; true_body = wrap tau2 (
                  apply (EVar e) (wrap tau1 (EVar x))
                )
                ; false_body = EAbort
                }
            )
          )
        )
    | ETypeRecord m ->
      Embedded_type.make
        ~gen:(ERecord (Map.map m ~f:gen))
        ~check:(
          fresh_abstraction "e_rec_check" (fun e ->
            build @@
              let%bind () =
                iter (Map.to_alist m) ~f:(fun (label, tau) ->
                  ignore (check tau (proj (EVar e) label))
                )
                in
                return (EBool true)
          )
        )
        ~wrap:(
          fresh_abstraction "e_rec_wrap" (fun e ->
            ERecord (
              Map.mapi m ~f:(fun ~key:label ~data:tau ->
                wrap tau (proj (EVar e) label)
              )
            )
          )
        )
    | EKind ->
      Embedded_type.make
        ~gen:(
          let a = Names.fresh_poly_value () in
          Embedded_type.make
            ~gen:(EVariant { label = Reserved_labels.Variants.untouched ; payload = EInt a })
            ~check:(
              fresh_abstraction "e_alpha_check" (fun e ->
                EMatch { subject = EVar e ; patterns =
                  let v = Names.fresh_id ~suffix:"v" () in
                  [ (PVariant { variant_label = Reserved_labels.Variants.untouched ; payload_id = v }
                    , EIf
                        { cond = EBinop { left = EVar v ; binop = BEqual ; right = EInt a }
                        ; true_body = EBool true
                        ; false_body = EAbort 
                        })
                  ]
                }
              )
            )
            ~wrap:EId
        )
        ~check:(
          fresh_abstraction "e_kind_check" (fun e ->
            build @@  
              let e = EVar e in
              let%bind () = ignore @@ proj e Reserved_labels.Records.gen in
              let%bind () = ignore @@ proj e Reserved_labels.Records.check in
              let%bind () = ignore @@ proj e Reserved_labels.Records.wrap in
              return (EBool true)
          )
        )
        ~wrap:EId
    | ETypeArrowD { binding = x ; domain = tau1 ; codomain = tau2 } ->
      Embedded_type.make
        ~gen:(
          fresh_abstraction "xp_arrowd_gen" (fun x' ->
            EIf
              { cond = check tau1 (EVar x')
              ; true_body = apply (EFunction { param = x ; body = gen tau2}) (EVar x')
              ; false_body = EAbort
              }
          )
        )
        ~check:(
          fresh_abstraction "e_arrowd_check"( fun e ->
            build @@
              let%bind arg = capture @@ gen tau1 in
              return
              @@ appl_list
                  (EFunction { param = x ; body = proj (embed tau2) Reserved_labels.Records.check })
                  [ (EVar arg) ; apply (EVar e) (EVar arg) ]
          )
        )
        ~wrap:(
          fresh_abstraction "e_arrowd_wrap" (fun e ->
            fresh_abstraction "xp_arrowd_wrap" (fun x' ->
              EIf
                { cond = check tau1 (EVar x')
                ; true_body = 
                  appl_list
                    (EFunction { param = x ; body = proj (embed tau2) Reserved_labels.Records.wrap })
                    [ (EVar x') ; apply (EVar e) (wrap tau1 (EVar x')) ]
                ; false_body = EAbort
                }
            )
          )
        )
    | ETypeRefinement { tau ; predicate = e_p } ->
      Embedded_type.make
        ~gen:(
          build @@
            let%bind gend = capture @@ gen tau in
            return (EIf
              { cond = apply (embed e_p) (EVar gend)
              ; true_body = EVar gend
              ; false_body = EDiverge
              }
            )
        )
        ~check:(
          fresh_abstraction "e_ref_check" (fun e ->
            build @@
              let%bind () = ignore @@ check tau (EVar e) in
              return (EIf
                { cond = apply (embed e_p) (EVar e)
                ; true_body = EBool true
                ; false_body = EAbort
                }
              )
          )
        )
        ~wrap:(
          fresh_abstraction "e_ref_wrap" (fun e ->
            wrap tau (EVar e)
          )
        )
    | ETypeVariant e_variant_type ->
      let e_variant_ls =
        List.map e_variant_type ~f:(fun (type_label, tau) ->
          VariantTypeLabel.to_variant_label type_label, tau
        )
      in
      Embedded_type.make
        ~gen:(
          ECase
            { subject = EPick_i
            ; cases =
              List.drop_last_exn e_variant_ls
              |> List.mapi ~f:(fun i (label, tau) ->
                i, EVariant { label ; payload = gen tau }
              )
            ; default = 
              let (last_label, last_tau) = List.last_exn e_variant_ls in
              EVariant { label = last_label ; payload = gen last_tau }
            }
        )
        ~check:(
          fresh_abstraction "e_var_check" (fun e ->
            EMatch { subject = EVar e ; patterns =
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , check tau (EVar v)
                )
            }  
          )
        )
        ~wrap:(
          fresh_abstraction "e_var_wrap" (fun e ->
            EMatch { subject = EVar e ; patterns = 
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , EVariant { label = variant_label ; payload = wrap tau (EVar v) }
                )
            }  
          )
        )
    | ETypeMu { var = b ; body = tau } ->
      apply (
        apply Embedded_functions.y_comb (
          fresh_abstraction "self_mu" (fun self ->
            fresh_abstraction "dummy_mu" (fun _dummy ->
              let appl_self_0 = apply (EVar self) (EInt 0) in
              Embedded_type.make
                ~gen:(
                  apply (EFunction { param = b ; body = gen tau }) appl_self_0
                )
                ~check:(
                  fresh_abstraction "e_mu_check" (fun e ->
                    apply (EFunction { param = b ; body = check tau (EVar e) }) appl_self_0
                  )
                )
                ~wrap:(
                  fresh_abstraction "e_mu_wrap" (fun e ->
                    apply (EFunction { param = b ; body = wrap tau (EVar e) }) appl_self_0
                  )
                )
            )
          )
        )
      ) (EInt 0)
    | ETypeIntersect e_intersect_type ->
      let e_intersect_ls = 
        List.map e_intersect_type ~f:(fun (type_label, tau, tau') ->
          VariantTypeLabel.to_variant_label type_label, tau, tau'
        )
      in
      Embedded_type.make
        ~gen:(
          fresh_abstraction "arg_inter_gen" (fun arg ->
            EMatch { subject = EVar arg ; patterns =
              let v = Names.fresh_id () in
              List.map e_intersect_ls ~f:(fun (label, tau, tau') ->
                PVariant { variant_label = label ; payload_id = v }
                , EIf
                    { cond = check tau (EVar v)
                    ; true_body = gen tau'
                    ; false_body = EAbort
                    }
              )
            }
          )
        )
        ~check:(
          fresh_abstraction "e_inter_check" (fun e ->
            ECase
              { subject = EPick_i
              ; cases =
                List.drop_last_exn e_intersect_type
                |> List.mapi ~f:(fun i (label, tau, tau') ->
                  i, check (
                    ETypeArrow { domain = ETypeVariant [ (label, tau) ] ; codomain = tau' }
                ) (EVar e) 
              )
              ; default =
                let (last_label, last_tau, last_tau') = List.last_exn e_intersect_type in
                check (
                  ETypeArrow { domain = ETypeVariant [ (last_label, last_tau) ] ; codomain = last_tau' }
                ) (EVar e)
              }
          )
        )
        ~wrap:(
          fresh_abstraction "e_inter_wrap" (fun e ->
            fresh_abstraction "arg_inter_wrap" (fun arg ->
              EMatch { subject = EVar arg ; patterns = 
                let v = Names.fresh_id () in
                List.map e_intersect_ls ~f:(fun (label, tau, tau') ->
                  PVariant { variant_label = label ; payload_id = v }
                  , EIf
                      { cond = check tau (EVar v)
                      ; true_body = wrap tau' (apply (EVar e) (EVariant { label ; payload = wrap tau (EVar v) }))
                      ; false_body = EAbort
                      }
                )
              }
            )
          )
        )

    and embed_pattern (pat : Desugared.pattern) : Embedded.pattern =
      match pat with
      | (PAny | PVariable _ | PVariant _) as p -> p

    and gen (tau : Desugared.t) : Embedded.t =
      Embedded_type.gen (embed tau)

    and check (tau : Desugared.t) (x : Embedded.t) : Embedded.t =
      Embedded_type.check ~tau:(embed tau) x

    and wrap (tau : Desugared.t) (x : Embedded.t) : Embedded.t =
      Embedded_type.wrap ~tau:(embed tau) x
  in
  embed expr

