
open Core
open Ast
open Pattern
open Expr
open Translation_tools

module Names = Desugar.Names (* TODO: get the actual state from desugaring *)

module LetMonad = struct
  module Binding = struct
    type t = (Ident.t * Embedded.t)
  end

  module State = struct
    type s = Binding.t list
  end

  include Monadlib.State.Make (State)

  let capture ?(prefix : string option) (e : Embedded.t) : Ident.t m =
    let v = Names.fresh_id ?prefix () in
    let%bind () = modify (List.cons (v, e)) in
    return v

  let assign (id : Ident.t) (e : Embedded.t) : unit m =
    modify (List.cons (id, e))

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold ls ~init:(return ()) ~f:(fun acc_m a ->
      let%bind () = acc_m in
      f a
    )

  let ignore (e : Embedded.t) : unit m =
    assign Reserved_labels.Idents.catchall e

  let build (m : Embedded.t m) : Embedded.t =
    let resulting_bindings, cont = run m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont (id, body) ->
      ELet { var = id ; body ; cont }
    )
end

open LetMonad

(*
  Is a partially-evaluating apply, where the function is checked against the identity function.
*)
let apply (type a) (func : a Expr.t) (arg : a Expr.t) : a Expr.t =
  match func with
  | EId -> arg
  | _ -> EAppl { func ; arg }

(*
  Is a partially-evaluating record projection, which is safe because we have no side effects coming
  from the evaluation of the non-projected fields.
*)
let proj (type a) (tau : a Expr.t) (label : RecordLabel.t) : a Expr.t =
  match tau with
  | ERecord m when Map.mem m label ->
    Map.find_exn m label
  | _ -> EProject { record = tau ; label }

let fresh_abstraction (type a) ?(prefix : string option) (e : Ident.t -> a Expr.t) : a Expr.t =
  let id = Names.fresh_id ?prefix () in
  EFunction { param = id ; body = e id }

(*
  TODO: move this to common place because I have it in both desugar and embed

  f, [ x1 ; ... ; xn ] |->
    f x1 ... xn
*)
let appl_list (type a) (f : a Expr.t) (args : a Expr.t list) : a Expr.t =
  List.fold args ~init:f ~f:(fun func arg ->
    apply func arg
  )

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

let embed_desugared (expr : Desugared.t) : Embedded.t =
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
              let%bind r = capture ~prefix:"r" @@ embed tau in
              let%bind v = capture ~prefix:"v" @@ embed e in
              let%bind () = ignore @@ Embedded_type.check ~tau:(EVar r) (EVar v) in
              return @@ Embedded_type.wrap ~tau:(EVar r) (EVar v)
          )
        in
        return @@ embed e'
    | ELetWrap { typed_var = { var = x ; tau } ; body = e ; cont = e' } ->
      build @@
        let%bind () = assign x @@ Embedded_type.wrap ~tau:(embed tau) (embed e)
        in
        return @@ embed e'
    (* types *)
    | ETypeInt ->
      Embedded_type.make
        ~gen:EPick_i
        ~check:(
          fresh_abstraction (fun e ->
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
          fresh_abstraction (fun e ->
            build @@
              let%bind () = ignore (ENot (EVar e)) in
              return (EBool true)
          
          )
        )
        ~wrap:EId
    | ETypeArrow { domain = tau1 ; codomain = tau2 } ->
      Embedded_type.make
        ~gen:(
          fresh_abstraction ~prefix:"arg" (fun arg ->
            EIf
              { cond = Embedded_type.check ~tau:(embed tau1) (EVar arg)
              ; true_body = Embedded_type.gen (embed tau2)
              ; false_body = EAbort
              }
          )
        )
        ~check:(
          fresh_abstraction (fun e ->
            Embedded_type.check ~tau:(embed tau2) (
              apply (EVar e) (Embedded_type.gen (embed tau1))
            )
          )
        )
        ~wrap:(
          fresh_abstraction (fun e ->
            fresh_abstraction (fun x ->
              EIf
                { cond = Embedded_type.check ~tau:(embed tau1) (EVar x)
                ; true_body = Embedded_type.wrap ~tau:(embed tau2) (
                  apply (EVar e) (Embedded_type.wrap ~tau:(embed tau1) (EVar x))
                )
                ; false_body = EAbort
                }
            )
          )
        )
    | ETypeRecord m ->
      Embedded_type.make
        ~gen:(ERecord (Map.map m ~f:(fun tau -> Embedded_type.gen @@ embed tau)))
        ~check:(
          fresh_abstraction (fun e ->
            build @@
              let%bind () =
                iter (Map.to_alist m) ~f:(fun (label, tau) ->
                  ignore (Embedded_type.check ~tau:(embed tau) (proj (EVar e) label))
                )
                in
                return (EBool true)
          )
        )
        ~wrap:(
          fresh_abstraction (fun e ->
            ERecord (
              Map.mapi m ~f:(fun ~key:label ~data:tau ->
                Embedded_type.wrap ~tau:(embed tau) (proj (EVar e) label)
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
              fresh_abstraction (fun e ->
                EMatch { subject = EVar e ; patterns =
                  let v = Names.fresh_id ~prefix:"v" () in
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
          fresh_abstraction (fun e ->
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
          fresh_abstraction (fun x' ->
            EIf
              { cond = Embedded_type.check ~tau:(embed tau1) (EVar x')
              ; true_body = apply (EFunction { param = x ; body = Embedded_type.gen (embed tau2)}) (EVar x')
              ; false_body = EAbort
              }
          )
        )
        ~check:(
          fresh_abstraction (fun e ->
            build @@
              let%bind arg = capture @@ Embedded_type.gen (embed tau1) in
              return
              @@ appl_list
                  (EFunction { param = x ; body = proj (embed tau2) Reserved_labels.Records.check })
                  [ (EVar arg) ; apply (EVar e) (EVar arg) ]
          )
        )
        ~wrap:(
          fresh_abstraction (fun e ->
            fresh_abstraction (fun x' ->
              EIf
                { cond = Embedded_type.check ~tau:(embed tau1) (EVar x')
                ; true_body = 
                  appl_list
                    (EFunction { param = x ; body = proj (embed tau2) Reserved_labels.Records.wrap })
                    [ (EVar x') ; apply (EVar e) (Embedded_type.wrap ~tau:(embed tau1) (EVar x')) ]
                ; false_body = EAbort
                }
            )
          )
        )
    | ETypeRefinement { tau ; predicate = e_p } ->
      Embedded_type.make
        ~gen:(
          build @@
            let%bind gend = capture @@ Embedded_type.gen (embed tau) in
            return (EIf
              { cond = apply (embed e_p) (EVar gend)
              ; true_body = EVar gend
              ; false_body = EDiverge
              }
            )
        )
        ~check:(
          fresh_abstraction (fun e ->
            build @@
              let%bind () = ignore @@ Embedded_type.check ~tau:(embed tau) (EVar e) in
              return (EIf
                { cond = apply (embed e_p) (EVar e)
                ; true_body = EBool true
                ; false_body = EAbort
                }
              )
          )
        )
        ~wrap:(
          fresh_abstraction (fun e ->
            Embedded_type.wrap ~tau:(embed tau) (EVar e)
          )
        )
    | ETypeVariant e_variant_type ->
      let e_variant_ls =
        List.map e_variant_type ~f:(fun (type_label, tau) -> VariantTypeLabel.to_variant_label type_label, tau)
      in
      Embedded_type.make
        ~gen:(
          ECase { subject = EPick_i ; cases =
            List.mapi e_variant_ls ~f:(fun i (label, tau) ->
              i, EVariant { label ; payload = Embedded_type.gen (embed tau) }
            )
          }
          (* TODO: handle when input is not in case, i.e. have wildcard per the spec *)
        )
        ~check:(
          fresh_abstraction (fun e ->
            EMatch { subject = EVar e ; patterns =
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                let v = Names.fresh_id () in
                PVariant { variant_label ; payload_id = v }
                , Embedded_type.check ~tau:(embed tau) (EVar v)
                )
            }  
          )
        )
        ~wrap:(
          fresh_abstraction (fun e ->
            EMatch { subject = EVar e ; patterns = 
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                let v = Names.fresh_id () in
                PVariant { variant_label ; payload_id = v }
                , EVariant { label = variant_label ; payload = Embedded_type.wrap ~tau:(embed tau) (EVar v) }
                )
            }  
          )
        )
    | ETypeMu _
    | ETypeIntersect _ -> failwith "unimplemented type embedding"

    and embed_pattern (pat : Desugared.pattern) : Embedded.pattern =
      match pat with
      | (PAny | PVariable _ | PVariant _) as p -> p

  in
  embed expr

