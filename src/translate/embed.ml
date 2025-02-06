
open Core
open Lang
open Ast
open Pattern
open Expr
open Translation_tools
open Ast_tools
open Ast_tools.Utils

module LetMonad (Names : Fresh_names.S) = struct
  module Binding = struct
    type t =
      | Bind of Ident.t * Embedded.t
      | Ignore of Embedded.t
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

  open Binding

  (*
    Capture the expression under a fresh name and return that name.
  *)
  let capture ?(suffix : string option) (e : Embedded.t) : Ident.t m =
    let v = Names.fresh_id ?suffix () in
    let%bind () = modify (List.cons (Bind (v, e))) in
    return v

  (*
    Assign the expression the given name.
  *)
  let assign (id : Ident.t) (e : Embedded.t) : unit m =
    modify (List.cons (Bind (id, e)))

  let iter (ls : 'a list) ~(f : 'a -> unit m) : unit m =
    List.fold ls ~init:(return ()) ~f:(fun acc_m a ->
      let%bind () = acc_m in
      f a
    )

  (*
    Compute the value of the expression but ignore it.
  *)
  let ignore (e : Embedded.t) : unit m =
    modify (List.cons (Ignore e))

  (*
    Build the expression (of many nested let-expressions or ignore-expressions) using
    the monad's state.

    This is the slowest part of the translation BY FAR. However, I keep the translation
    small by only asking for certain labels from types, so the effect is not that big.
    However, for faster translation, I would NEED to remove the state monad.
  *)
  let[@landmark] build (m : Embedded.t m) : Embedded.t =
    let cont, resulting_bindings = run_identity m [] in
    List.fold resulting_bindings ~init:cont ~f:(fun cont -> function
      | Bind (id, body) ->
        ELet { var = id ; body ; cont }
      | Ignore ignored ->
        EIgnore { ignored ; cont }
    )
end

module Embedded_type (W : sig val do_wrap : bool end) = struct
  type labels = [ `Gen | `Check | `Wrap | `All ]

  (*
    Note: then gen body gets frozen.

    When we make a record with these labels, we might now ahead of time that we're only asking
    for one of the labels (or maybe we do need all of them). For this reason, we have
    `ask_for`, where we say which labels we're asking for.
  *)
  let make ~(ask_for : labels) ~(gen : Embedded.t Lazy.t) ~(check : Embedded.t Lazy.t) ~(wrap : Embedded.t Lazy.t) : Embedded.t =
    let record_body =
      match ask_for with
      | `All ->
        [ (Reserved_labels.Records.gen, Expr.EFreeze (force gen))
        ; (Reserved_labels.Records.check, (force check))
        ] @
        if W.do_wrap
        then [ (Reserved_labels.Records.wrap, (force wrap)) ]
        else []
      | `Gen -> [ (Reserved_labels.Records.gen, Expr.EFreeze (force gen)) ]
      | `Check -> [ (Reserved_labels.Records.check, (force check)) ]
      | `Wrap -> if W.do_wrap then [ (Reserved_labels.Records.wrap, (force wrap)) ] else []
    in
    ERecord (Parsing_tools.record_of_list record_body)

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

let uses_id (expr : Desugared.t) (id : Ident.t) : bool =
  let rec loop (e : Desugared.t) : bool =
    match e with
    | (EInt _ | EBool _ | EPick_i | EAbort | EDiverge | EType | ETypeInt | ETypeBool | ETypeTop | ETypeBottom) -> false
    | EVar id' -> Ident.equal id id'
    (* capturing variables *)
    | ELet { var ; body ; _ } when Ident.equal var id -> loop body
    | EFunction { param ; _ } when Ident.equal param id -> false
    | ETypeMu { var ; _  } when Ident.equal var id -> false
    | ETypeArrowD { binding ; domain ; _ } when Ident.equal binding id -> loop domain
    | ELetFlagged { typed_var = { var ; tau } ; body ; _  } when Ident.equal var id -> loop tau || loop body
    | ELetTyped { typed_var = { var ; tau } ; body ; _ } when Ident.equal var id -> loop tau || loop body
    (* simple unary cases *)
    | EFunction { body = e ; _ }
    | ENot e
    | EVariant { payload = e ; _ }
    | ETypeMu { body = e ; _ }
    | ETypeSingle e
    | EProject { record = e ; _ } -> loop e
    (* simple binary cases *)
    | ELet { body = e1 ; cont = e2 ; _ }
    | EAppl { func = e1 ; arg = e2 }
    | EBinop { left = e1 ; right = e2 ; _ }
    | ETypeArrow { domain = e1 ; codomain = e2 }
    | ETypeArrowD { domain = e1 ; codomain = e2 ; _ }
    | ETypeRefinement { tau = e1 ; predicate = e2 } -> loop e1 || loop e2
    (* special cases *)
    | ERecord m -> Map.exists m ~f:loop
    | ETypeRecord m -> Map.exists m ~f:loop
    | ETypeRecordD m -> List.fold_until m ~init:false ~f:(fun acc (label, e) ->
        let RecordLabel label_id = label in
        let res = acc || loop e in
        if Ident.equal label_id id
        then Stop res (* stop because id is bound to this label in later labels *)
        else Continue res (* continue to check remaining labels after this *)
      ) ~finish:Fn.id
    | ETypeVariant ls -> List.exists ls ~f:(fun (_, e) -> loop e)
    | ETypeIntersect ls -> List.exists ls ~f:(fun (_, e1, e2) -> loop e1 || loop e2)
    | EMatch { subject ; patterns } -> loop subject || List.exists patterns ~f:(fun (_, e) -> loop e)
    | EIf { cond ; true_body ; false_body } -> loop cond || loop true_body || loop false_body
    | ELetFlagged { typed_var = { tau ; _ } ; body ; cont ; _ }
    | ELetTyped { typed_var = { tau ; _ } ; body ; cont } -> loop tau || loop body || loop cont
  in
  loop expr

let embed_pgm (names : (module Fresh_names.S)) (pgm : Desugared.pgm) ~(do_wrap : bool) : Embedded.pgm =
  let module E = Embedded_type (struct let do_wrap = do_wrap end) in
  let module Names = (val names) in
  let open LetMonad (Names) in

  let fresh_abstraction (type a) (suffix : string) (e : Ident.t -> a Expr.t) : a Expr.t =
    let id = Names.fresh_id ~suffix () in
    EFunction { param = id ; body = e id }
  in

  let cur_mu_vars : Ident.t list ref = ref [] in (* FIXME : don't use ref for this *)

  let rec embed ?(ask_for : E.labels = `All) (expr : Desugared.t) : Embedded.t =
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
    | ELetTyped { typed_var ; body ; cont } ->
      Program.stmt_to_expr (embed_statement (STyped { typed_var ; body })) (embed cont)
    | ELetFlagged { flags ; typed_var ; body ; cont } ->
      Program.stmt_to_expr (embed_statement (SFlagged { flags ; typed_var ; body })) (embed cont)
    (* types *)
    | ETypeInt ->
      E.make
        ~ask_for
        ~gen:(lazy EPick_i)
        ~check:(lazy (
          fresh_abstraction "e_int_check" (fun e ->
            build @@
              let%bind () = ignore (EBinop { left = EVar e ; binop = BPlus ; right = EInt 0 }) in
              return (EBool true)
          )
        ))
        ~wrap:(lazy EId)
    | ETypeBool ->
      E.make
        ~ask_for
        ~gen:(lazy EPick_b)
        ~check:(lazy (
          fresh_abstraction "e_bool_check" (fun e ->
            build @@
              let%bind () = ignore (ENot (EVar e)) in
              return (EBool true)
          
          )
        ))
        ~wrap:(lazy EId)
    | ETypeArrow { domain = tau1 ; codomain = tau2 } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          fresh_abstraction "arg_arrow_gen" (fun arg ->
            EIf
              { cond = check tau1 (EVar arg)
              ; true_body = gen tau2
              ; false_body = EAbort
              }
          )
        ))
        ~check:(lazy (
          fresh_abstraction "e_arrow_check" (fun e ->
            check tau2 (
              apply (EVar e) (gen tau1)
            )
          )
        ))
        ~wrap:(lazy (
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
        ))
    | ETypeRecord m ->
      E.make
        ~ask_for
        ~gen:(lazy (ERecord (Map.map m ~f:gen)))
        ~check:(lazy (
          fresh_abstraction "e_rec_check" (fun e ->
            build @@
              let%bind () =
                iter (Map.to_alist m) ~f:(fun (label, tau) ->
                  ignore (check tau (proj (EVar e) label))
                )
                in
                return (EBool true)
          )
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_rec_wrap" (fun e ->
            ERecord (
              Map.mapi m ~f:(fun ~key:label ~data:tau ->
                wrap tau (proj (EVar e) label)
              )
            )
          )
        ))
    | ETypeRecordD ls ->
      E.make
        ~ask_for
        ~gen:(lazy (
          build @@
            let%bind () =
              iter ls ~f:(fun (RecordLabel label_id, tau) ->
                assign label_id @@ gen tau
              )
            in
            return (ERecord (RecordLabel.Map.of_alist_exn
              @@ List.map ls ~f:(fun (((RecordLabel label_id) as l), _) -> l, EVar (label_id))
            ))
        ))
        ~check:(lazy (
          fresh_abstraction "e_dep_rec_check" (fun e ->
            build @@
              let%bind () =
                iter ls ~f:(fun (RecordLabel label_id as l, tau) ->
                  let%bind () = ignore @@ check tau (proj (EVar e) l) in
                  assign label_id @@ proj (EVar e) l
                )
              in
              return (EBool true)
          )
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_dep_rec_wrap" (fun e ->
            build @@
              let%bind () =
                iter ls ~f:(fun (RecordLabel label_id as l, tau) ->
                  assign label_id @@ wrap tau (proj (EVar e) l)
                )
              in
              return (ERecord (RecordLabel.Map.of_alist_exn
                @@ List.map ls ~f:(fun (((RecordLabel label_id) as l), _) -> l, EVar (label_id))
              ))
          )
        ))
    | EType ->
      E.make
        ~ask_for
        ~gen:(lazy (
          (* let a = Names.fresh_poly_value () in *)
          build @@
            let%bind i = capture EPick_i in
            return @@
            E.make
              ~ask_for:`All
              ~gen:(lazy (EVariant { label = Reserved_labels.Variants.untouched ; payload = EVar i }))
              ~check:(lazy (
                fresh_abstraction "e_alpha_check" (fun e ->
                  EMatch { subject = EVar e ; patterns =
                    let v = Names.fresh_id ~suffix:"v" () in
                    [ (PVariant { variant_label = Reserved_labels.Variants.untouched ; payload_id = v }
                      , EIf
                          { cond = EBinop { left = EVar v ; binop = BEqual ; right = EVar i }
                          ; true_body = EBool true
                          ; false_body = EAbort 
                          })
                    ]
                  }
                )
              ))
              ~wrap:(lazy EId)
        ))
        ~check:(lazy (
          fresh_abstraction "e_type_check" (fun e ->
            build @@  
              let e = EVar e in
              let%bind () = ignore @@ proj e Reserved_labels.Records.gen in
              let%bind () = ignore @@ proj e Reserved_labels.Records.check in
              let%bind () = ignore @@ proj e Reserved_labels.Records.wrap in
              return (EBool true)
          )
        ))
        ~wrap:(lazy EId)
    | ETypeArrowD { binding = x ; domain = tau1 ; codomain = tau2 } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          fresh_abstraction "xp_arrowd_gen" (fun x' ->
            EIf
              { cond = check tau1 (EVar x')
              ; true_body = apply (EFunction { param = x ; body = gen tau2}) (EVar x')
              ; false_body = EAbort
              }
          )
        ))
        ~check:(lazy (
          fresh_abstraction "e_arrowd_check"( fun e ->
            build @@
              let%bind arg = capture @@ gen tau1 in
              return
              @@ appl_list
                  (EFunction { param = x ; body = proj (embed tau2) Reserved_labels.Records.check })
                  [ (EVar arg) ; apply (EVar e) (EVar arg) ]
          )
        ))
        ~wrap:(lazy (
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
        ))
    | ETypeRefinement { tau ; predicate = e_p } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          build @@
            let%bind gend = capture @@ gen tau in
            return (EIf
              { cond = apply (embed e_p) (EVar gend)
              ; true_body = EVar gend
              ; false_body = EDiverge
              }
            )
        ))
        ~check:(lazy (
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
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_ref_wrap" (fun e ->
            wrap tau (EVar e)
          )
        ))
    | ETypeVariant e_variant_type ->
      let e_variant_ls =
        List.map e_variant_type ~f:(fun (type_label, tau) ->
          VariantTypeLabel.to_variant_label type_label, tau
        )
      in
      E.make
        ~ask_for
        ~gen:(lazy (
          let of_case_list ls =
            ECase
              { subject = EPick_i
              ; cases =
                List.tl_exn ls
                |> List.mapi ~f:(fun i (label, tau) ->
                  i + 1, EVariant { label ; payload = gen tau }
                )
              ; default = 
                let (last_label, last_tau) = List.hd_exn ls in
                EVariant { label = last_label ; payload = gen last_tau }
              }
          in
          let unlikely, likely =
            List.partition_tf e_variant_ls ~f:(fun (_, tau) -> 
              List.exists !cur_mu_vars ~f:(fun id -> uses_id tau id)
            )
          in
          match unlikely, likely with
          | [], _ | _, [] -> of_case_list e_variant_ls (* either was empty, so just put all flat *)
          | _ ->
            EIf
              { cond = EBinop { left = EPick_i ; binop = BEqual ; right = EInt 10 } (* unlikely but not THAT unlikely to choose *)
              ; true_body = of_case_list unlikely
              ; false_body = of_case_list likely
              }

        ))
        ~check:(lazy (
          fresh_abstraction "e_var_check" (fun e ->
            EMatch { subject = EVar e ; patterns =
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , check tau (EVar v)
                )
            }  
          )
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_var_wrap" (fun e ->
            EMatch { subject = EVar e ; patterns = 
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , EVariant { label = variant_label ; payload = wrap tau (EVar v) }
                )
            }  
          )
        ))
    | ETypeMu { var = b ; body = tau } ->
      cur_mu_vars := b :: !cur_mu_vars;
      let res =
        apply (
          apply Embedded_functions.y_comb (
            fresh_abstraction "self_mu" (fun self ->
              fresh_abstraction "dummy_mu" (fun _dummy ->
                let appl_self_0 = apply (EVar self) (EInt 0) in
                E.make
                  ~ask_for
                  ~gen:(lazy (
                    apply (EFunction { param = b ; body = gen tau }) appl_self_0
                  ))
                  ~check:(lazy (
                    fresh_abstraction "e_mu_check" (fun e ->
                      apply (EFunction { param = b ; body = check tau (EVar e) }) appl_self_0
                    )
                  ))
                  ~wrap:(lazy (
                    fresh_abstraction "e_mu_wrap" (fun e ->
                      apply (EFunction { param = b ; body = wrap tau (EVar e) }) appl_self_0
                    )
                  ))
              )
            )
          )
        ) (EInt 0)
      in
      cur_mu_vars := List.tl_exn !cur_mu_vars;
      res
    | ETypeIntersect e_intersect_type ->
      let e_intersect_ls = 
        List.map e_intersect_type ~f:(fun (type_label, tau, tau') ->
          VariantTypeLabel.to_variant_label type_label, tau, tau'
        )
      in
      E.make
        ~ask_for
        ~gen:(lazy (
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
        ))
        ~check:(lazy (
          fresh_abstraction "e_inter_check" (fun e ->
            ECase
              { subject = EPick_i
              ; cases =
                List.tl_exn e_intersect_type
                |> List.mapi ~f:(fun i (label, tau, tau') ->
                  i + 1, check (
                    ETypeArrow { domain = ETypeVariant [ (label, tau) ] ; codomain = tau' }
                ) (EVar e) 
              )
              ; default =
                let (last_label, last_tau, last_tau') = List.hd_exn e_intersect_type in
                check (
                  ETypeArrow { domain = ETypeVariant [ (last_label, last_tau) ] ; codomain = last_tau' }
                ) (EVar e)
              }
          )
        ))
        ~wrap:(lazy (
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
        ))
    | ETypeTop ->
      E.make
        ~ask_for
        ~gen:(lazy (EVariant { label = Reserved_labels.Variants.top ; payload = EInt 0 }))
        ~check:(lazy (fresh_abstraction "e_top_check" (fun _ -> EBool (true))))
        ~wrap:(lazy EId)
    | ETypeBottom ->
      E.make
        ~ask_for
        ~gen:(lazy EDiverge)
        ~check:(lazy (fresh_abstraction "e_top_check" (fun _ -> EAbort)))
        ~wrap:(lazy EId)
    | ETypeSingle tau ->
      E.make
        ~ask_for
        ~gen:(lazy (embed tau))
        (* Note: check and wrap are just copied code from the EType embedding *)
        ~check:(lazy (
          fresh_abstraction "e_singlet_check" (fun e ->
            build @@  
              let e = EVar e in
              let%bind () = ignore @@ proj e Reserved_labels.Records.gen in
              let%bind () = ignore @@ proj e Reserved_labels.Records.check in
              let%bind () = ignore @@ proj e Reserved_labels.Records.wrap in
              return (EBool true)
          )
        ))
        ~wrap:(lazy EId)

    and embed_let ?(v_name : Ident.t = Names.fresh_id ()) ~(do_check : bool)
      ~(do_wrap : bool) ~(tau : Desugared.t) (body : Desugared.t) : Embedded.t =
      build @@
        let%bind () = assign v_name @@ embed body in
        let%bind r = capture ~suffix:"r" @@ embed tau in
        let%bind () = 
          if do_check
          then ignore @@ E.check ~tau:(EVar r) (EVar v_name)
          else return ()
        in
        if do_wrap
        then return @@ E.wrap ~tau:(EVar r) (EVar v_name)
        else return (EVar v_name)

    and embed_pattern (pat : Desugared.pattern) : Embedded.pattern =
      match pat with
      | (PAny | PVariable _ | PVariant _) as p -> p

    and gen (tau : Desugared.t) : Embedded.t =
      E.gen (embed ~ask_for:`Gen tau)

    and check (tau : Desugared.t) (x : Embedded.t) : Embedded.t =
      E.check ~tau:(embed ~ask_for:`Check tau) x

    and wrap (tau : Desugared.t) (x : Embedded.t) : Embedded.t =
      E.wrap ~tau:(embed ~ask_for:`Wrap  tau) x

    and embed_statement (stmt : Desugared.statement) : Embedded.statement =
      match stmt with
      | SUntyped { var ; body } ->
        SUntyped { var ; body = embed body }
      | STyped { typed_var = { var = x ; tau } ; body } ->
        SUntyped { var = x ; body = embed_let ~do_wrap ~do_check:true ~tau body }
      | SFlagged { flags ; typed_var = { var = x ; tau } ; body } ->
        let do_check = not @@ Set.mem flags NoCheck in
        let v_name = if Set.mem flags TauKnowsBinding then x else Names.fresh_id () in
        SUntyped { var = x ; body = embed_let ~v_name ~do_wrap ~do_check ~tau body }
  in
  List.map pgm ~f:embed_statement
