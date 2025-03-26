
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
    type a = Constraints.embedded
    type t =
      | Bind of Ident.t * Embedded.t
      | Ignore of Embedded.t

    let t_to_expr tape ~cont =
      match tape with
      | Bind (id, body) ->
        ELet { var = id ; body ; cont }
      | Ignore ignored ->
        EIgnore { ignored ; cont }
  end

  include Let_builder (Binding)

  open Binding

  (*
    Capture the expression under a fresh name and return that name.
  *)
  let capture ?(suffix : string option) (e : Embedded.t) : Ident.t m =
    let v = Names.fresh_id ?suffix () in
    let%bind () = tell (Bind (v, e)) in
    return v

  (*
    Assign the expression the given name.
  *)
  let assign (id : Ident.t) (e : Embedded.t) : unit m =
    tell (Bind (id, e))

  (*
    Compute the value of the expression but ignore it.
  *)
  let ignore (e : Embedded.t) : unit m =
    tell (Ignore e)
end

module Embedded_type (W : sig val do_wrap : bool end) = struct
  type labels = [ `Gen | `Check | `Wrap | `All ]

  (*
    Note: then gen body always gets frozen with the `EFreeze` constructor, so the caller
      does not need to do that.

    When we make a record with these labels, we might now ahead of time that we're only asking
    for one of the labels (or maybe we do need all of them). For this reason, we have
    `ask_for`, where we say which labels we're asking for.
  *)
  let make ~(ask_for : labels) ~(gen : Embedded.t Lazy.t) ~(check : Embedded.t Lazy.t) ~(wrap : Embedded.t Lazy.t) : Embedded.t =
    let record_body =
      match ask_for with
      | `All ->
        [ (Reserved.gen, Expr.EFreeze (force gen))
        ; (Reserved.check, (force check))
        ] @
        if W.do_wrap
        then [ (Reserved.wrap, (force wrap)) ]
        else []
      | `Gen -> [ (Reserved.gen, Expr.EFreeze (force gen)) ]
      | `Check -> [ (Reserved.check, (force check)) ]
      | `Wrap -> if W.do_wrap then [ (Reserved.wrap, (force wrap)) ] else []
    in
    ERecord (Parsing_tools.record_of_list record_body)

  (*
    Applies arg to tau's check, which is partially evaluated if possible.
  *)
  let check ~(tau : Embedded.t) (arg : Embedded.t) : Embedded.t =
    apply
      (proj tau Reserved.check)
      arg

  (*
    Thaws the gen from the given tau, where the record projection on "gen" might be partially evaluated.
  *)
  let gen (tau : Embedded.t) : Embedded.t =
    EThaw (proj tau Reserved.gen)

  (*
    Applies arg to tau's wrap, which is partially evaluated if possible.
  *)
  let wrap ~(tau : Embedded.t) (x : Embedded.t) : Embedded.t =
    assert W.do_wrap;
    apply
      (proj tau Reserved.wrap)
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
    | EMatch { subject ; patterns } -> loop subject || List.exists patterns ~f:(fun (_, e) -> loop e)
    | EIf { cond ; true_body ; false_body } -> loop cond || loop true_body || loop false_body
    | ELetTyped { typed_var = { tau ; _ } ; body ; cont ; _ } -> loop tau || loop body || loop cont
  in
  loop expr

let embed_pgm (names : (module Fresh_names.S)) (pgm : Desugared.pgm) ~(do_wrap : bool) : Embedded.pgm =
  let module E = Embedded_type (struct let do_wrap = do_wrap end) in
  let module Names = (val names) in
  let open LetMonad (Names) in

  (* alias because sometimes we shadow do_wrap, and we need this to embed let-expressions *)
  let wrap_flag = do_wrap in

  let fresh_abstraction (type a) (suffix : string) (e : Ident.t -> a Expr.t) : a Expr.t =
    let id = Names.fresh_id ~suffix () in
    EFunction { param = id ; body = e id }
  in

  let cur_mu_vars : Ident.t Stack.t = Stack.create () in

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
    | ELetTyped { typed_var ; body ; cont ; do_wrap ; do_check } ->
      Program.stmt_to_expr (embed_statement (STyped { typed_var ; body ; do_wrap ; do_check })) (embed cont)
    (* types *)
    | ETypeInt ->
      E.make
        ~ask_for
        ~gen:(lazy EPick_i)
        ~check:(lazy (
          fresh_abstraction "e_int_check" @@ fun e ->
            build @@
              let%bind () = ignore (EBinop { left = EVar e ; binop = BPlus ; right = EInt 0 }) in
              return unit_value
        ))
        ~wrap:(lazy EId)
    | ETypeBool ->
      E.make
        ~ask_for
        ~gen:(lazy EPick_b)
        ~check:(lazy (
          fresh_abstraction "e_bool_check" @@ fun e ->
            build @@
              let%bind () = ignore (ENot (EVar e)) in
              return unit_value
        ))
        ~wrap:(lazy EId)
    | ETypeArrow { domain = tau1 ; codomain = tau2 } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          fresh_abstraction "arg_arrow_gen" @@ fun arg ->
            build @@
              let%bind () = ignore (check tau1 (EVar arg)) in
              return @@ gen tau2
        ))
        ~check:(lazy (
          fresh_abstraction "e_arrow_check" @@ fun e ->
            check tau2 @@ apply (EVar e) (gen tau1)
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_arrow_wrap" @@ fun e ->
            fresh_abstraction "x_arrow_wrap" @@ fun x ->
              build @@
                let%bind () = ignore (check tau1 (EVar x)) in
                return @@ wrap tau2 (
                  apply (EVar e) (wrap tau1 (EVar x))
                )
        ))
    | ETypeRecord m ->
      E.make
        ~ask_for
        ~gen:(lazy (ERecord (Map.map m ~f:gen)))
        ~check:(lazy (
          fresh_abstraction "e_rec_check" @@ fun e ->
            build @@
              let%bind () =
                iter (Map.to_alist m) ~f:(fun (label, tau) ->
                  ignore (check tau (proj (EVar e) label))
                )
              in
              return unit_value
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_rec_wrap"  @@ fun e ->
            ERecord (
              Map.mapi m ~f:(fun ~key:label ~data:tau ->
                wrap tau (proj (EVar e) label)
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
          fresh_abstraction "e_dep_rec_check" @@ fun e ->
            build @@
              let%bind () =
                iter ls ~f:(fun (RecordLabel label_id as l, tau) ->
                  let%bind () = ignore @@ check tau (proj (EVar e) l) in
                  assign label_id @@ proj (EVar e) l
                )
              in
              return unit_value
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_dep_rec_wrap" @@ fun e ->
            build @@
              let%bind () =
                iter ls ~f:(fun (RecordLabel label_id as l, tau) ->
                  assign label_id @@ wrap tau (proj (EVar e) l)
                )
              in
              return (ERecord (RecordLabel.Map.of_alist_exn
                @@ List.map ls ~f:(fun (((RecordLabel label_id) as l), _) -> l, EVar (label_id))
              ))
        ))
    | EType ->
      E.make
        ~ask_for
        ~gen:(lazy (
          build @@
            let%bind i = capture EPick_i in
            return @@
            E.make
              ~ask_for:`All
              ~gen:(lazy (EVariant { label = Reserved.untouched ; payload = EVar i }))
              ~check:(lazy (
                fresh_abstraction "e_alpha_check" @@ fun e ->
                  EMatch { subject = EVar e ; patterns =
                    let v = Names.fresh_id ~suffix:"v" () in
                    [ (PVariant { variant_label = Reserved.untouched ; payload_id = v }
                      , EIf
                          { cond = EBinop { left = EVar v ; binop = BEqual ; right = EVar i }
                          ; true_body = unit_value
                          ; false_body = EBinop { left = EVar e ; binop = BEqual ; right = EVariant { label = Reserved.untouched ; payload = EVar i }}
                          })
                    ]
                  }
              ))
              ~wrap:(lazy EId)
        ))
        ~check:(lazy (
          fresh_abstraction "e_type_check" @@ fun e ->
            build @@  
              let e = EVar e in
              let%bind () = ignore @@ proj e Reserved.gen in
              let%bind () = ignore @@ proj e Reserved.check in
              let%bind () = if do_wrap then ignore @@ proj e Reserved.wrap else return () in
              return unit_value
        ))
        ~wrap:(lazy EId)
    | ETypeArrowD { binding = x ; domain = tau1 ; codomain = tau2 } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          fresh_abstraction "xp_arrowd_gen" @@ fun x' ->
            build @@
              let%bind () = ignore (check tau1 (EVar x')) in
              return @@ apply (EFunction { param = x ; body = gen tau2 }) (EVar x')
        ))
        ~check:(lazy (
          fresh_abstraction "e_arrowd_check" @@ fun e ->
            build @@
              let%bind arg = capture @@ gen tau1 in
              return @@ appl_list
                (EFunction { param = x ; body = proj (embed ~ask_for:`Check tau2) Reserved.check })
                [ (EVar arg) ; apply (EVar e) (EVar arg) ]
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_arrowd_wrap" @@ fun e ->
            fresh_abstraction "xp_arrowd_wrap" @@ fun x' ->
              build @@
                let%bind () = ignore (check tau1 (EVar x')) in
                return @@ appl_list
                  (EFunction { param = x ; body = proj (embed ~ask_for:`Wrap tau2) Reserved.wrap })
                  [ (EVar x') ; apply (EVar e) (wrap tau1 (EVar x')) ]
        ))
    | ETypeRefinement { tau ; predicate = e_p } ->
      E.make
        ~ask_for
        ~gen:(lazy (
          build @@
            let%bind gend = capture @@ gen tau in
            return @@ EIf
              { cond = apply (embed e_p) (EVar gend)
              ; true_body = EVar gend
              ; false_body = EDiverge
              }
        ))
        ~check:(lazy (
          fresh_abstraction "e_ref_check" @@ fun e ->
            build @@
              let%bind () = ignore @@ check tau (EVar e) in
              return @@ EIf
                { cond = apply (embed e_p) (EVar e)
                ; true_body = unit_value
                ; false_body =
                  apply 
                    (EVariant { label = Reserved.predicate_failed ; payload = EVar e })
                    (embed e_p)
                }
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_ref_wrap" @@ fun e ->
            wrap tau (EVar e)
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
          let of_case_list = function
            | [] -> failwith "invalid empty variant"
            | [ (label, tau) ] -> EVariant { label ; payload = gen tau } (* no case needed on one variant *)
            | ls ->
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
              Stack.exists cur_mu_vars ~f:(fun id -> uses_id tau id)
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
          fresh_abstraction "e_var_check" @@ fun e ->
            EMatch { subject = EVar e ; patterns =
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , check tau (EVar v)
                )
            }  
        ))
        ~wrap:(lazy (
          fresh_abstraction "e_var_wrap" @@ fun e ->
            EMatch { subject = EVar e ; patterns = 
              let v = Names.fresh_id () in
              List.map e_variant_ls ~f:(fun (variant_label, tau) ->
                PVariant { variant_label ; payload_id = v }
                , EVariant { label = variant_label ; payload = wrap tau (EVar v) }
                )
            }  
        ))
    | ETypeMu { var = b ; body = tau } ->
      Stack.push cur_mu_vars b;
      let res =
        EThaw (apply Embedded_functions.y_freeze_thaw @@ 
          fresh_abstraction "self_mu" @@ fun self ->
            EFreeze (
              let thaw_self = EThaw (EVar self) in
              E.make
                ~ask_for
                ~gen:(lazy (
                  apply (EFunction { param = b ; body = gen tau }) thaw_self
                ))
                ~check:(lazy (
                  fresh_abstraction "e_mu_check" @@ fun e ->
                    apply (EFunction { param = b ; body = check tau (EVar e) }) thaw_self
                ))
                ~wrap:(lazy (
                  fresh_abstraction "e_mu_wrap" @@ fun e ->
                    apply (EFunction { param = b ; body = wrap tau (EVar e) }) thaw_self
                ))
              )
        )
      in
      let _ = Stack.pop_exn cur_mu_vars in
      res
    | ETypeTop ->
      E.make
        ~ask_for
        ~gen:(lazy (EVariant { label = Reserved.top ; payload = unit_value }))
        ~check:(lazy (fresh_abstraction "e_top_check" @@ fun _ -> unit_value))
        ~wrap:(lazy EId)
    | ETypeBottom ->
      E.make
        ~ask_for
        ~gen:(lazy EDiverge)
        ~check:(lazy (
          fresh_abstraction "e_top_check" @@ fun _ ->
            apply
              (EVariant { label = Reserved.bottom ; payload = unit_value })
              (EVariant { label = Reserved.bottom ; payload = unit_value })
        ))
        ~wrap:(lazy EId)
    | ETypeSingle tau ->
      E.make
        ~ask_for
        ~gen:(lazy (embed tau))
        (* Note: check and wrap refer to the EType embedding, per the specification *)
        ~check:(lazy (proj (embed ~ask_for:`Check EType) Reserved.check))
        ~wrap:(lazy (proj (embed ~ask_for:`Wrap EType) Reserved.wrap))

    and embed_let ?(do_wrap : bool = do_wrap) ~(do_check : bool) ~(tau : Desugared.t) (body : Desugared.t) : Embedded.t =
      build @@
        let%bind v = capture @@ embed body in
        let%bind () = 
          if do_check
          then ignore @@ check tau (EVar v)
          else return ()
        in
        (* wrap_flag here is the external do_wrap that tells use whether we *ever* wrap *)
        if wrap_flag && do_wrap
        then return @@ wrap tau (EVar v)
        else return (EVar v)

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
      | STyped { typed_var = { var = x ; tau } ; body ; do_wrap ; do_check } ->
        SUntyped { var = x ; body = embed_let ~do_wrap ~do_check ~tau body }
    in

  let embed_single_program (pgm : Desugared.pgm) =
    List.map pgm ~f:embed_statement
  in

  embed_single_program pgm

(* 
  Split the program into many different programs, where each one has a different check turned on, and the rest are off.

  Note:
  * This is somewhat inefficient because we translate the program once for each version, so we are duplicating work.
  * We could do this really intelligently, but right now it doesn't matter.
*)
let split_checks (stmt_ls : Desugared.statement list) : Desugared.pgm Preface.Nonempty_list.t =
  let has_check (stmt : Desugared.statement) : bool =
    match stmt with
    | SUntyped _ -> false
    | STyped { do_check ; _ } -> do_check
  in
  let turn_off_check (stmt : Desugared.statement) : Desugared.statement =
    match stmt with
    | SUntyped _ -> stmt
    | STyped st ->
      STyped { st with do_check = false }
  in
  (*
    Now for each statement with a check, we want to return the program with only that check on.
  *)
  let rec go pgms prev_stmts stmts =
    match stmts with
    | [] -> pgms
    | stmt :: tl ->
      if has_check stmt
      then
        let new_pgm =
          prev_stmts
          @ [ stmt ]
          @ List.map tl ~f:turn_off_check
        in
        go (new_pgm :: pgms) (prev_stmts @ [ turn_off_check stmt ]) tl
      else
        go pgms (prev_stmts @ [ stmt ]) tl
  in
  match Preface.Nonempty_list.from_list @@ go [] [] stmt_ls with
  | None -> Preface.Nonempty_list.Last stmt_ls
  | Some pgm_ls -> pgm_ls

let embed_fragmented (names : (module Fresh_names.S)) (pgm : Desugared.pgm) ~(do_wrap : bool) : Embedded.pgm Preface.Nonempty_list.t =
  Preface.Nonempty_list.map (fun pgm -> embed_pgm names pgm ~do_wrap)
  @@ split_checks pgm
