open Core
open Dj_common
open Jayil
open Jayil.Ast
open Rule
open Types
module U = Unrolls.U_dbmc
module Log = Log.Export.CMLog
module Riddler = Riddler.V1

(*
   Status promotion:
   The status of a lookup depends om the status of messages from its sublookups.
   The status promotion is a lattice and any status can only be promoted once.

   The workflow is:
   1. If not top-status:
   1.a.1 check the status from the incoming message
   1.b.1 check the sub-lookups to get the current status
   1.b.2     record the current status
   1.b.3 promote the status in the message if necessary

   The difference of 1.a and 1.b comes from whether 
*)

let fold_lookups_status map lookups_with_pre =
  let lookups = List.map ~f:snd lookups_with_pre in
  let open Lookup_status in
  List.fold_until lookups ~init:Good
    ~f:(fun acc_s lookup ->
      match Hashtbl.find map lookup with
      | Some (detail : Lookup_detail.t) -> (
          match (detail.status, acc_s) with
          | Good, _ -> Stop Good
          | Complete, _ -> Continue Complete
          | Fail, Complete -> Continue Complete
          | Fail, _ -> Continue Fail)
      | None -> Stop Good)
    ~finish:Fn.id

let set_status (detail : Lookup_detail.t) status = detail.status <- status

let set_status_gen_phi (detail : Lookup_detail.t) status =
  detail.status_gen_phi <- status

let change_status old_status new_status =
  let open Lookup_status in
  match (old_status, new_status) with
  | Complete, _ -> None (* failwith "[complete] why here" *)
  | Fail, _ -> None (* failwith "[fail] why here" *)
  | Good, Good -> Some Good
  | Good, _ -> Some new_status

let promote_result (target : Lookup_key.t) map (detail : Lookup_detail.t)
    new_status (v : Lookup_key.t) =
  match change_status detail.status new_status with
  | Some Complete ->
      (* detail Complete ; *)
      Some Lookup_result.(from_as v Complete)
  | Some Fail ->
      (* set_status detail Fail ; *)
      Some Lookup_result.(from_as v Fail)
  | Some Good -> Some Lookup_result.(from_as v Good)
  | None -> None

module N = Unroll.Naive_state_machine

type stream_result =
  | Real_stream of U.message Lwt_stream.t * Lookup_key.t list
  | Just_status of N.t

let join_result srcs =
  let streams =
    List.map srcs ~f:(function
      | Real_stream (s, deps) -> s
      | Just_status _ -> failwith "not here")
  in
  let deps =
    List.map srcs ~f:(function
      | Real_stream (s, deps) -> deps
      | Just_status _ -> failwith "not here")
    |> List.join
  in
  (streams, deps)

(* TODO:
   sub_preconds is used to prioritize tasks, which can be removed.
   sub_lookups is used to complete constraints, which should be kept;
   however, sublookup is obvious a dependency, which should be able to be tracked inside
   unroll (not here)

   what is sublookup after all, saying we have

   A <- bind B (Bv_i -> C_i)
   C_i <- on D E

   A depends on B, C_i, D, E; or to say
   A's completion depends on the completion of all B, C_i, D, E.

   However, the constaints may also depend on some of the lookup
   e.g. in Fun_enter and Fun_exit, when the lookup is complete,
   the encoding can change from the template getting from the analysis from
   the template getting from the actual possible values.

   However, in this sense, we don't need to track the sub_lookup any way,
   we can directly ask for a key's possible values in domains.
*)
let run_action dispatch unroll (state : Global_state.t)
    (detail : Lookup_detail.t) target source =
  let open Rule_action in
  let add_phi = Global_state.add_phi state detail in
  let set_status = set_status detail in
  let set_status_gen_phi = set_status_gen_phi detail in
  let add_to_domain v = detail.domain <- detail.domain @ [ v ] in
  (* let add_sub_preconds cond =
       detail.sub_preconds <- detail.sub_preconds @ [ cond ]
     in *)
  (* let add_sublookup pre_pair key =
       detail.sub_lookups <- detail.sub_lookups @ [ (pre_pair, key) ]
     in *)
  let promote_result =
    promote_result target state.search.lookup_detail_map detail
  in

  let rec stream_of_action source =
    match source with
    | Leaf Fail ->
        (* set_status Lookup_status.Fail ;
           set_status_gen_phi Lookup_status.Fail ; *)
        (* U.set_status unroll target N.Fail *)
        Just_status N.Fail
    | Leaf Complete ->
        add_to_domain target ;
        Real_stream (U.one_shot unroll [ Lookup_result.complete target ], [])
        (* U.set_status unroll target N.Done *)
        (* set_status Lookup_status.Complete ;
             set_status_gen_phi Lookup_status.Complete ; *)
    | Leaf _ -> failwith "incorrect leaf status"
    | Direct e ->
        (* (match sub_lookup with
           | Some parent -> add_sublookup parent e.pub
           | None -> ()) ; *)
        Real_stream
          ( U.map unroll e.pub (fun r ->
                add_to_domain r.from ;
                r),
            [ e.pub ] )
    (* The difference between Map and MapSeq is the pre-and-post handler around `f` *)
    | Map e ->
        Real_stream
          ( U.map unroll e.pub (fun r ->
                let r' = Lookup_result.good (e.map r.from) in
                add_to_domain r'.from ;
                r'),
            [ e.pub ] )
    (* U.filter_map unroll e.pub target (fun r ->
            let r' = promote_result r.status (e.map r.from) in
            Option.iter r' ~f:(fun r ->
                Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from)) ;
            r') ; *)
    | MapSeq e ->
        Global_state.create_counter state detail target ;
        (* let f r =
             let i = Global_state.fetch_counter state target in
             let r', phis = e.map i r in
             add_phi (Riddler.list_append target i (Riddler.and_ phis)) ;
             Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from) ;
             let r' = promote_result r.status r'.from in
             Option.iter r' ~f:(fun r ->
                 Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from)) ;
             r'
           in
           U.filter_map unroll e.pub target f ; *)
        let f r =
          let i = Global_state.fetch_counter state target in
          let r', phis = e.map r in
          add_to_domain r'.from ;
          add_phi (Riddler.list_append target i (Riddler.and_ phis)) ;
          let r' = Lookup_result.good r'.from in
          r'
        in
        Real_stream (U.map unroll e.pub f, [ e.pub ])
    | Both e ->
        Real_stream
          ( U.map2 unroll e.pub1 e.pub2 (fun (v1, v2) ->
                Lookup_result.good target),
            [ e.pub1; e.pub2 ] )
        (* U.joini unroll [ e.pub1; e.pub2 ] target (fun (_i, vi) ->
            Lookup_result.good target) ; *)
    | Bind_like e ->
        if not e.bounded then Global_state.create_counter state detail target ;
        (* let precond = ref false in *)
        (* add_sub_preconds precond ; *)
        let on_precursor (r : Lookup_result.t) =
          (* if Lookup_status.is_complete_or_fail r.status then precond := true ; *)
          let i =
            if not e.bounded then Global_state.fetch_counter state target else 0
          in
          let phi_new, action_next = e.next i r.from in
          (match phi_new with
          | Some phi_i -> add_phi @@ Riddler.list_append target i phi_i
          | None ->
              if not e.bounded
              then add_phi (Riddler.list_append target i Riddler.false_)) ;
          match action_next with
          | Some key_src ->
              dispatch key_src ;
              (* Fmt.pr "Bind(2) %a <- (%a) <= %a" Lookup_key.pp target
                 Lookup_key.pp e.precursor Lookup_key.pp key_src ; *)
              Some key_src
              (* run ~sub_lookup:(e.pub, r.from) edge *)
              (* stream_of_action ~sub_lookup:(e.pub, r.from) (Direct d) *)
          | _ -> None
        in
        Real_stream
          (U.bind_like unroll e.precursor on_precursor, [ e.precursor ])
    | Bind_list_like e ->
        if not e.bounded then Global_state.create_counter state detail target ;
        let on_precursor (r : Lookup_result.t) =
          let i =
            if not e.bounded then Global_state.fetch_counter state target else 0
          in
          let phi_new, action_next = e.next i r.from in
          (match phi_new with
          | Some phi_i -> add_phi @@ Riddler.list_append target i phi_i
          | None ->
              if not e.bounded
              then add_phi (Riddler.list_append target i Riddler.false_)) ;
          match action_next with
          | Some ds ->
              List.iter ds ~f:dispatch ;
              ds
          | _ -> []
        in
        Real_stream
          (U.bind_like_list unroll e.precursor on_precursor, [ e.precursor ])
    | Join e ->
        if not e.bounded then Global_state.create_counter state detail target ;
        let srcs = List.map e.elements ~f:stream_of_action in
        let streams, deps = join_result srcs in
        Real_stream (Lwt_stream.choose streams, deps)
  in
  (* U.set_pre_push_payload unroll target (fun key p ->
      Fmt.pr "@.@[<v>(%a <== %a)@]@." Lookup_key.pp key Lookup_result.pp p ;
      Some p) ; *)
  (match stream_of_action source with
  | Real_stream (stream, deps) ->
      U.set unroll target stream ;
      List.iter deps ~f:dispatch
  | Just_status status -> U.set_status unroll target status) ;
  ()
(* let need_pre_push =
     (not (List.is_empty detail.sub_preconds))
     || not (List.is_empty detail.sub_lookups)
   in
   if need_pre_push
   then (
     let pre_push (r : Lookup_result.t) =
       (* Fmt.pr "[PrePush] %a <- %a;%a@." Lookup_key.pp target Lookup_key.pp r.from
          Lookup_status.pp_short r.status ; *)
       if List.for_all detail.sub_preconds ~f:Ref.( ! )
       then
         let status' =
           fold_lookups_status state.search.lookup_detail_map detail.sub_lookups
         in
         promote_result status' r.from
       else Some r
     in
     (* U.set_pre_push unroll target pre_push ; *)
     U.set_pre_push_payload unroll target pre_push ;
     Log.debug (fun m ->
         m "[Reg-%B] %a %d@." need_pre_push Lookup_key.pp target
           (List.length detail.sub_preconds))) *)

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val block_map : Cfg.block Jayil.Ast.Ident_map.t
  (* val run_eval : Lookup_key.t -> (Lookup_key.t -> unit -> unit Lwt.t) -> unit *)
end

module Make (S : S) = struct
  open Rule_action

  (* Common actions *)
  let first_but_drop (key : Lookup_key.t) =
    let key_first = Lookup_key.to_first key S.state.info.first in
    Map { pub = key_first; map = Fn.const key }

  let record_start_action (p : Record_start_rule.t) (key : Lookup_key.t) =
    let next i (key_rv : Lookup_key.t) =
      let rv = Cfg.clause_body_of_x key_rv.block key_rv.x in
      match rv with
      | Value_body (Value_record (Record_value rv)) -> (
          match Ident_map.Exceptionless.find p.lbl rv with
          | Some (Var (field, _)) ->
              let key_l = Lookup_key.with_x key_rv field in
              let phi_i =
                Riddler.(eq_list [ K2 (key, key_l); K (p.r, key_rv) ])
              in
              (Some phi_i, Some key_l)
          | None -> (None, None))
      | _ -> (None, None)
    in
    Bind_like { precursor = p.r; next; bounded = false }

  let cond_btm p (key : Lookup_key.t) =
    let ({ x'; rets; _ } : Cond_btm_rule.t) = p in
    let next _ _r =
      (* let eager_result = Checker.eager_check S.state S.config term_c [] in
         Fmt.pr "[CondBtm]%a <- %a\nEager=%B\n" Lookup_key.pp key Lookup_key.pp
           r.from eager_result ; *)
      (* if eager_result *)
      if true
      then
        let elements = List.map rets ~f:(fun (beta, key_ret) -> key_ret) in
        (None, Some elements)
      else (None, None)
    in
    Bind_list_like { precursor = x'; next; bounded = true }

  let fun_enter_local_action (p : Fun_enter_local_rule.t) (key : Lookup_key.t) =
    let fid = key.block.id in
    let elements =
      List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
          let next _ _r = (None, Some key_arg) in
          Bind_like { precursor = key_f; next; bounded = true })
    in
    Join { elements; bounded = true }

  let fun_enter_nonlocal (p : Fun_enter_nonlocal_rule.t) (key : Lookup_key.t) =
    let elements =
      List.map p.callsites_with_stk ~f:(fun (key_f, _key_arg) ->
          let next i (r : Lookup_key.t) =
            let fv_block = Cfg.find_reachable_block r.x S.block_map in
            let key_arg = Lookup_key.of3 key.x r.r_stk fv_block in
            let phi_i =
              let fid = key.block.id in
              Riddler.(
                eq_list
                  [
                    K2 (key, key_arg);
                    Z (key_f, z_of_fid fid);
                    Z (r, z_of_fid fid);
                  ])
            in
            (Some phi_i, Some key_arg)
          in
          Bind_like { precursor = key_f; next; bounded = false })
    in
    Join { elements; bounded = false }

  let fun_exit_action (p : Fun_exit_rule.t) (key : Lookup_key.t) =
    let next _ (rf : Lookup_key.t) =
      let fid = rf.x in
      if List.mem p.fids fid ~equal:Id.equal
      then
        let key_ret = Lookup_key.get_f_return S.block_map fid key in
        (None, Some key_ret)
      else (None, None)
    in
    Bind_like { precursor = p.xf; next; bounded = true }

  let pattern_action p (key : Lookup_key.t) =
    let ({ x'; pat; _ } : Pattern_rule.t) = p in
    let f (r : Lookup_result.t) =
      let key_rv = r.from in
      let rv = Cfg.clause_body_of_x key_rv.block key_rv.x in
      ( Lookup_result.from_as key r.status,
        [ Riddler.pattern key x' key_rv rv pat ] )
    in
    MapSeq { pub = x'; map = f }

  let get_initial_phi_action (key : Lookup_key.t) (rule : Rule.t) =
    let key_first = Lookup_key.to_first key S.state.info.first in
    let open Rule in
    let open Lookup_status in
    let open Riddler in
    match rule with
    (* Bounded (same as complete phi) *)
    | Discovery_main p -> (at_main key (Some p.v), Leaf Complete)
    | Discovery_nonmain p -> (implies_v key key_first p.v, first_but_drop key)
    | Input p ->
        Hash_set.add S.state.search.input_nodes key ;
        if p.is_in_main
        then (at_main key None, Leaf Complete)
        else (implies key key_first, first_but_drop key)
    | Assume p ->
        ( Riddler.imply key [ K2 (key, p.x'); Z (p.x', Riddler.bool_ true) ],
          Leaf Complete )
    | Assert p ->
        ( Riddler.imply key [ K2 (key, p.x'); Z (p.x', Riddler.bool_ true) ],
          Leaf Complete )
    | Mismatch -> (invalid key, Leaf Fail)
    | Abort p ->
        if p.is_target
        then (implies key key_first, first_but_drop key)
        else (invalid key, Leaf Fail)
    | Alias p -> (eq_lookup key p.x', Direct { pub = p.x' })
    | Not p -> (not_lookup key p.x', Map { pub = p.x'; map = Fn.const key })
    | Binop p -> (binop key p.bop p.x1 p.x2, Both { pub1 = p.x1; pub2 = p.x2 })
    | Record_start p -> (true_, record_start_action p key)
    | Cond_top p ->
        ( Riddler.imply key
            [ K2 (key, p.x); Z (p.x2, Riddler.bool_ p.cond_case_info.choice) ],
          (* if Riddler.eager_check S.state S.config key_x2
             [ Riddler.(eqz key_x2 (bool_ choice)) ] *)
          Bind_like
            {
              precursor = p.x2;
              next = (fun _ _ -> (None, Some p.x));
              bounded = true;
            } )
    | Cond_btm p -> (cond_bottom key p.x' p.rets, cond_btm p key)
    | Fun_enter_local p -> (fun_enter_local key p, fun_enter_local_action p key)
    | Fun_enter_nonlocal p -> (true_, fun_enter_nonlocal p key)
    | Fun_exit p ->
        (fun_exit key p.xf p.fids S.state.info.block_map, fun_exit_action p key)
    | Pattern p -> (true_, pattern_action p key)
end

open Riddler

(* Completion *)
let complete_phis_of_rule (state : Global_state.t) key
    (detail : Lookup_detail.t) =
  let open Rule in
  let open Riddler in
  let key_first = Lookup_key.to_first key state.info.first in
  match detail.rule with
  (* Bounded (same as complete phi) *)
  | Discovery_main p -> at_main key (Some p.v)
  | Discovery_nonmain p -> implies_v key key_first p.v
  | Input p -> if p.is_in_main then at_main key None else implies key key_first
  | Assume p -> invalid key
  | Assert p -> invalid key
  | Mismatch -> invalid key
  | Abort p -> if p.is_target then implies key key_first else invalid key
  | Alias p -> eq_lookup key p.x'
  | Not p -> not_lookup key p.x'
  | Binop p -> binop key p.bop p.x1 p.x2
  | Record_start p -> imply_domain_with key detail.domain [ P p.r ]
  | Cond_top p ->
      imply_domain_with key detail.domain
        [ P p.x; Z (p.x2, bool_ p.cond_case_info.choice) ]
  | Cond_btm p -> cond_bottom key p.x' p.rets
  | Fun_enter_local p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
            let detail_f =
              Hashtbl.find_exn state.search.lookup_detail_map key_f
            in
            let detail_arg =
              Hashtbl.find_exn state.search.lookup_detail_map key_arg
            in
            [
              K2 (key, key_arg);
              Z (key_f, z_of_fid fid);
              D (key_f, detail_f.domain);
              D (key_arg, detail_arg.domain);
            ])
      in
      choices key phi_f_and_arg
  | Fun_enter_nonlocal p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map detail.sub_lookups ~f:(fun ((key_f, key_fv), key_arg) ->
            let detail_arg =
              Hashtbl.find_exn state.search.lookup_detail_map key_arg
            in
            [
              K (key_f, key_fv);
              Z (key_f, z_of_fid fid);
              D (key_arg, detail_arg.domain);
            ])
      in
      choices key phi_f_and_arg
  | Fun_exit p ->
      let phi_f_and_ret =
        List.map detail.sub_lookups ~f:(fun ((key_f, key_fv), key_ret) ->
            let detail_ret =
              Hashtbl.find_exn state.search.lookup_detail_map key_ret
            in
            [
              K (key_f, key_fv);
              Z (key_fv, z_of_fid key_fv.x);
              D (key_ret, detail_ret.domain);
            ])
      in
      choices key phi_f_and_ret
  | Pattern p ->
      let detail_x' = Hashtbl.find_exn state.search.lookup_detail_map p.x' in
      let phi_choices =
        (* detail.domain *)
        List.map detail_x'.domain ~f:(fun key_r ->
            let rv = Cfg.clause_body_of_x key_r.block key_r.x in
            [
              P p.x';
              P key_r;
              Z
                ( key,
                  bool_ (Option.value_exn (Jayil.Ast.pattern_match p.pat rv)) );
            ])
      in
      choices key phi_choices
