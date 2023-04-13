open Core
open Dj_common
open Jayil
open Jayil.Ast
open Rule
open Types
module U = Unrolls.U_dbmc
module Log = Log.Export.CMLOG

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
      set_status detail Complete ;
      Some Lookup_result.(from_as v Complete)
  | Some Fail ->
      set_status detail Fail ;
      Some Lookup_result.(from_as v Fail)
  | Some Good -> Some Lookup_result.(from_as v Good)
  | None -> None

let run_action run_task unroll (state : Global_state.t)
    (detail : Lookup_detail.t) target source =
  let open Rule_action in
  let add_phi = Global_state.add_phi state detail in
  let set_status = set_status detail in
  let set_status_gen_phi = set_status_gen_phi detail in
  let add_to_domain v = detail.domain <- detail.domain @ [ v ] in
  let add_sub_preconds cond =
    detail.sub_preconds <- detail.sub_preconds @ [ cond ]
  in
  let add_sublookup pre_pair key =
    detail.sub_lookups <- detail.sub_lookups @ [ (pre_pair, key) ]
  in
  let promote_result = promote_result target state.lookup_detail_map detail in
  let rec run ?sub_lookup source =
    match source with
    | Leaf Fail ->
        set_status Lookup_status.Fail ;
        set_status_gen_phi Lookup_status.Fail
        (* ; U.by_return unroll target (Lookup_result.fail target) *)
    | Leaf Complete ->
        set_status Lookup_status.Complete ;
        set_status_gen_phi Lookup_status.Complete ;
        add_to_domain target ;
        U.by_return unroll target (Lookup_result.complete target)
    | Leaf _ -> failwith "incorrect leaf status"
    | Direct e ->
        (match sub_lookup with
        | Some pre ->
            add_sublookup pre e.pub ;
            U.by_map_u unroll target e.pub (fun r ->
                Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from) ;
                r)
        | None ->
            U.by_filter_map_u unroll target e.pub (fun r ->
                Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from) ;
                promote_result r.status r.from)) ;

        Log.debug (fun m ->
            m "[Direct]%a <- %a(%B) @." Lookup_key.pp target Lookup_key.pp e.pub
              (Option.is_some sub_lookup)) ;
        run_task e.pub
    | Map e ->
        U.by_filter_map_u unroll target e.pub (fun r ->
            let r' = promote_result r.status (e.map r.from) in
            Option.iter r' ~f:(fun r ->
                Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from)) ;
            r') ;
        run_task e.pub
    | MapSeq e ->
        Global_state.create_counter state detail target ;
        let f r =
          let i = Global_state.fetch_counter state target in
          let r', phis = e.map i r in
          add_phi (Riddler.list_append target i (Riddler.and_ phis)) ;
          Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from) ;
          let r' = promote_result r.status r'.from in
          Option.iter r' ~f:(fun r ->
              Lookup_status.iter_ok r.status (fun () -> add_to_domain r.from)) ;
          r'
        in
        U.by_filter_map_u unroll target e.pub f ;
        run_task e.pub
    | Both e ->
        U.by_filter_map2_u unroll target e.pub1 e.pub2 (fun (v1, v2) ->
            let joined_status = Lookup_status.join v1.status v2.status in
            Lookup_status.iter_ok joined_status (fun () -> add_to_domain target) ;
            promote_result joined_status target) ;
        run_task e.pub1 ;
        run_task e.pub2
    | Chain e ->
        if not e.bounded then Global_state.create_counter state detail target ;

        let precond = ref false in
        add_sub_preconds precond ;
        let part1_cb key (r : Lookup_result.t) =
          if Lookup_status.is_complete_or_fail r.status then precond := true ;
          Lookup_status.iter_ok r.status (fun () ->
              let i =
                if not e.bounded
                then Global_state.fetch_counter state target
                else 0
              in
              let phi_new, action_next = e.next i r.from in
              (match phi_new with
              | Some phi_i -> add_phi @@ Riddler.list_append target i phi_i
              | None ->
                  if not e.bounded
                  then add_phi (Riddler.list_append target i Riddler.false_)) ;
              match action_next with
              | Some edge -> run ~sub_lookup:(e.pub, r.from) edge
              | None -> ()) ;
          Lwt.return_unit
        in
        U.by_bind_u unroll target e.pub part1_cb ;
        run_task e.pub
    | Or_list e ->
        if not e.bounded then Global_state.create_counter state detail target ;
        List.iter e.elements ~f:(run ?sub_lookup)
  in

  run source ;
  let need_pre_push =
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
          fold_lookups_status state.lookup_detail_map detail.sub_lookups
        in
        promote_result status' r.from
      else Some r
    in
    U.set_pre_push unroll target pre_push ;
    Log.debug (fun m ->
        m "[Reg-%B] %a %d@." need_pre_push Lookup_key.pp target
          (List.length detail.sub_preconds)))

let pattern_truth pat rv =
  match (pat, rv) with
  | Any_pattern, _
  | Fun_pattern, Value_body (Value_function _)
  | Int_pattern, Value_body (Value_int _)
  | Int_pattern, Input_body
  | Bool_pattern, Value_body (Value_bool _) ->
      true
  | Rec_pattern ids, Value_body (Value_record (Record_value rv)) ->
      Ident_set.for_all (fun id -> Ident_map.mem id rv) ids
  | Strict_rec_pattern ids, Value_body (Value_record (Record_value rv)) ->
      Ident_set.equal ids (Ident_set.of_enum @@ Ident_map.keys rv)
  | _ -> false

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val block_map : Cfg.block Jayil.Ast.Ident_map.t
end

module Make (S : S) = struct
  open Rule_action

  (* Common actions *)
  let first_but_drop (key : Lookup_key.t) =
    let key_first = Lookup_key.to_first key S.state.first in
    Map { pub = key_first; map = Fn.const key }

  let record_start (p : Record_start_rule.t) (key : Lookup_key.t) =
    let next i (key_rv : Lookup_key.t) =
      let rv = Cfg.clause_body_of_x key_rv.block key_rv.x in
      match rv with
      | Value_body (Value_record (Record_value rv)) -> (
          match Ident_map.Exceptionless.find p.lbl rv with
          | Some (Var (field, _)) ->
              let key_l = Lookup_key.with_x key_rv field in
              let phi_i = Riddler.record_start key p.r key_rv key_l in
              let action = Direct { pub = key_l } in
              (Some phi_i, Some action)
          | None -> (None, None))
      | _ -> (None, None)
    in
    Chain { pub = p.r; next; bounded = false }

  let cond_btm p (key : Lookup_key.t) =
    let ({ x'; rets; _ } : Cond_btm_rule.t) = p in
    let next _ _r =
      (* let eager_result = Checker.eager_check S.state S.config term_c [] in
         Fmt.pr "[CondBtm]%a <- %a\nEager=%B\n" Lookup_key.pp key Lookup_key.pp
           r.from eager_result ; *)
      (* if eager_result *)
      if true
      then
        let elements =
          List.map rets ~f:(fun (beta, key_ret) -> Direct { pub = key_ret })
        in
        (None, Some (Or_list { elements; bounded = true }))
      else (None, None)
    in
    Chain { pub = x'; next; bounded = true }

  let fun_enter_local (p : Fun_enter_local_rule.t) (key : Lookup_key.t) =
    let fid = key.block.id in
    let elements =
      List.map p.callsites_with_stk ~f:(fun (key_f, key_arg) ->
          let next _ _r = (None, Some (Direct { pub = key_arg })) in
          Chain { pub = key_f; next; bounded = true })
    in
    Or_list { elements; bounded = true }

  let fun_enter_nonlocal (p : Fun_enter_nonlocal_rule.t) (key : Lookup_key.t) =
    let elements =
      List.map p.callsites_with_stk ~f:(fun (key_f, _key_arg) ->
          let next i (r : Lookup_key.t) =
            let fv_block = Cfg.find_block_by_id r.x S.block_map in
            let key_arg = Lookup_key.of3 key.x r.r_stk fv_block in
            let phi_i =
              Riddler.fun_enter_nonlocal key key_f r key.block.id key_arg
            in
            let action = Direct { pub = key_arg } in
            (Some phi_i, Some action)
          in
          Chain { pub = key_f; next; bounded = false })
    in
    Or_list { elements; bounded = false }

  let fun_exit (p : Fun_exit_rule.t) (key : Lookup_key.t) =
    let next _ (rf : Lookup_key.t) =
      let fid = rf.x in
      if List.mem p.fids fid ~equal:Id.equal
      then
        let key_ret = Lookup_key.get_f_return S.block_map fid key in
        (None, Some (Direct { pub = key_ret }))
      else (None, None)
    in
    Chain { pub = p.xf; next; bounded = true }

  let pattern p (key : Lookup_key.t) =
    let ({ x'; pat; _ } : Pattern_rule.t) = p in
    let f i (r : Lookup_result.t) =
      (* OB1: For some patterns, we can immediately know the result of the matching:
           when the returning value is a literal value. We can use it in the interpreter.
           We lose this information when the lookup go through a conditional block or
           some binop. *)
      (* OB2: The pattern matching can tolerate infeasible cases caused by the analysis,
         because the literal value is incorrect. A conditional block can use this result
         to go into a then-block or a else-block.
      *)
      let key_rv = r.from in
      let rv = Cfg.clause_body_of_x key_rv.block key_rv.x in
      let matched = pattern_truth pat rv in
      let phis =
        match (pat, rv) with
        | Any_pattern, _
        | Fun_pattern, Value_body (Value_function _)
        | Int_pattern, Value_body (Value_int _)
        | Int_pattern, Input_body
        | Bool_pattern, Value_body (Value_bool _) ->
            [
              Riddler.picked_imply_with_v1 key x'
                (Value_bool (* true *) matched);
              Riddler.picked_pattern key x' pat;
            ]
        | Rec_pattern ids, Value_body (Value_record (Record_value rv_)) ->
            [ Riddler.picked_record_pattern key x' (Value_bool matched) ]
        | Strict_rec_pattern ids, Value_body (Value_record (Record_value rv_))
          ->
            [ Riddler.picked_record_pattern key x' (Value_bool matched) ]
        | Rec_pattern _, _ | _, Value_body _ ->
            [
              Riddler.picked_imply_with_v1 key x'
                (Value_bool (* false *) matched);
              Riddler.picked_pattern key x' pat;
            ]
        | _, _ ->
            (* TODO: some binops contain type information for patterns *)
            (* TODO: and for previous pattern match *)
            [ Riddler.picked_pattern key x' pat ]
      in
      (* Fmt.pr "[Pattern][%B] %a | %a |%a\n" matched Lookup_key.pp key
         Jayil.Ast_pp.pp_pattern pat Lookup_key.pp key_rv ; *)
      let eq_key'_rv = Riddler.eq x' key_rv in
      let picked_rv = Riddler.picked key_rv in
      (Lookup_result.from_as key r.status, picked_rv :: eq_key'_rv :: phis)
    in
    MapSeq { pub = x'; map = f }

  let get_initial_phi_action (key : Lookup_key.t) (rule : Rule.t) =
    let key_first = Lookup_key.to_first key S.state.first in
    let open Rule in
    let open Lookup_status in
    match rule with
    (* Bounded (same as complete phi) *)
    | Discovery_main p -> (Riddler.picked_main key (Some p.v), Leaf Complete)
    | Discovery_nonmain p ->
        ( Riddler.picked_imply_with key key_first (Riddler.eqv key p.v),
          first_but_drop key )
    | Input p ->
        Hash_set.add S.state.input_nodes key ;
        if p.is_in_main
        then (Riddler.picked_main key None, Leaf Complete)
        else (Riddler.picked_imply key key_first, first_but_drop key)
    | Assume p -> (Riddler.picked_false key, Leaf Fail)
    | Assert p -> (Riddler.picked_false key, Leaf Fail)
    | Mismatch -> (Riddler.picked_false key, Leaf Fail)
    | Abort p ->
        if p.is_target
        then (Riddler.picked_imply key key_first, first_but_drop key)
        else (Riddler.picked_false key, Leaf Fail)
    | Alias p -> (Riddler.eq_with_picked key p.x', Direct { pub = p.x' })
    | Not p -> (Riddler.not_with_picked key p.x', listen_but_use p.x' key)
    | Binop p ->
        ( Riddler.binop_with_picked key p.bop p.x1 p.x2,
          Both { pub1 = p.x1; pub2 = p.x2 } )
    (*
      Unbounded
    *)
    | Record_start p -> (Riddler.true_, record_start p key)
    | Cond_top p ->
        ( Riddler.cond_top key p.x p.x2 p.cond_case_info.choice,
          chain_then_direct p.x2 p.x )
    | Cond_btm p -> (Riddler.cond_bottom key p.x' p.rets, cond_btm p key)
    | Fun_enter_local p -> (Riddler.fun_enter_local key p, fun_enter_local p key)
    | Fun_enter_nonlocal p -> (Riddler.true_, fun_enter_nonlocal p key)
    | Fun_exit p ->
        (Riddler.fun_exit key p.xf p.fids S.state.block_map, fun_exit p key)
    | Pattern p -> (Riddler.true_, pattern p key)
end

open Riddler
open SuduZ3

(* Completion *)
let complete_phis_of_rule (state : Global_state.t) key
    (detail : Lookup_detail.t) =
  let open Rule in
  let open Riddler in
  let key_first = Lookup_key.to_first key state.first in
  match detail.rule with
  (* Bounded (same as complete phi) *)
  | Discovery_main p -> picked_main key (Some p.v)
  | Discovery_nonmain p -> picked_imply_with key key_first (Riddler.eqv key p.v)
  | Assume p -> picked_false key
  | Assert p -> picked_false key
  | Mismatch -> picked_false key
  | Abort p ->
      if p.is_target then picked_imply key key_first else picked_false key
  | Alias p -> eq_with_picked key p.x'
  | Input p ->
      if p.is_in_main then picked_main key None else picked_imply key key_first
  | Not p -> not_with_picked key p.x'
  | Binop p -> binop_with_picked key p.bop p.x1 p.x2
  (*
      Unbounded
  *)
  | Record_start p -> picked_eq_choices key detail.domain (picked p.r)
  | Cond_top p ->
      picked_eq_choices key detail.domain
        (and_
           [
             picked p.x;
             picked p.x2;
             eq key p.x;
             eq_bool p.x2 p.cond_case_info.choice;
           ])
  | Cond_btm p -> cond_bottom key p.x' p.rets
  | Fun_enter_local p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map p.callsites ~f:(fun callsite ->
            let callsite_block, x', x'', x''' =
              Cfg.fun_info_of_callsite callsite state.block_map
            in
            let callsite_stack =
              Option.value_exn (Rstack.pop key.r_stk (x', fid))
            in
            let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
            let detail_f = Hashtbl.find_exn state.lookup_detail_map key_f in
            let domain_f = detail_f.domain in
            let key_arg = Lookup_key.of3 x''' callsite_stack callsite_block in
            let detail_arg = Hashtbl.find_exn state.lookup_detail_map key_arg in
            let domain_arg = detail_arg.domain in
            and_
              [
                eq key key_arg;
                eq_fid key_f fid;
                picked key_f;
                picked_eq_choices key_f domain_f true_;
                picked key_arg;
                picked_eq_choices key_arg domain_arg true_;
              ])
      in
      (* and_
         [
           Riddler.fun_enter_local key key.block.id p.callsites state.block_map;
           ;
         ] *)
      picked key @=> or_ phi_f_and_arg
  | Fun_enter_nonlocal p ->
      let fid = key.block.id in
      let phi_f_and_arg =
        List.map detail.sub_lookups ~f:(fun ((key_f, key_fv), key_arg) ->
            let detail_arg = Hashtbl.find_exn state.lookup_detail_map key_arg in
            and_
              [
                picked key_f;
                picked key_fv;
                eq_fid key_f fid;
                eq key_f key_fv;
                picked_eq_choices key_arg detail_arg.domain true_;
              ])
      in
      picked key @=> or_ phi_f_and_arg
  | Fun_exit p ->
      let phi_f_and_ret =
        List.map detail.sub_lookups ~f:(fun ((key_f, key_fv), key_ret) ->
            let detail_ret = Hashtbl.find_exn state.lookup_detail_map key_ret in
            and_
              [
                picked key_f;
                eq_fid key_fv key_fv.x;
                picked key_fv;
                eq key_f key_fv;
                picked_eq_choices key_ret detail_ret.domain true_;
              ])
      in
      picked key @=> or_ phi_f_and_ret
  | Pattern p ->
      let detail_x' = Hashtbl.find_exn state.lookup_detail_map p.x' in
      let choices =
        (* detail.domain *)
        List.map detail_x'.domain ~f:(fun key_r ->
            let rv = Cfg.clause_body_of_x key_r.block key_r.x in
            and_ [ eq_bool key (pattern_truth p.pat rv); picked key_r ])
      in
      picked key @=> and_ [ picked p.x'; or_ choices ]
