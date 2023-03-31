open Core
open Dj_common
open Jayil
open Jayil.Ast
open Rule
open Types
module U = Unrolls.U_dbmc
module Log = Log.Export.CMLOG

let create_counter (state : Global_state.t) (detail : Lookup_detail.t) key =
  Hashtbl.update state.smt_lists key ~f:(function
    | Some i -> i
    | None ->
        Global_state.add_phi state detail (Riddler.list_head key) ;
        0)

let fetch_counter (state : Global_state.t) key =
  let new_i =
    Hashtbl.update_and_return state.smt_lists key ~f:(function
      | Some i -> i + 1
      | None -> failwith (Fmt.str "why not inited : %a" Lookup_key.pp key))
  in
  new_i - 1

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
        create_counter state detail target ;
        let f r =
          let i = fetch_counter state target in
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
        if not e.bounded then create_counter state detail target ;

        let precond = ref false in
        add_sub_preconds precond ;
        let part1_cb key (r : Lookup_result.t) =
          if Lookup_status.is_complete_or_fail r.status then precond := true ;
          Lookup_status.iter_ok r.status (fun () ->
              let i = if not e.bounded then fetch_counter state target else 0 in
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
        if not e.bounded then create_counter state detail target ;
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

  let listen_but_use source value = Map { pub = source; map = Fn.const value }

  let chain_then_direct pre source =
    let next _ _r =
      (* cond_top *)
      (* true *)
      (* if Riddler.eager_check S.state S.config key_x2
           [ Riddler.eqv key_x2 (Value_bool choice) ] *)
      (None, Some (Direct { pub = source }))
    in
    Chain { pub = pre; next; bounded = true }

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
    let ({ x'; cond_both } : Cond_btm_rule.t) = p in
    let next _ _r =
      (* let eager_result = Checker.eager_check S.state S.config term_c [] in
         Fmt.pr "[CondBtm]%a <- %a\nEager=%B\n" Lookup_key.pp key Lookup_key.pp
           r.from eager_result ; *)
      (* if eager_result *)
      if true
      then
        let elements =
          List.filter_map [ true; false ] ~f:(fun beta ->
              let cond_case_block_opt =
                if beta then cond_both.then_ else cond_both.else_
              in
              (* Riddler.step_eager_check S.state S.config term_c
                    [ Riddler.eqv term_c (Value_bool beta) ]
                    S.config.stride *)
              match cond_case_block_opt with
              | Some cond_case_block ->
                  let key_ret =
                    Lookup_key.return_key_of_cond key beta cond_case_block
                  in
                  Some (Direct { pub = key_ret })
              | None -> None)
        in
        (None, Some (Or_list { elements; bounded = true }))
      else (None, None)
    in
    Chain { pub = x'; next; bounded = true }

  let fun_enter_local (p : Fun_enter_local_rule.t) (key : Lookup_key.t) =
    let fid = key.block.id in
    let elements =
      List.map p.callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop key.r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              let next _ _r =
                let key_arg =
                  Lookup_key.of3 x''' callsite_stack callsite_block
                in
                (None, Some (Direct { pub = key_arg }))
              in
              Chain { pub = key_f; next; bounded = true }
          | None -> failwith "why Rstack.pop fails here")
    in
    Or_list { elements; bounded = true }

  let fun_enter_nonlocal (p : Fun_enter_nonlocal_rule.t) (key : Lookup_key.t) =
    let elements =
      List.map p.callsites ~f:(fun callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop key.r_stk (x', key.block.id) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              let next i (r : Lookup_key.t) =
                let fv_block = Cfg.find_block_by_id r.x S.block_map in
                let key_arg = Lookup_key.of3 key.x r.r_stk fv_block in
                let phi_i =
                  Riddler.fun_enter_nonlocal key key_f r key.block.id key_arg
                in
                let action = Direct { pub = key_arg } in
                (Some phi_i, Some action)
              in
              Chain { pub = key_f; next; bounded = false }
          | None -> failwith "why Rstack.pop fails here")
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
              Riddler.eqv_with_picked key x' (Value_bool (* true *) matched);
              Riddler.picked_pattern key x' pat;
            ]
        | Rec_pattern ids, Value_body (Value_record (Record_value rv_)) ->
            [ Riddler.picked_record_pattern key x' (Value_bool matched) pat ]
        | Strict_rec_pattern ids, Value_body (Value_record (Record_value rv_))
          ->
            [ Riddler.picked_record_pattern key x' (Value_bool matched) pat ]
        | Rec_pattern _, _ | _, Value_body _ ->
            [
              Riddler.eqv_with_picked key x' (Value_bool (* false *) matched);
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
    | Discovery_main p ->
        (Riddler.discover_main_with_picked key (Some p.v), Leaf Complete)
    | Discovery_nonmain p ->
        (Riddler.discover_non_main key key_first (Some p.v), first_but_drop key)
    | Assume p -> (Riddler.mismatch_with_picked key, Leaf Fail)
    | Assert p -> (Riddler.mismatch_with_picked key, Leaf Fail)
    | Mismatch -> (Riddler.mismatch_with_picked key, Leaf Fail)
    | Abort p ->
        if p.is_target
        then (Riddler.discover_non_main key key_first None, first_but_drop key)
        else (Riddler.mismatch_with_picked key, Leaf Fail)
    | Alias p -> (Riddler.eq_with_picked key p.x', Direct { pub = p.x' })
    | Input p ->
        Hash_set.add S.state.input_nodes key ;
        if p.is_in_main
        then (Riddler.discover_main_with_picked key None, Leaf Complete)
        else (Riddler.discover_non_main key key_first None, first_but_drop key)
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
    | Cond_btm p -> (Riddler.cond_bottom key p.x' p.cond_both, cond_btm p key)
    | Fun_enter_local p ->
        ( Riddler.fun_enter_local key key.block.id p.callsites S.state.block_map,
          fun_enter_local p key )
    | Fun_enter_nonlocal p -> (Riddler.true_, fun_enter_nonlocal p key)
    | Fun_exit p ->
        (Riddler.fun_exit key p.xf p.fids S.state.block_map, fun_exit p key)
    | Pattern p -> (Riddler.true_, pattern p key)
end

(* Fmt.pr "[Chain][P1]%a <- %a(%a) @." Lookup_key.pp target Lookup_key.pp
     r.from Lookup_status.pp_short r.status ;
   Fmt.pr "[Chain][P1](%B)%a @." !precond
     (Observe.pp_key_with_detail state.lookup_detail_map)
     (target, lookup_detail) ; *)
