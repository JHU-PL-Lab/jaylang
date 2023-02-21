open Core
open Dj_common
open Jayil
open Jayil.Ast
open Log.Export
open Rule
open Types

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val block_map : Cfg.block Jayil.Ast.Ident_map.t
end

module Make (S : S) = struct
  open Rule_action

  let rule_main v _p (key : Lookup_key.t) =
    let target_stk = Rstack.concretize_top key.r_stk in
    let phis = [ Riddler.discover_main_with_picked key v ] in
    Leaf { sub = key; phis }

  let rule_nonmain v _p (key : Lookup_key.t) =
    let key_first = Lookup_key.to_first key S.state.first in
    let phis = [ Riddler.discover_non_main key key_first v ] in
    Map
      {
        sub = key;
        pub = key_first;
        map = (fun _ -> Lookup_result.ok key);
        phis;
      }

  let discovery_main p (key : Lookup_key.t) =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) p key

  let discovery_nonmain p (key : Lookup_key.t) =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) p key

  let input p (key : Lookup_key.t) =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes key ;
    if is_in_main then rule_main None p key else rule_nonmain None p key

  let alias p (key : Lookup_key.t) =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let phis = [ Riddler.eq_with_picked key key' ] in
    Direct { sub = key; pub = key'; phis }

  let not_ p (key : Lookup_key.t) =
    let ({ x'; _ } : Not_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let phis = [ Riddler.not_with_picked key key' ] in
    Map { sub = key; pub = key'; map = (fun _ -> Lookup_result.ok key); phis }

  let binop b (key : Lookup_key.t) =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let key_x2 = Lookup_key.with_x key x2 in
    let phis = [ Riddler.binop_with_picked key bop key_x1 key_x2 ] in
    Both
      {
        sub = key;
        pub1 = Lookup_key.with_x key x1;
        pub2 = Lookup_key.with_x key x2;
        phis;
      }

  let record_start p (key : Lookup_key.t) =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in
    let next i r =
      if Lookup_result.is_ok r
      then
        let key_rv = r.from in
        let rv = Cfg.clause_body_of_x key_rv.block key_rv.x in
        match rv with
        | Value_body (Value_record (Record_value rv)) -> (
            match Ident_map.Exceptionless.find lbl rv with
            | Some (Var (field, _)) ->
                let key_l = Lookup_key.with_x key_rv field in
                let phi_i = Riddler.record_start key key_r key_rv key_l in
                let action = Direct { sub = key; pub = key_l; phis = [] } in
                Some (phi_i, action)
            | None -> None)
        | _ -> None
      else None
    in
    Sequence { sub = key; pub = key_r; next; phis = [] }

  let cond_top p (this_key : Lookup_key.t) =
    let ({ cond_case_info = cb; condsite_block } : Cond_top_rule.t) = p in
    let beta = cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop this_key.r_stk (cb.condsite, Id.cond_fid beta)
    in
    let x2 = cb.cond in
    let key_x2 = Lookup_key.of3 x2 condsite_stack condsite_block in
    let key_x = Lookup_key.of3 this_key.x condsite_stack condsite_block in
    let next _ r =
      (* true *)
      (* if Riddler.eager_check S.state S.config key_x2
           [ Riddler.eqv key_x2 (Value_bool choice) ] *)
      if Lookup_result.is_ok r
      then Some (Direct { sub = this_key; pub = key_x; phis = [] })
      else None
    in
    let phis = [ Riddler.cond_top this_key key_x key_x2 beta ] in
    (* Fmt.pr "\n[Chain inspect]key= %a, x= %a, x2= %a," Lookup_key.pp this_key
       Lookup_key.pp key_x Lookup_key.pp key_x2 ; *)
    Chain { sub = this_key; pub = key_x2; next; phis }

  let cond_btm p (key : Lookup_key.t) =
    let this_key = key in
    let ({ x; x'; cond_both } : Cond_btm_rule.t) = p in
    let term_c = Lookup_key.with_x this_key x' in
    let next _ r =
      if Lookup_result.is_ok r
      then
        let sub = this_key in
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
                    Some (Direct { sub = this_key; pub = key_ret; phis = [] })
                | None -> None)
          in
          Some (Or_list { sub; elements; unbound = false; phis = [] })
        else None
      else None
    in

    let phis = [ Riddler.cond_bottom this_key term_c cond_both ] in
    Chain { sub = this_key; pub = term_c; next; phis }

  let fun_enter_local p (key : Lookup_key.t) =
    let this_key = key in
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let fid = key.block.id in
    let callsites = Lookup_key.get_callsites this_key.r_stk key.block in
    let elements =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop this_key.r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              let next this_key r =
                if Lookup_result.is_ok r
                then
                  let key_arg =
                    Lookup_key.of3 x''' callsite_stack callsite_block
                  in
                  Some (Direct { sub = this_key; pub = key_arg; phis = [] })
                else None
              in
              Chain { sub = this_key; pub = key_f; next; phis = [] }
          | None -> failwith "why Rstack.pop fails here")
    in
    let phis = [ Riddler.fun_enter_local this_key fid callsites S.block_map ] in
    Or_list { sub = this_key; elements; unbound = false; phis }

  let fun_enter_nonlocal p (key : Lookup_key.t) =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let callsites = Lookup_key.get_callsites key.r_stk key.block in
    let elements =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop key.r_stk (x', key.block.id) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack callsite_block in
              let next i r =
                if Lookup_result.is_ok r
                then
                  let fv_block = Cfg.find_block_by_id r.from.x S.block_map in
                  let key_arg = Lookup_key.of3 key.x r.from.r_stk fv_block in
                  let phi_i =
                    Riddler.fun_enter_nonlocal key key_f r.from key.block.id
                      key_arg
                  in
                  let action = Direct { sub = key; pub = key_arg; phis = [] } in
                  Some (phi_i, action)
                else None
              in
              Sequence { sub = key; pub = key_f; next; phis = [] }
          | None -> failwith "why Rstack.pop fails here")
    in
    Or_list { sub = key; elements; unbound = true; phis = [] }

  let fun_exit p (this_key : Lookup_key.t) =
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of3 xf this_key.r_stk this_key.block in
    let next (this_key : Lookup_key.t) (rf : Lookup_result.t) =
      if Lookup_result.is_ok rf
      then
        let fid = rf.from.x in
        if List.mem fids fid ~equal:Id.equal
        then
          let key_ret = Lookup_key.get_f_return S.block_map fid this_key in
          Some (Direct { sub = this_key; pub = key_ret; phis = [] })
        else None
      else None
    in
    let phis = [ Riddler.fun_exit this_key key_f fids S.block_map ] in
    Chain { sub = this_key; pub = key_f; next; phis }

  let pattern p (key : Lookup_key.t) =
    let ({ x'; pat; _ } : Pattern_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let next i (r : Lookup_result.t) =
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
      let ans, phis, _matched =
        match (pat, rv) with
        | Any_pattern, _
        | Fun_pattern, Value_body (Value_function _)
        | Int_pattern, Value_body (Value_int _)
        | Int_pattern, Input_body
        | Bool_pattern, Value_body (Value_bool _) ->
            let phi1 = Riddler.eqv_with_picked key key' (Value_bool true) in
            let phi2 = Riddler.picked_pattern key key' pat in

            (Lookup_result.ok key, [ phi1; phi2 ], true)
        | Rec_pattern ids, Value_body (Value_record (Record_value rv)) ->
            let have_all =
              Ident_set.for_all (fun id -> Ident_map.mem id rv) ids
            in
            let phi =
              Riddler.picked_record_pattern key key' (Value_bool have_all) pat
            in
            (Lookup_result.ok key, [ phi ], true)
        | Strict_rec_pattern ids, Value_body (Value_record (Record_value rv)) ->
            let have_all =
              Ident_set.equal ids (Ident_set.of_enum @@ Ident_map.keys rv)
            in
            let phi =
              Riddler.picked_record_pattern key key' (Value_bool have_all) pat
            in
            (Lookup_result.ok key, [ phi ], true)
        | Rec_pattern _, _ | _, Value_body _ ->
            let phi1 = Riddler.eqv_with_picked key key' (Value_bool false) in
            let phi2 = Riddler.picked_pattern key key' pat in

            (Lookup_result.ok key, [ phi1; phi2 ], false)
        | _, _ ->
            (* TODO: some binops contain type information for patterns *)
            (* TODO: and for previous pattern match *)
            let phi = Riddler.picked_pattern key key' pat in
            (Lookup_result.ok key, [ phi ], false)
      in
      (* Fmt.pr "[Pattern][%B] %a | %a |%a\n" matched Lookup_key.pp key
         Jayil.Ast_pp.pp_pattern pat Lookup_key.pp key_rv ; *)
      let eq_key'_rv = Riddler.eq key' key_rv in
      let picked_rv = Riddler.picked key_rv in
      (ans, picked_rv :: eq_key'_rv :: phis)
    in
    MapSeq { sub = key; pub = key'; map = next; phis = [] }

  let assume _p (key : Lookup_key.t) =
    Withered { sub = key; phis = [ Riddler.mismatch_with_picked key ] }

  let assert_ _p (key : Lookup_key.t) =
    Withered { sub = key; phis = [ Riddler.mismatch_with_picked key ] }

  let abort p (key : Lookup_key.t) =
    if Lookup_key.equal key (Lookup_key.start S.config.target key.block)
       (* TODO: take care of direct `abort` in the main block *)
    then rule_nonmain None p key
    else Withered { sub = key; phis = [ Riddler.mismatch_with_picked key ] }

  let mismatch (key : Lookup_key.t) =
    Withered { sub = key; phis = [ Riddler.mismatch_with_picked key ] }
end
