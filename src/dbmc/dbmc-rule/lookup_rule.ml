open Core
open Dj_common
open Jayil
open Jayil.Ast
open Log.Export
open Rule
module U = Unrolls.U_dbmc
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

  let rule_nonmain v _p (key : Lookup_key.t) block =
    let key_first = Lookup_key.to_first key S.state.first in
    let phis = [ Riddler.discover_non_main key key_first v ] in
    Map
      {
        sub = key;
        pub = (key_first, block);
        map = (fun _ -> Lookup_result.ok key);
        phis;
      }

  let discovery_main p (key : Lookup_key.t) =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) p key

  let discovery_nonmain p (key : Lookup_key.t) block =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) p key block

  let input p (key : Lookup_key.t) block =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes key ;
    if is_in_main then rule_main None p key else rule_nonmain None p key block

  let alias p (key : Lookup_key.t) block =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let phis = [ Riddler.eq_with_picked key key' ] in
    Direct { sub = key; pub = (key', block); phis }

  let not_ p (key : Lookup_key.t) block =
    let ({ x'; _ } : Not_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let phis = [ Riddler.not_with_picked key key' ] in
    Direct { sub = key; pub = (key', block); phis }

  let binop b (key : Lookup_key.t) block =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let key_x2 = Lookup_key.with_x key x2 in
    let phis = [ Riddler.binop_with_picked key bop key_x1 key_x2 ] in
    Both
      {
        sub = key;
        pub1 = (Lookup_key.with_x key x1, block);
        pub2 = (Lookup_key.with_x key x2, block);
        phis;
      }

  let record_start p (key : Lookup_key.t) block =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in
    let next i (r : Lookup_result.t) =
      let key_rv = r.from in
      let rv_block = Cfg.block_of_id key_rv.x S.block_map in
      let rv = Cfg.clause_body_of_x rv_block key_rv.x in
      match rv with
      | Value_body (Value_record (Record_value rv)) -> (
          match Ident_map.Exceptionless.find lbl rv with
          | Some (Var (field, _)) ->
              let key_l = Lookup_key.with_x key_rv field in
              let phi_i = Riddler.record_start key key_r key_rv key_l in
              let action =
                Direct { sub = key; pub = (key_l, rv_block); phis = [] }
              in
              Some (phi_i, action)
          | None -> None)
      | _ -> None
    in
    Sequence { sub = key; pub = (key_r, block); next; phis = [] }

  let cond_top (cb : Cond_top_rule.t) (key : Lookup_key.t) block =
    let condsite_block = Cfg.outer_block block S.block_map in
    let x, r_stk = Lookup_key.to2 key in
    let choice = Option.value_exn cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop r_stk (cb.point, Id.cond_fid choice)
    in
    let x2 = cb.cond in
    let key_x2 =
      Lookup_key.of3 x2 condsite_stack (Cfg.id_of_block condsite_block)
    in
    let key_x =
      Lookup_key.of3 x condsite_stack (Cfg.id_of_block condsite_block)
    in
    let next _ r =
      (* true *)
      (* if Riddler.eager_check S.state S.config key_x2
           [ Riddler.eqv key_x2 (Value_bool choice) ] *)
      Some (Direct { sub = key; pub = (key_x, condsite_block); phis = [] })
    in
    let phis = [ Riddler.cond_top key key_x key_x2 choice ] in
    Chain { sub = key; pub = (key_x2, condsite_block); next; phis }

  let cond_btm p (key : Lookup_key.t) block =
    let this_key = key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block = Ident_map.find tid S.block_map |> Cfg.cast_to_cond_block in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.with_x this_key x' in
    let next _ (r : Lookup_result.t) =
      if r.status
      then
        let sub = this_key in
        let nexts =
          List.filter_map [ true; false ] ~f:(fun beta ->
              if true
                 (* Riddler.step_eager_check S.state S.config term_c
                     [ Riddler.eqv term_c (Value_bool beta) ]
                     S.config.stride *)
              then
                let case_block, key_ret =
                  Lookup_key.get_cond_block_and_return cond_block beta
                    this_key.r_stk x
                in
                Some
                  (Direct
                     { sub = this_key; pub = (key_ret, case_block); phis = [] })
              else None)
        in
        Some (Or_list { sub; nexts; unbound = false; phis = [] })
      else None
    in
    let phis = [ Riddler.cond_bottom this_key term_c cond_block ] in
    Chain { sub = this_key; pub = (term_c, block); next; phis }

  let fun_enter_local p (key : Lookup_key.t) block =
    let this_key = key in
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let _x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let nexts =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          let b_id = Cfg.id_of_block callsite_block in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack b_id in
              let next this_key (_r : Lookup_result.t) =
                let key_arg = Lookup_key.of3 x''' callsite_stack b_id in
                Some
                  (Direct
                     {
                       sub = this_key;
                       pub = (key_arg, callsite_block);
                       phis = [];
                     })
              in
              Chain
                {
                  sub = this_key;
                  pub = (key_f, callsite_block);
                  next;
                  phis = [];
                }
          | None -> failwith "why Rstack.pop fails here")
    in
    let phis = [ Riddler.fun_enter_local this_key fid callsites S.block_map ] in
    Or_list { sub = this_key; nexts; unbound = false; phis }

  let fun_enter_nonlocal p (key : Lookup_key.t) block =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let x, r_stk = Lookup_key.to2 key in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let nexts =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          let b_id = Cfg.id_of_block callsite_block in
          match Rstack.pop r_stk (x', fb.point) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of3 x'' callsite_stack b_id in
              let next i (r : Lookup_result.t) =
                let key_arg = Lookup_key.of3 x r.from.r_stk b_id in
                let phi_i =
                  Riddler.fun_enter_nonlocal key key_f r.from fb.point key_arg
                in
                let fv_block = Cfg.block_of_id r.from.x S.block_map in
                let action =
                  Direct { sub = key; pub = (key_arg, fv_block); phis = [] }
                in
                Some (phi_i, action)
              in
              Sequence
                { sub = key; pub = (key_f, callsite_block); next; phis = [] }
          | None -> failwith "why Rstack.pop fails here")
    in
    Or_list { sub = key; nexts; unbound = true; phis = [] }

  let fun_exit p (key : Lookup_key.t) block =
    let this_key = key in
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of3 xf r_stk (Cfg.id_of_block block) in
    let next this_key (rf : Lookup_result.t) =
      let fid = rf.from.x in
      if List.mem fids fid ~equal:Id.equal
      then
        let fblock = Ident_map.find fid S.block_map in
        let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in
        Some (Direct { sub = this_key; pub = (key_ret, fblock); phis = [] })
      else None
    in
    let phis = [ Riddler.fun_exit this_key key_f fids S.block_map ] in
    Chain { sub = this_key; pub = (key_f, block); next; phis }

  let pattern p (key : Lookup_key.t) block =
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
      let rv_block = Cfg.block_of_id key_rv.x S.block_map in
      let rv = Cfg.clause_body_of_x rv_block key_rv.x in

      (* Fmt.pr "[Pattern] %a | %a | %d <- %a\n" Lookup_key.pp key
         Jayil.Ast_pp.pp_pattern pat i Lookup_key.pp key_rv ; *)
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
      (ans, phis)
    in
    MapSeq { sub = key; pub = (key', block); map = next; phis = [] }

  let assume _p (key : Lookup_key.t) = Withered { phis = [] }

  let assert_ _p (key : Lookup_key.t) =
    Withered { phis = [ Riddler.mismatch_with_picked key ] }

  let abort p (key : Lookup_key.t) block =
    if Lookup_key.equal key
         (Lookup_key.start S.config.target (Cfg.id_of_block block))
       (* TODO: take care of direct `abort` in the main block *)
    then rule_nonmain None p key block
    else Withered { phis = [ Riddler.mismatch_with_picked key ] }

  let mismatch (key : Lookup_key.t) =
    Withered { phis = [ Riddler.mismatch_with_picked key ] }
end