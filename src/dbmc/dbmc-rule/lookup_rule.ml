open Core
open Odefa_ast
open Odefa_ast.Ast
open Log.Export
open Rule
module U = Unrolls.U_dbmc
open Types

module type S = sig
  val state : Global_state.t
  val config : Global_config.t
  val block_map : Cfg.block Odefa_ast.Ast.Ident_map.t
  val unroll : U.t
  val stride : int ref
end

let add_phi (state : Global_state.t) (term_detail : Term_detail.t) phi =
  let phi' =
    Option.value_map term_detail.phi ~default:phi ~f:(fun phi' ->
        Riddler.and_ [ phi'; phi ])
  in
  term_detail.phi <- Some phi' ;
  state.phis_z3 <- phi :: state.phis_z3

module Make (S : S) = struct
  open Edge

  let add_phi (term_detail : Term_detail.t) phi =
    add_phi S.state term_detail phi

  let init_list_counter (term_detail : Term_detail.t) key =
    Hashtbl.update S.state.smt_lists key ~f:(function
      | Some i -> i
      | None ->
          add_phi term_detail (Riddler.list_head key) ;
          0)

  let fetch_list_counter (term_detail : Term_detail.t) key =
    let new_i =
      Hashtbl.update_and_return S.state.smt_lists key ~f:(function
        | Some i -> i + 1
        | None ->
            add_phi term_detail (Riddler.list_head key) ;
            1)
    in
    new_i - 1

  (* for a compositiona edge, if a seq with indexed at key will be used later,
       the user needs to `init_list_counter` eagerly once.
     The reason for lazy init is the callback may never be called at all. The fix of
     infinitive list cannot be applied before calling the SMT solver.
  *)
  let rec run_edge run_task (term_detail : Term_detail.t) edge phi =
    add_phi term_detail phi ;
    match edge with
    | Leaf e -> U.by_return S.unroll e.sub (Lookup_result.ok e.sub)
    | Direct e ->
        run_task e.pub e.block ;
        U.by_id_u S.unroll e.sub e.pub
    | Map e ->
        run_task e.pub e.block ;
        U.by_map_u S.unroll e.sub e.pub e.map
    | Both e ->
        run_task e.pub1 e.block ;
        run_task e.pub2 e.block ;
        U.by_map2_u S.unroll e.sub e.pub1 e.pub2 (fun _ ->
            Lookup_result.ok e.sub)
    | Chain e ->
        let cb key r =
          let edge = e.next key r in
          (match edge with
          | Some edge -> run_edge run_task term_detail edge Riddler.true_
          | None -> ()) ;
          Lwt.return_unit
        in
        U.by_bind_u S.unroll e.sub e.pub cb ;
        run_task e.pub e.block
    | Sequence e ->
        init_list_counter term_detail e.sub ;

        run_task e.pub e.block ;
        let cb _key (r : Lookup_result.t) =
          let i = fetch_list_counter term_detail e.sub in
          let next = e.next i r in
          (match next with
          | Some (phi_i, next) -> run_edge run_task term_detail next phi_i
          | None -> add_phi term_detail (Riddler.list_append_mismatch e.sub i)) ;
          Lwt.return_unit
        in
        U.by_bind_u S.unroll e.sub e.pub cb
    | Or_list e ->
        List.iter e.nexts ~f:(fun e ->
            run_edge run_task term_detail e Riddler.true_)
  (* | Direct_bind e ->
     run_task e.pub e.block ;
     U.by_bind_u S.unroll e.sub e.pub e.cb *)

  let rule_main v _p term_detail (key : Lookup_key.t) block run_task =
    let target_stk = Rstack.concretize_top key.r_stk in
    let phi = Riddler.discover_main_with_picked key v in
    let edge = Leaf { sub = key } in
    run_edge run_task term_detail edge phi

  let rule_nonmain v _p term_detail (key : Lookup_key.t) block run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    let phi = Riddler.discover_non_main key key_first v in
    let edge =
      Map
        {
          sub = key;
          pub = key_first;
          block;
          map = (fun _ -> Lookup_result.ok key);
        }
    in
    run_edge run_task term_detail edge phi

  let discovery_main p term_detail (key : Lookup_key.t) block run_task =
    let ({ v; _ } : Discovery_main_rule.t) = p in
    rule_main (Some v) p term_detail key block run_task

  let discovery_nonmain p term_detail (key : Lookup_key.t) block run_task =
    let ({ v; _ } : Discovery_nonmain_rule.t) = p in
    rule_nonmain (Some v) p term_detail key block run_task

  let input p term_detail (key : Lookup_key.t) block run_task =
    let ({ is_in_main; _ } : Input_rule.t) = p in
    Hash_set.add S.state.input_nodes key ;
    if is_in_main
    then rule_main None p term_detail key block run_task
    else rule_nonmain None p term_detail key block run_task

  let alias p term_detail (key : Lookup_key.t) block run_task =
    let ({ x'; _ } : Alias_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let edge = Direct { sub = key; pub = key'; block } in
    let phi = Riddler.eq_with_picked key key' in
    run_edge run_task term_detail edge phi

  let not_ p term_detail (key : Lookup_key.t) block run_task =
    let ({ x'; _ } : Not_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let edge = Direct { sub = key; pub = key'; block } in
    let phi = Riddler.not_with_picked key key' in
    run_edge run_task term_detail edge phi

  let binop b term_detail (key : Lookup_key.t) block run_task =
    let ({ bop; x1; x2; _ } : Binop_rule.t) = b in
    let key_x1 = Lookup_key.with_x key x1 in
    let key_x2 = Lookup_key.with_x key x2 in
    let edge =
      Both
        {
          sub = key;
          pub1 = Lookup_key.with_x key x1;
          pub2 = Lookup_key.with_x key x2;
          block;
        }
    in
    let phi = Riddler.binop_with_picked key bop key_x1 key_x2 in
    run_edge run_task term_detail edge phi

  let record_start p term_detail (key : Lookup_key.t) block run_task =
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
              let phi_i =
                Riddler.record_start_append key key_r key_rv key_l i
              in
              let edge = Direct { sub = key; pub = key_l; block = rv_block } in
              Some (phi_i, edge)
          | None -> None)
      | _ -> None
    in

    let edge = Sequence { sub = key; pub = key_r; block; next } in
    let phi = Riddler.true_ in
    run_edge run_task term_detail edge phi

  let record_end p term_detail (key : Lookup_key.t) block run_task =
    let ({ r; is_in_main; _ } : Record_end_rule.t) = p in
    let rv = Some (Value_record r) in
    if is_in_main
    then rule_main rv p term_detail key block run_task
    else rule_nonmain rv p term_detail key block run_task

  let cond_top (cb : Cond_top_rule.t) term_detail (key : Lookup_key.t) block
      run_task =
    let condsite_block = Cfg.outer_block block S.block_map in
    let x, r_stk = Lookup_key.to2 key in
    let choice = Option.value_exn cb.choice in
    let _paired, condsite_stack =
      Rstack.pop_at_condtop r_stk (cb.point, Id.cond_fid choice)
    in
    let x2 = cb.cond in
    let key_x2 = Lookup_key.of2 x2 condsite_stack in
    let key_x = Lookup_key.of2 x condsite_stack in
    let phi = Riddler.cond_top key key_x key_x2 choice in
    let next _ r =
      (* true *)
      (* if Riddler.eager_check S.state S.config key_x2
           [ Riddler.eqv key_x2 (Value_bool choice) ] *)
      Some (Direct { sub = key; pub = key_x; block = condsite_block })
    in
    let edge =
      Chain { sub = key; pub = key_x2; block = condsite_block; next }
    in
    run_edge run_task term_detail edge phi

  let cond_btm p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block = Ident_map.find tid S.block_map |> Cfg.cast_to_cond_block in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.with_x this_key x' in
    let phi = Riddler.cond_bottom this_key term_c cond_block in
    let next _ (r : Lookup_result.t) =
      if r.status
      then
        let sub = this_key in
        let nexts =
          List.filter_map [ true; false ] ~f:(fun beta ->
              if true
                 (* Riddler.step_eager_check S.state S.config term_c
                     [ Riddler.eqv term_c (Value_bool beta) ]
                     S.stride *)
              then
                let case_block, key_ret =
                  Lookup_key.get_cond_block_and_return cond_block beta
                    this_key.r_stk x
                in
                Some
                  (Direct { sub = this_key; pub = key_ret; block = case_block })
              else None)
        in
        Some (Or_list { sub; nexts })
      else None
    in
    let edge = Chain { sub = this_key; pub = term_c; block; next } in
    run_edge run_task term_detail edge phi

  let fun_enter_local p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let _x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let phi = Riddler.fun_enter_local this_key fid callsites S.block_map in
    add_phi term_detail phi ;

    let nexts =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let next this_key (_r : Lookup_result.t) =
                let key_arg = Lookup_key.of2 x''' callsite_stack in
                Some
                  (Direct
                     { sub = this_key; pub = key_arg; block = callsite_block })
              in
              Chain
                { sub = this_key; pub = key_f; block = callsite_block; next }
          | None -> failwith "why Rstack.pop fails here")
    in
    let edge = Or_list { sub = this_key; nexts } in
    run_edge run_task term_detail edge phi

  let fun_enter_nonlocal p term_detail (key : Lookup_key.t) block run_task =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let x, r_stk = Lookup_key.to2 key in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let nexts =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fb.point) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let next i (r : Lookup_result.t) =
                let key_arg = Lookup_key.of2 x r.from.r_stk in
                let phi_i =
                  Riddler.fun_enter_append key key_f r.from fb.point key_arg i
                in
                let fv_block = Cfg.block_of_id r.from.x S.block_map in
                let edge =
                  Direct { sub = key; pub = key_arg; block = fv_block }
                in
                Some (phi_i, edge)
              in
              Sequence { sub = key; pub = key_f; block = callsite_block; next }
          | None -> failwith "why Rstack.pop fails here")
    in
    init_list_counter term_detail key ;
    let edge = Or_list { sub = key; nexts } in
    run_edge run_task term_detail edge Riddler.true_

  let fun_exit p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of2 xf r_stk in
    let phi = Riddler.fun_exit this_key key_f fids S.block_map in
    let next this_key (rf : Lookup_result.t) =
      let fid = rf.from.x in
      if List.mem fids fid ~equal:Id.equal
      then
        let fblock = Ident_map.find fid S.block_map in
        let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in
        Some (Direct { sub = this_key; pub = key_ret; block = fblock })
      else None
    in
    let edge = Chain { sub = this_key; pub = key_f; block; next } in
    run_edge run_task term_detail edge phi

  let pattern p term_detail (key : Lookup_key.t) block run_task =
    let ({ x'; pat; _ } : Pattern_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let map (r : Lookup_result.t) =
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

      let ans, _matched =
        match (pat, rv) with
        | Any_pattern, _
        | Fun_pattern, Value_body (Value_function _)
        | Int_pattern, Value_body (Value_int _)
        | Int_pattern, Input_body
        | Bool_pattern, Value_body (Value_bool _) ->
            let phi = Riddler.eqv_with_picked key key' (Value_bool true) in
            add_phi term_detail phi ;
            let phi = Riddler.picked_pattern key key' pat in
            add_phi term_detail phi ;

            (Lookup_result.ok key, true)
        | Rec_pattern ids, Value_body (Value_record (Record_value rv)) ->
            let have_all =
              Ident_set.for_all (fun id -> Ident_map.mem id rv) ids
            in
            let phi =
              Riddler.picked_record_pattern key key' (Value_bool have_all) pat
            in
            add_phi term_detail phi ;
            (Lookup_result.ok key, true)
        | Rec_pattern _, _ | _, Value_body _ ->
            let phi = Riddler.eqv_with_picked key key' (Value_bool false) in
            add_phi term_detail phi ;
            let phi = Riddler.picked_pattern key key' pat in
            add_phi term_detail phi ;

            (Lookup_result.ok key, false)
        | _, _ ->
            (* TODO: some binops contain type information for patterns *)
            (* TODO: and for previous pattern match *)
            let phi = Riddler.picked_pattern key key' pat in
            add_phi term_detail phi ;

            (Lookup_result.ok key, false)
      in
      ans
    in
    let edge = Map { sub = key; pub = key'; block; map } in
    run_edge run_task term_detail edge Riddler.true_

  let assume _p _term_detail (key : Lookup_key.t) block run_task = ()

  let assert_ _p term_detail (key : Lookup_key.t) block run_task =
    (* update_rule this_key Node.mismatch ; *)
    add_phi term_detail (Riddler.mismatch_with_picked key)

  let abort p term_detail (key : Lookup_key.t) block run_task =
    if Lookup_key.equal key (Lookup_key.start S.config.target)
       (* TODO: take care of direct `abort` in the main block *)
    then rule_nonmain None p term_detail key block run_task
    else add_phi term_detail (Riddler.mismatch_with_picked key)

  let mismatch term_detail (key : Lookup_key.t) block run_task =
    add_phi term_detail (Riddler.mismatch_with_picked key)
end