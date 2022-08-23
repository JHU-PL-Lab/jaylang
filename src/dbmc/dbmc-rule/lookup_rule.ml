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

  let rec run_edge run_task edge (term_detail : Term_detail.t) block phi =
    add_phi term_detail phi ;
    match edge with
    | Direct e ->
        run_task e.pub block ;
        U.by_id_u S.unroll e.sub e.pub
    | Direct_map e ->
        run_task e.pub block ;
        U.by_map_u S.unroll e.sub e.pub e.map
    | Direct_bind e ->
        run_task e.pub block ;
        U.by_bind_u S.unroll e.sub e.pub e.cb
    | Both e ->
        run_task e.pub1 block ;
        run_task e.pub2 block ;
        U.by_map2_u S.unroll e.sub e.pub1 e.pub2 (fun _ ->
            Lookup_result.ok e.sub)
    | Or_list e ->
        List.iter e.pub_with_blocks ~f:(fun (key, block) ->
            run_task key block ;
            U.by_id_u S.unroll e.sub key)
    | Or_list_two_phases e ->
        List.iter e.pub_details ~f:(fun (key, block, cb) ->
            run_task key block ;
            U.by_bind_u S.unroll e.sub key cb)
    | Two_phases e ->
        let cb_lazy key (_rc : Lookup_result.t) =
          if true
             (* if Riddler.eager_check S.state S.config key_x2
                  [ Riddler.eqv key_x2 (Value_bool choice) ] *)
          then (
            run_task e.pub_lazy e.block_lazy ;
            U.by_id_u S.unroll key e.pub_lazy)
          else () ;
          Lwt.return_unit
        in
        U.by_bind_u S.unroll e.sub e.pub cb_lazy ;
        run_task e.pub e.block_pub
    | Two_phases_lazy e ->
        run_task e.pub block ;
        let cb key result =
          if e.pre_lazy_check result
          then run_edge run_task (Lazy.force e.lazy_edge) term_detail block phi
          else () ;
          Lwt.return_unit
        in
        U.by_bind_u S.unroll e.sub e.pub cb
    | Seq_on_pub e ->
        run_task e.pub block ;
        let counter = ref 0 in
        add_phi term_detail (Riddler.list_head e.sub) ;
        Hashtbl.add_exn S.state.smt_lists ~key:e.sub ~data:0 ;
        let cb this_key (r : Lookup_result.t) =
          let i = !counter in
          Int.incr counter ;
          (* update for next i *)
          Hashtbl.update S.state.smt_lists e.sub ~f:(function
            | Some _ -> i + 1
            | None -> failwith "smt list key") ;
          (* use this i *)
          if e.seq_on_pub i r
          then ()
          else add_phi term_detail (Riddler.list_append_mismatch e.sub i) ;
          Lwt.return_unit
        in
        U.by_bind_u S.unroll e.sub e.pub cb
    | Seq_for_sub e ->
        add_phi term_detail (Riddler.list_head e.sub) ;
        Hashtbl.add_exn S.state.smt_lists ~key:e.sub ~data:0 ;
        let nonlocal_i = ref 0 in

        let cb_with_i cb key r =
          let i = !nonlocal_i in
          Int.incr nonlocal_i ;
          Hashtbl.update S.state.smt_lists e.sub ~f:(function
            | Some _ -> !nonlocal_i
            | None -> failwith "smt list key") ;
          cb i key r
        in
        List.iter e.pub_with_cbs ~f:(fun (key, block, cb) ->
            run_task key block ;
            U.by_bind_u S.unroll e.sub key (cb_with_i cb))

  let rule_main v _p term_detail (key : Lookup_key.t) block run_task =
    let target_stk = Rstack.concretize_top key.r_stk in
    add_phi term_detail (Riddler.discover_main_with_picked key v) ;
    U.by_return S.unroll key (Lookup_result.ok key)

  let rule_nonmain v _p term_detail (key : Lookup_key.t) block run_task =
    let key_first = Lookup_key.to_first key S.state.first in
    add_phi term_detail (Riddler.discover_non_main key key_first v) ;
    run_task key_first block ;
    U.by_map_u S.unroll key key_first (fun _ -> Lookup_result.ok key)
  (* let node_child = S.find_or_add_node key_first block this_node in
     Node.update_rule this_node (Node.to_first node_child) ; *)

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
    let edge = Direct { sub = key; pub = key' } in
    let phi = Riddler.eq_with_picked key key' in
    run_edge run_task edge term_detail block phi

  let not_ p term_detail (key : Lookup_key.t) block run_task =
    let ({ x'; _ } : Not_rule.t) = p in
    let key' = Lookup_key.with_x key x' in
    let edge = Direct { sub = key; pub = key' } in
    let phi = Riddler.not_with_picked key key' in
    run_edge run_task edge term_detail block phi

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
        }
    in
    let phi = Riddler.binop_with_picked key bop key_x1 key_x2 in
    run_edge run_task edge term_detail block phi

  let record_start p term_detail (key : Lookup_key.t) block run_task =
    let ({ r; lbl; _ } : Record_start_rule.t) = p in
    let key_r = Lookup_key.with_x key r in
    let seq_on_pub i (r : Lookup_result.t) =
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
              let edge = Direct { sub = key; pub = key_l } in
              run_edge run_task edge term_detail rv_block phi_i ;
              true
          | None -> false)
      | _ -> false
    in

    let edge = Seq_on_pub { sub = key; pub = key_r; seq_on_pub } in
    let phi = Riddler.true_ in
    run_edge run_task edge term_detail block phi

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

    let edge =
      Two_phases
        {
          sub = key;
          pub = key_x2;
          block_pub = condsite_block;
          pub_lazy = key_x;
          block_lazy = condsite_block;
        }
    in
    run_edge run_task edge term_detail block phi

  let cond_btm p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let ({ x; x'; tid } : Cond_btm_rule.t) = p in
    let cond_block = Ident_map.find tid S.block_map |> Cfg.cast_to_cond_block in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    let term_c = Lookup_key.with_x this_key x' in
    let phi = Riddler.cond_bottom this_key term_c cond_block in
    let lazy_edge : Edge.t Lazy.t =
      let sub = this_key in
      let pub_with_blocks =
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
              Some (key_ret, case_block)
            else None)
      in
      lazy (Or_list { sub; pub_with_blocks })
    in
    let edge =
      Two_phases_lazy
        {
          sub = this_key;
          pub = term_c;
          pre_lazy_check = (fun c -> c.status);
          lazy_edge;
        }
    in
    run_edge run_task edge term_detail block phi

  let fun_enter_local p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let ({ fb; _ } : Fun_enter_local_rule.t) = p in
    let _x, r_stk = Lookup_key.to2 this_key in
    let fid = fb.point in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let phi = Riddler.fun_enter_local this_key fid callsites S.block_map in
    add_phi term_detail phi ;

    let pub_details =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fid) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in

              let cb this_key (_r : Lookup_result.t) =
                let key_arg = Lookup_key.of2 x''' callsite_stack in

                run_task key_arg callsite_block ;
                U.by_id_u S.unroll this_key key_arg ;
                Lwt.return_unit
              in

              (key_f, callsite_block, cb)
          | None -> failwith "why Rstack.pop fails here")
    in
    let edge = Or_list_two_phases { sub = this_key; pub_details } in
    run_edge run_task edge term_detail block phi

  let fun_enter_nonlocal p term_detail (key : Lookup_key.t) block run_task =
    let ({ fb; _ } : Fun_enter_nonlocal_rule.t) = p in
    let x, r_stk = Lookup_key.to2 key in
    let callsites = Lookup_key.get_callsites r_stk fb in
    let pub_with_cbs =
      List.map callsites ~f:(fun callsite ->
          let callsite_block, x', x'', _x''' =
            Cfg.fun_info_of_callsite callsite S.block_map
          in
          match Rstack.pop r_stk (x', fb.point) with
          | Some callsite_stack ->
              let key_f = Lookup_key.of2 x'' callsite_stack in
              let cb i key (r : Lookup_result.t) =
                let key_arg = Lookup_key.of2 x r.from.r_stk in
                let phi_i =
                  Riddler.fun_enter_append key key_f r.from fb.point key_arg i
                in
                add_phi term_detail phi_i ;
                let fv_block = Cfg.block_of_id r.from.x S.block_map in
                run_task key_arg fv_block ;
                U.by_id_u S.unroll key key_arg ;
                Lwt.return_unit
              in
              (key_f, callsite_block, cb)
          | None -> failwith "why Rstack.pop fails here")
    in
    let edge = Seq_for_sub { sub = key; pub_with_cbs } in
    run_edge run_task edge term_detail block Riddler.true_

  let fun_exit p term_detail (key : Lookup_key.t) block run_task =
    let this_key = key in
    let _x, r_stk = Lookup_key.to2 this_key in
    let ({ x; xf; fids } : Fun_exit_rule.t) = p in
    let key_f = Lookup_key.of2 xf r_stk in
    let phi = Riddler.fun_exit this_key key_f fids S.block_map in
    let cb this_key (rf : Lookup_result.t) =
      let fid = rf.from.x in
      if List.mem fids fid ~equal:Id.equal
      then (
        let fblock = Ident_map.find fid S.block_map in
        let key_ret = Lookup_key.get_f_return S.block_map fid r_stk x in

        run_task key_ret fblock ;
        U.by_id_u S.unroll this_key key_ret ;
        Lwt.return_unit)
      else Lwt.return_unit
    in
    let edge = Direct_bind { sub = this_key; pub = key_f; cb } in
    run_edge run_task edge term_detail block phi

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
    let edge = Direct_map { sub = key; pub = key'; map } in
    run_edge run_task edge term_detail block Riddler.true_

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