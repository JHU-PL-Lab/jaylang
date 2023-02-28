open Core
open Dj_common
module U = Unrolls.U_dbmc

let init_list_counter (state : Global_state.t) (term_detail : Term_detail.t) key
    =
  Hashtbl.update state.smt_lists key ~f:(function
    | Some i -> i
    | None ->
        Global_state.add_phi state term_detail (Riddler.list_head key) ;
        0)

let fetch_list_counter (state : Global_state.t) (term_detail : Term_detail.t)
    key =
  let new_i =
    Hashtbl.update_and_return state.smt_lists key ~f:(function
      | Some i -> i + 1
      | None ->
          (* add_phi state term_detail (Riddler.list_head key) *)
          failwith (Fmt.str "why not inited : %a" Lookup_key.pp key))
  in
  new_i - 1

let add_phi_edge state term_detail edge =
  List.iter (Rule_action.phis_of edge)
    ~f:(Global_state.add_phi state term_detail)

let fold_lookups_status map lookups =
  let open Lookup_status in
  List.fold_until lookups ~init:Good
    ~f:(fun acc_s lookup ->
      let (detail : Term_detail.t) = Hashtbl.find_exn map lookup in
      match (detail.status, acc_s) with
      | Good, _ -> Stop Good
      | Complete, _ -> Continue Complete
      | Fail, Complete -> Continue Complete
      | Fail, _ -> Continue Fail)
    ~finish:Fn.id

let pre_push_for_seq map lookups (td : Term_detail.t) part1_done part2_done
    (r : Lookup_result.t) =
  Lookup_status.iter_ok r.status (fun () ->
      if !part1_done && not !part2_done
      then
        let status' = fold_lookups_status map lookups in
        if Lookup_status.is_complete_or_fail status'
        then (
          part2_done := true ;
          td.status <- status')) ;
  Some r

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

let rec run run_task unroll (state : Global_state.t)
    (term_detail : Term_detail.t) rule_action =
  let add_then_loop action =
    term_detail.sub_lookups <-
      term_detail.sub_lookups @ [ Rule_action.sub_of action ] ;
    run run_task unroll state term_detail @@ action
  in
  let add_phi = Global_state.add_phi state in
  let mark_and_id (r : Lookup_result.t) =
    if Lookup_status.is_ok term_detail.status
    then term_detail.status <- r.status ;
    r
  in
  let open Rule_action in
  (match (rule_action : Rule_action.t) with
  | Withered e -> term_detail.status <- Lookup_status.Fail
  | Leaf e ->
      term_detail.status <- Lookup_status.Complete ;
      U.by_return unroll e.sub (Lookup_result.complete e.sub)
  | Direct e ->
      U.by_map_u unroll e.sub e.pub mark_and_id ;
      run_task e.pub
  | Map e ->
      U.by_map_u unroll e.sub e.pub (fun r -> e.map (mark_and_id r)) ;
      run_task e.pub
  | MapSeq e ->
      init_list_counter state term_detail e.sub ;
      let f r =
        let i = fetch_list_counter state term_detail e.sub in
        let ans, phis = e.map i (mark_and_id r) in
        add_phi term_detail (Riddler.list_append e.sub i (Riddler.and_ phis)) ;
        Lookup_result.status_as ans r.status
      in
      U.by_map_u unroll e.sub e.pub f ;
      run_task e.pub
  | Both e ->
      U.by_map2_u unroll e.sub e.pub1 e.pub2 (fun (v1, v2) ->
          let joined_status = Lookup_status.join v1.status v2.status in
          let open Lookup_status in
          (match (term_detail.status, joined_status) with
          | Good, Good -> ()
          | Good, _ -> term_detail.status <- joined_status
          | _, _ -> failwith "failed in both") ;
          Lookup_result.(status_as (ok e.sub) joined_status)) ;
      run_task e.pub1 ;
      run_task e.pub2
  | Chain e ->
      let part1_done = ref false in
      let part1_cb key (r : Lookup_result.t) =
        if Lookup_status.is_complete_or_fail r.status then part1_done := true ;
        Lookup_status.iter_ok r.status (fun () ->
            match e.next key r with
            | Some edge -> add_then_loop edge
            | None -> ()) ;
        Lwt.return_unit
      in
      let part2_done = ref false in
      let pre_push =
        pre_push_for_seq state.term_detail_map term_detail.sub_lookups
          term_detail part1_done part2_done
      in
      U.by_bind_u unroll e.sub e.pub part1_cb ;
      U.set_pre_push unroll e.sub pre_push ;
      run_task e.pub
  | Sequence e ->
      init_list_counter state term_detail e.sub ;
      let part1_done = ref false in
      let part1_cb _key (r : Lookup_result.t) =
        if Lookup_status.is_complete_or_fail r.status then part1_done := true ;

        Lookup_status.iter_ok r.status (fun () ->
            let i = fetch_list_counter state term_detail e.sub in
            let next = e.next i r in
            match next with
            | Some (phi_i, edge) ->
                let phi = Riddler.list_append e.sub i phi_i in
                add_phi term_detail phi ;
                add_then_loop edge
            | None ->
                add_phi term_detail (Riddler.list_append e.sub i Riddler.false_)) ;
        Lwt.return_unit
      in
      let part2_done = ref false in
      let pre_push =
        pre_push_for_seq state.term_detail_map term_detail.sub_lookups
          term_detail part1_done part2_done
      in
      U.by_bind_u unroll e.sub e.pub part1_cb ;
      U.set_pre_push unroll e.sub pre_push ;
      run_task e.pub
  | Or_list e ->
      if e.unbound then init_list_counter state term_detail e.sub ;
      List.iter e.elements ~f:add_then_loop ;
      let pre_push (r : Lookup_result.t) =
        let status' =
          fold_lookups_status state.term_detail_map term_detail.sub_lookups
        in
        Some r
      in
      U.set_pre_push unroll e.sub pre_push) ;

  add_phi_edge state term_detail rule_action
