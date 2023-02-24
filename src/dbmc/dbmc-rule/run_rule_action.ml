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

let exists_status map lookups f =
  List.exists lookups ~f:(fun lookup ->
      let (detail : Term_detail.t) = Hashtbl.find_exn map lookup in
      f detail.status)

let for_all_status map lookups f =
  List.for_all lookups ~f:(fun lookup ->
      let (detail : Term_detail.t) = Hashtbl.find_exn map lookup in
      f detail.status)

let rec run run_task unroll (state : Global_state.t)
    (term_detail : Term_detail.t) rule_action =
  let loop rule_action = run run_task unroll state term_detail @@ rule_action in
  let add_phi = Global_state.add_phi state in
  let mark_and_id (r : Lookup_result.t) =
    if Lookup_status.is_ok term_detail.status
    then term_detail.status <- r.status
    else () ;
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
        (* ans *)
      in
      U.by_map_u unroll e.sub e.pub f ;
      run_task e.pub
  | Both e ->
      U.by_map2_u unroll e.sub e.pub1 e.pub2 (fun (v1, v2) ->
          Lookup_result.(status_as (ok e.sub) (status_join v1 v2))) ;
      run_task e.pub1 ;
      run_task e.pub2
  | Chain e ->
      (* Fmt.pr "\n[Chain start]%a = %a -> " Lookup_key.pp e.sub Lookup_key.pp
         e.pub ; *)
      (* Fmt.pr "\n[pre_push][X]%a = %a in %a " Lookup_key.pp e.sub Lookup_key.pp
           e.pub Lookup_status.pp term_detail.status ;
         Fmt.pr "\n[pre_push][R]<- %a in %a" Lookup_key.pp r.from
           Lookup_status.pp r.status ; *)
      let part1_done = ref false in
      let part1_cb key (r : Lookup_result.t) =
        if Lookup_status.is_complete_or_fail r.status
        then part1_done := true
        else () ;

        Lookup_status.iter_ok r.status (fun () ->
            match e.next key r with
            | Some edge ->
                term_detail.sub_lookups <-
                  term_detail.sub_lookups @ [ Rule_action.sub_of edge ] ;
                loop edge
            | None -> ()) ;
        Lwt.return_unit
      in
      let part2_done = ref false in
      let pre_push (r : Lookup_result.t) =
        Lookup_status.iter_ok r.status (fun () ->
            if !part1_done && not !part2_done
            then
              if for_all_status state.term_detail_map term_detail.sub_lookups
                   Lookup_status.is_complete_or_fail
              then (
                part2_done := true ;
                let this_status =
                  exists_status state.term_detail_map term_detail.sub_lookups
                    Lookup_status.is_complete
                  |> Lookup_status.complete_or_fail
                in
                term_detail.status <- this_status)
              else ()) ;
        Some r
      in
      U.by_bind_u unroll e.sub e.pub part1_cb ;
      U.set_pre_push unroll e.sub pre_push ;
      run_task e.pub
  | Sequence e ->
      init_list_counter state term_detail e.sub ;
      let part1_done = ref false in
      let part1_cb _key (r : Lookup_result.t) =
        if Lookup_status.is_complete_or_fail r.status
        then part1_done := true
        else () ;

        Lookup_status.iter_ok r.status (fun () ->
            let i = fetch_list_counter state term_detail e.sub in
            let next = e.next i r in
            match next with
            | Some (phi_i, next) ->
                let phi = Riddler.list_append e.sub i phi_i in
                add_phi term_detail phi ;
                term_detail.sub_lookups <-
                  term_detail.sub_lookups @ [ Rule_action.sub_of next ] ;
                loop next
            | None ->
                add_phi term_detail (Riddler.list_append e.sub i Riddler.false_)) ;
        Lwt.return_unit
      in
      let part2_done = ref false in
      let pre_push (r : Lookup_result.t) =
        Lookup_status.iter_ok r.status (fun () ->
            if !part1_done && not !part2_done
            then
              if for_all_status state.term_detail_map term_detail.sub_lookups
                   Lookup_status.is_complete_or_fail
              then (
                part2_done := true ;
                let this_status =
                  exists_status state.term_detail_map term_detail.sub_lookups
                    Lookup_status.is_complete
                  |> Lookup_status.complete_or_fail
                in
                term_detail.status <- this_status)
              else ()) ;
        Some r
      in
      U.by_bind_u unroll e.sub e.pub part1_cb ;
      U.set_pre_push unroll e.sub pre_push ;
      run_task e.pub
  | Or_list e ->
      if e.unbound then init_list_counter state term_detail e.sub else () ;
      (* TODO *)
      List.iter e.elements ~f:(fun e ->
          term_detail.sub_lookups <-
            term_detail.sub_lookups @ [ Rule_action.sub_of e ] ;
          loop e)) ;

  add_phi_edge state term_detail rule_action
