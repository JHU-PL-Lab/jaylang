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
      let part1_done = ref false in
      let cb key (r : Lookup_result.t) =
        (match r.status with
        | Fail -> ()
        | Complete | Good -> (
            part1_done := true ;
            let edge = e.next key r in
            match edge with
            | Some edge ->
                term_detail.sub_lookups <-
                  term_detail.sub_lookups @ [ Rule_action.sub_of edge ] ;
                loop edge
            | None -> ())) ;
        Lwt.return_unit
      in
      let pre_push (r : Lookup_result.t) =
        (* Fmt.pr "\n[pre_push][X]%a = %a in %a " Lookup_key.pp e.sub Lookup_key.pp
             e.pub Lookup_status.pp term_detail.status ;
           Fmt.pr "\n[pre_push][R]<- %a in %a" Lookup_key.pp r.from
             Lookup_status.pp r.status ; *)
        (match r.status with
        | Fail -> ()
        | Complete | Good -> if !part1_done then () else ()) ;
        Some r
        (* ; *)
      in

      (* =
           (match r.status with
           | Fail | Complete ->
               Lwt.async (fun () ->
                   (* Fmt.pr "\n[Chain end (step1)]%a = %a -> " Lookup_key.pp e.sub
                      Lookup_key.pp e.pub ; *)
                   let%lwt all_complete =
                     Lwt_list.for_all_s
                       (fun sub_key ->
                         (* Fmt.pr "%a; " Lookup_key.pp sub_key ; *)
                         let sub_s = U.get_stream unroll sub_key in
                         Lwt_stream.junk_while Lookup_result.is_good sub_s ;%lwt
                         Lwt.map Lookup_result.is_complete (Lwt_stream.next sub_s))
                       term_detail.sub_lookups
                   in
                   (* Fmt.pr "\n[Chain end (step2)]%a = %a <- " Lookup_key.pp e.sub
                      Lookup_key.pp e.pub ; *)
                   term_detail.is_complete_or_fail <- all_complete ;
                   Lwt.return_unit)
           | Good -> ()) ;
         in *)
      U.by_bind_u unroll e.sub e.pub cb ;
      U.set_pre_push unroll e.sub pre_push ;
      run_task e.pub
  | Sequence e ->
      init_list_counter state term_detail e.sub ;

      let cb _key (r : Lookup_result.t) =
        let i = fetch_list_counter state term_detail e.sub in
        let next = e.next i r in
        (match next with
        | Some (phi_i, next) ->
            let phi = Riddler.list_append e.sub i phi_i in
            add_phi term_detail phi ;
            loop next
        | None ->
            add_phi term_detail (Riddler.list_append e.sub i Riddler.false_)) ;
        Lwt.return_unit
      in
      U.by_bind_u unroll e.sub e.pub cb ;
      run_task e.pub
  | Or_list e ->
      if e.unbound then init_list_counter state term_detail e.sub else () ;
      List.iter e.elements ~f:(fun e -> loop e)) ;
  add_phi_edge state term_detail rule_action
