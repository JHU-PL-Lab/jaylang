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

(* CAUTION: `sub` is not the id by a lookup.
         `sub` is the _id_ for lookup task creation. However, multiple streams can push to this `sub`.
         `pub` is the _id_ for a lookup result.

         These lookups won't call this function:
         Leaf-lookup has no `pub` because itself is a sub and a pub.
         Both-lookup has two `pub`s.
         Map-lookup and MapSeq has one `pub`.
*)
(* Option.iter (Rule_action.source_of action) ~f:(fun source ->
    term_detail.sub_lookups <- term_detail.sub_lookups @ [ source ]) ; *)

let fold_lookups_status map lookups =
  let open Lookup_status in
  List.fold_until lookups ~init:Good
    ~f:(fun acc_s lookup ->
      match Hashtbl.find map lookup with
      | Some (detail : Term_detail.t) -> (
          match (detail.status, acc_s) with
          | Good, _ -> Stop Good
          | Complete, _ -> Continue Complete
          | Fail, Complete -> Continue Complete
          | Fail, _ -> Continue Fail)
      | None -> Stop Good)
    ~finish:Fn.id

let pre_push_for_seq map lookups (td : Term_detail.t) part1_done part2_done
    (r : Lookup_result.t) =
  Fmt.pr "[pre_push] r:%a_%a@." Lookup_key.pp r.from Lookup_status.pp_short
    r.status ;
  if Lookup_status.is_complete_or_fail r.status
     && !part1_done && not !part2_done
  then (
    let status' = fold_lookups_status map lookups in
    Fmt.pr "[pre_push] .:%a@." Lookup_status.pp_short status' ;
    if Lookup_status.is_complete_or_fail status'
    then (
      part2_done := true ;
      td.status <- status') ;
    Some (Lookup_result.status_as r status'))
  else Some r

let set_status (td : Term_detail.t) status = td.status <- status

let promote_status (td : Term_detail.t) status' =
  let open Lookup_status in
  match (td.status, status') with
  | Good, Good -> Good
  | Good, _ ->
      td.status <- status' ;
      status'
  | Complete, _ -> failwith "[complete] why here"
  | Fail, _ -> failwith "[fail] why here"

let mark_and_id (td : Term_detail.t) (r : Lookup_result.t) =
  let status' = promote_status td r.status in
  set_status td status' ;
  Lookup_result.status_as r status'

let promote_result (td : Term_detail.t) (r : Lookup_result.t) =
  let status' = promote_status td r.status in
  set_status td status' ;
  Lookup_result.(status_as r status')

(* let mk_pre_push (td : Term_detail.t) (target : Lookup_key.t) (r : Lookup_result.t) = *)

let run run_task unroll (state : Global_state.t) (term_detail : Term_detail.t)
    rule_action =
  let open Rule_action in
  let { target; source } = rule_action in
  let add_phi = Global_state.add_phi state term_detail in
  let set_status = set_status term_detail in

  let rec run source =
    let add_then_loop source =
      Option.iter (Rule_action.pub_of source) ~f:(fun src ->
          term_detail.sub_lookups <- term_detail.sub_lookups @ [ src ]) ;
      run source
    in
    (match source with
    | Withered e -> set_status Lookup_status.Fail
    | Leaf e ->
        set_status Lookup_status.Complete ;
        U.by_return unroll target (Lookup_result.complete target)
    | Direct e ->
        U.by_map_u unroll target e.pub (mark_and_id term_detail) ;
        run_task e.pub
    | Map e ->
        U.by_map_u unroll target e.pub (fun r ->
            Fmt.pr "[Map]%a(%a) <- %a(%a) @." Lookup_key.pp target
              Lookup_status.pp_short term_detail.status Lookup_key.pp r.from
              Lookup_status.pp_short r.status ;
            mark_and_id term_detail (e.map r)) ;
        run_task e.pub
    | MapSeq e ->
        init_list_counter state term_detail target ;
        let f r =
          let i = fetch_list_counter state term_detail target in
          let r', phis = e.map i r in
          add_phi (Riddler.list_append target i (Riddler.and_ phis)) ;
          mark_and_id term_detail r'
        in
        U.by_map_u unroll target e.pub f ;
        run_task e.pub
    | Both e ->
        U.by_map2_u unroll target e.pub1 e.pub2 (fun (v1, v2) ->
            let joined_status = Lookup_status.join v1.status v2.status in

            Fmt.pr "[Both] %a <- %a <- %a(%a) %a(%a)@." Lookup_key.pp target
              Lookup_status.pp_short joined_status Lookup_key.pp v1.from
              Lookup_status.pp_short v1.status Lookup_key.pp v2.from
              Lookup_status.pp_short v2.status ;

            let status' = promote_status term_detail joined_status in
            Lookup_result.(from_as target status')) ;
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
          Fmt.pr "[Chain]%a@."
            (Observe.pp_key_with_detail state.term_detail_map)
            (target, term_detail) ;

          Fmt.pr "[Chain][p1:%B]<- %a(%a) @." !part1_done Lookup_key.pp r.from
            Lookup_status.pp_short r.status ;
          Lwt.return_unit
        in
        let part2_done = ref false in
        let pre_push (r : Lookup_result.t) =
          Fmt.pr "[Chain]%a@."
            (Observe.pp_key_with_detail state.term_detail_map)
            (target, term_detail) ;
          Fmt.pr "[Chain]<- %a(%a) @." Lookup_key.pp r.from
            Lookup_status.pp_short r.status ;

          pre_push_for_seq state.term_detail_map term_detail.sub_lookups
            term_detail part1_done part2_done r
        in
        U.by_bind_u unroll target e.pub part1_cb ;
        U.set_pre_push unroll target pre_push ;
        run_task e.pub
    | Sequence e ->
        init_list_counter state term_detail target ;
        let part1_done = ref false in
        let part1_cb _key (r : Lookup_result.t) =
          if Lookup_status.is_complete_or_fail r.status then part1_done := true ;

          Lookup_status.iter_ok r.status (fun () ->
              let i = fetch_list_counter state term_detail target in
              let next = e.next i r in
              match next with
              | Some (phi_i, edge) ->
                  let phi = Riddler.list_append target i phi_i in
                  add_phi phi ;
                  add_then_loop edge
              | None -> add_phi (Riddler.list_append target i Riddler.false_)) ;
          Lwt.return_unit
        in
        let part2_done = ref false in
        let pre_push (r : Lookup_result.t) =
          Fmt.pr "[Sequence]%a@."
            (Observe.pp_key_with_detail state.term_detail_map)
            (target, term_detail) ;

          Fmt.pr "[Sequence]<- %a(%a) @." Lookup_key.pp r.from
            Lookup_status.pp_short r.status ;

          pre_push_for_seq state.term_detail_map term_detail.sub_lookups
            term_detail part1_done part2_done r
        in
        U.by_bind_u unroll target e.pub part1_cb ;
        U.set_pre_push unroll target pre_push ;
        run_task e.pub
    | Or_list e ->
        if e.unbound then init_list_counter state term_detail target ;

        Fmt.pr "[Or_list][Create]%a(%a): %a@." Lookup_key.pp target
          Lookup_status.pp_short term_detail.status
          Fmt.Dump.(list @@ option Lookup_key.pp)
          (List.map e.elements ~f:pub_of) ;

        List.iter e.elements ~f:add_then_loop ;
        (* List.iter e.elements ~f:run ; *)
        let pre_push (r : Lookup_result.t) =
          Fmt.pr "[Or_list]%a@."
            (Observe.pp_key_with_detail state.term_detail_map)
            (target, term_detail) ;

          Fmt.pr "[Or_list]<- %a(%a) @." Lookup_key.pp r.from
            Lookup_status.pp_short r.status ;
          let status' =
            fold_lookups_status state.term_detail_map term_detail.sub_lookups
          in
          let status'' = promote_status term_detail status' in
          Some (Lookup_result.status_as r status')
        in
        U.set_pre_push unroll target pre_push) ;
    add_phi_edge state term_detail source
  in
  run source

(* let open Lookup_status in
   (match (term_detail.status, joined_status) with
   | Good, Good -> ()
   | Good, _ -> term_detail.status <- joined_status
   | _, _ ->
       (* () *)
       failwith "failed in both") ; *)
