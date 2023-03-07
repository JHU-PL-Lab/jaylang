open Core
open Dj_common
module U = Unrolls.U_dbmc
module Log = Log.Export.CMLOG

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

let set_status (td : Term_detail.t) status = td.status <- status

let promote_status (td : Term_detail.t) status' =
  let open Lookup_status in
  match (td.status, status') with
  | Good, Good -> Some Good
  | Good, _ -> Some status'
  | Complete, _ -> None (* failwith "[complete] why here" *)
  | Fail, _ -> None (* failwith "[fail] why here" *)

let promote_result (target : Lookup_key.t) map (td : Term_detail.t)
    (r : Lookup_result.t) status' =
  (* Fmt.pr "[Push]%a @." (Observe.pp_key_with_detail map) (target, td) ;
        Fmt.pr "[Push] <- %a;%a(%a) @." Lookup_key.pp r.from Lookup_status.pp_short
     status' Lookup_status.pp_short r.status ; *)
  match promote_status td status' with
  | Some Complete ->
      set_status td Complete ;
      Some Lookup_result.(status_as r Complete)
  | Some status'' ->
      set_status td status'' ;
      Some Lookup_result.(status_as r status'')
  | None ->
      (* Fmt.pr "[Push] %a =/=> @." Lookup_status.pp_short td.status ; *)
      None

let register run_task unroll (state : Global_state.t)
    (term_detail : Term_detail.t) target source =
  let open Rule_action in
  let add_phi = Global_state.add_phi state term_detail in
  let set_status = set_status term_detail in
  let add_sub_preconds cond =
    term_detail.sub_preconds <- term_detail.sub_preconds @ [ cond ]
  in
  let add_sublookup key =
    term_detail.sub_lookups <- term_detail.sub_lookups @ [ key ]
  in
  let promote_result = promote_result target state.term_detail_map in
  let rec run ?(sub_lookup = false) source =
    match source with
    | Withered -> set_status Lookup_status.Fail
    | Leaf ->
        set_status Lookup_status.Complete ;
        U.by_return unroll target (Lookup_result.complete target)
    | Direct e ->
        if sub_lookup
        then (
          add_sublookup e.pub ;
          U.by_id_u unroll target e.pub)
        else
          U.by_filter_map_u unroll target e.pub (fun r ->
              promote_result term_detail r r.status) ;
        Log.debug (fun m ->
            m "[Direct]%a <- %a(%B) @." Lookup_key.pp target Lookup_key.pp e.pub
              sub_lookup) ;
        run_task e.pub
    | Map e ->
        U.by_filter_map_u unroll target e.pub (fun r ->
            promote_result term_detail (e.map r) r.status) ;
        run_task e.pub
    | MapSeq e ->
        init_list_counter state term_detail target ;
        let f r =
          let i = fetch_list_counter state term_detail target in
          let r', phis = e.map i r in
          add_phi (Riddler.list_append target i (Riddler.and_ phis)) ;
          promote_result term_detail r' r.status
        in
        U.by_filter_map_u unroll target e.pub f ;
        run_task e.pub
    | Both e ->
        U.by_filter_map2_u unroll target e.pub1 e.pub2 (fun (v1, v2) ->
            let joined_status = Lookup_status.join v1.status v2.status in
            promote_result term_detail (Lookup_result.ok target) joined_status) ;
        run_task e.pub1 ;
        run_task e.pub2
    | Chain e ->
        let precond = ref false in
        add_sub_preconds precond ;
        let part1_cb key (r : Lookup_result.t) =
          if Lookup_status.is_complete_or_fail r.status then precond := true ;
          Lookup_status.iter_ok r.status (fun () ->
              match e.next key r with
              | Some edge -> run ~sub_lookup:true edge
              | None -> ()) ;
          (* Fmt.pr "[Chain][P1]%a <- %a(%a) @." Lookup_key.pp target Lookup_key.pp
               r.from Lookup_status.pp_short r.status ;
             Fmt.pr "[Chain][P1](%B)%a @." !precond
               (Observe.pp_key_with_detail state.term_detail_map)
               (target, term_detail) ; *)
          Lwt.return_unit
        in
        U.by_bind_u unroll target e.pub part1_cb ;
        run_task e.pub
    | Sequence e ->
        init_list_counter state term_detail target ;
        let precond = ref false in
        add_sub_preconds precond ;
        let part1_cb _key (r : Lookup_result.t) =
          if Lookup_status.is_complete_or_fail r.status then precond := true ;
          Lookup_status.iter_ok r.status (fun () ->
              let i = fetch_list_counter state term_detail target in
              let next = e.next i r in
              match next with
              | Some (phi_i, edge) ->
                  add_phi @@ Riddler.list_append target i phi_i ;
                  run ~sub_lookup:true edge
              | None -> add_phi (Riddler.list_append target i Riddler.false_)) ;
          Lwt.return_unit
        in
        U.by_bind_u unroll target e.pub part1_cb ;
        run_task e.pub
    | Or_list e ->
        if e.unbound then init_list_counter state term_detail target ;
        List.iter e.elements ~f:(run ~sub_lookup)
  in

  run source ;
  let need_pre_push =
    (not (List.is_empty term_detail.sub_preconds))
    || not (List.is_empty term_detail.sub_lookups)
  in
  if need_pre_push
  then (
    let pre_push (r : Lookup_result.t) =
      (* Fmt.pr "[PrePush] %a <- %a;%a@." Lookup_key.pp target Lookup_key.pp r.from
         Lookup_status.pp_short r.status ; *)
      if List.for_all term_detail.sub_preconds ~f:Ref.( ! )
      then
        let status' =
          fold_lookups_status state.term_detail_map term_detail.sub_lookups
        in
        (* Fmt.pr "[PrePush] <=%a@." Lookup_status.pp_short status' ; *)
        promote_result term_detail r status'
      else Some r
    in
    U.set_pre_push unroll target pre_push ;
    Log.debug (fun m ->
        m "[Reg-%B] %a %d@." need_pre_push Lookup_key.pp target
          (List.length term_detail.sub_preconds)))
