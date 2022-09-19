open Core
open Dj_common
module U = Unrolls.U_dbmc

let add_phi (state : Global_state.t) (term_detail : Term_detail.t) phi =
  term_detail.phis <- phi :: term_detail.phis ;
  state.phis <- phi :: state.phis

let init_list_counter (state : Global_state.t) (term_detail : Term_detail.t) key
    =
  Hashtbl.update state.smt_lists key ~f:(function
    | Some i -> i
    | None ->
        add_phi state term_detail (Riddler.list_head key) ;
        0)

let fetch_list_counter (state : Global_state.t) (term_detail : Term_detail.t)
    key =
  let new_i =
    Hashtbl.update_and_return state.smt_lists key ~f:(function
      | Some i -> i + 1
      | None ->
          add_phi state term_detail (Riddler.list_head key) ;
          1)
  in
  new_i - 1

let add_phi_edge state term_detail edge =
  let open Rule_action in
  let phis =
    match edge with
    | Withered e -> e.phis
    | Leaf e -> e.phis
    | Direct e -> e.phis
    | Map e -> e.phis
    | MapSeq e -> e.phis
    | Both e -> e.phis
    | Chain e -> e.phis
    | Sequence e -> e.phis
    | Or_list e -> e.phis
  in
  List.iter phis ~f:(add_phi state term_detail)

let rec run run_task unroll (state : Global_state.t)
    (term_detail : Term_detail.t) rule_action =
  let loop rule_action = run run_task unroll state term_detail @@ rule_action in
  let add_phi = add_phi state in
  let run_task' key block_id =
    Fmt.pr "[DEBUG] %a @@ %a" Lookup_key.pp key Id.pp block_id ;
    run_task key (Jayil.Ast.Ident_map.find block_id state.block_map) ;
    Fmt.pr "[DEBUG] safe"
  in
  let open Rule_action in
  add_phi_edge state term_detail rule_action ;
  match (rule_action : Rule_action.t) with
  | Withered e -> ()
  | Leaf e -> U.by_return unroll e.sub (Lookup_result.ok e.sub)
  | Direct e ->
      let pub, block = e.pub in
      run_task pub block ;
      U.by_id_u unroll e.sub pub
  | Map e ->
      let pub, block = e.pub in
      run_task pub block ;
      U.by_map_u unroll e.sub pub e.map
  | MapSeq e ->
      let pub, block = e.pub in
      init_list_counter state term_detail e.sub ;
      run_task pub block ;
      let f r =
        let i = fetch_list_counter state term_detail e.sub in
        let ans, phis = e.map i r in
        add_phi term_detail (Riddler.list_append e.sub i (Riddler.and_ phis)) ;
        ans
      in
      U.by_map_u unroll e.sub pub f
  | Both e ->
      let pub2, block = e.pub2 in
      run_task e.pub1 e.pub1.block ;
      run_task pub2 block ;
      U.by_map2_u unroll e.sub e.pub1 pub2 (fun _ -> Lookup_result.ok e.sub)
  | Chain e ->
      let pub, block = e.pub in
      let cb key r =
        let edge = e.next key r in
        (match edge with Some edge -> loop edge | None -> ()) ;
        Lwt.return_unit
      in
      U.by_bind_u unroll e.sub pub cb ;
      run_task pub block
  | Sequence e ->
      let pub, block = e.pub in
      init_list_counter state term_detail e.sub ;

      run_task pub block ;
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
      U.by_bind_u unroll e.sub pub cb
  | Or_list e ->
      if e.unbound then init_list_counter state term_detail e.sub else () ;
      List.iter e.nexts ~f:(fun e -> loop e)