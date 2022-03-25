(* let input_from_model =
          let input_collected = Global_state.collect_picked_input state model in
          input_collected
        in
        let input_from_model_ordered =
          List.sort input_from_model ~compare:(fun (k1, _v1) (k2, _v2) ->
              -Lookup_key.chrono_compare info.block_map k1 k2)
        in
       Logs.app (fun m ->
               m "M: %a\nI: %a\n"
                 Fmt.Dump.(
                   list
                     (Fmt.pair ~sep:(Fmt.any ", ") Lookup_key.pp_id (option Fmt.int)))
                 input_from_model_ordered
                 Fmt.Dump.(list (option Fmt.int))
                 inputs_from_interpreter);
           assert (
             List.length input_from_model_ordered
             = List.count inputs_from_interpreter ~f:Option.is_some);
           assert (
             List.equal [%equal: int option]
               (List.map input_from_model_ordered ~f:snd)
               (List.filter inputs_from_interpreter ~f:Option.is_some)); *)

(* Lookup_key.ml *)

(*
and deal_with_value mv key block (gate_tree : Node.t ref) result_pusher =
   let singleton_lookup = List.is_empty key.xs in

   (* Discovery Main & Non-Main *)
   if singleton_lookup
   then (
     if Ident.equal (id_of_block block) id_main
     then (
       (* Discovery Main *)
       let target_stk = Rstack.concretize_top key.r_stk in
       Node.update_rule gate_tree (Node.done_ target_stk);
       add_phi key (Riddler.discover_main key mv);
       Lookup_result.ok_lwt key.x)
     else
       (* Discovery Non-Main *)
       let key_first = Lookup_key.to_first key x_first in
       let node_child, lookup_first =
         find_or_add_task key_first block gate_tree
       in
       gate_tree := { !gate_tree with rule = Node.to_first node_child };
       add_phi key (Riddler.discover_non_main key x_first mv);
       let%lwt _ =
         Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_first
       in
       Lookup_result.ok_lwt key.x)
   else
     (* Discard *)
     match mv with
     | Some (Value_function _f) ->
         let key_drop_x = Lookup_key.drop_x key in
         let node_sub, lookup_sub =
           find_or_add_task key_drop_x block gate_tree
         in
         Node.update_rule gate_tree (Node.discard node_sub);
         add_phi key (Riddler.discard key mv);
         let%lwt _ =
           Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_sub
         in
         Lookup_result.ok_lwt key.x
     (* Record End *)
     | Some (Value_record r) -> (
         Logs.info (fun m -> m "Rule Record: %a" Ast_pp.pp_record_value r);
         let (Record_value rmap) = r in
         let _x, xs, r_stk = Lookup_key.to_parts key in
         let labal, xs' = (List.hd_exn xs, List.tl_exn xs) in
         match Ident_map.Exceptionless.find labal rmap with
         | Some (Var (vid, _)) ->
             let key' = Lookup_key.of_parts2 (vid :: xs') r_stk in
             let node_key, lookup_key =
               find_or_add_task key' block gate_tree
             in
             Node.update_rule gate_tree (Node.alias node_key);
             add_phi key (Riddler.alias_key key key');
             let%lwt _ =
               Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_key
             in
             Lookup_result.ok_lwt key.x
         | None ->
             Node.update_rule gate_tree Node.mismatch;
             add_phi key (Riddler.mismatch key);
             Lookup_result.fail_lwt key.x)
     | _ ->
         Node.update_rule gate_tree Node.mismatch;
         add_phi key (Riddler.mismatch key);
         Lookup_result.fail_lwt key.x
*)
