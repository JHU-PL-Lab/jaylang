open Core
open Odefa_ast
open Odefa_ast.Ast

(*
   module type X_int = sig val x : int end;;

   utop # let rec mm = (module struct let x = y end : X_int) and y = 4;;
   val mm : (module X_int) = <module>
*)

module type Ruler_state = sig
  val state : Global_state.t
  val add_phi : Lookup_key.t -> Z3.Expr.expr -> unit
  val x_first : Id.t
end

module Make (S : Ruler_state) = struct
  let deal_with_value mv (key : Lookup_key.t) block (gate_tree : Node.t ref)
      result_pusher find_or_add_task =
    let singleton_lookup = List.is_empty key.xs in

    (* Discovery Main & Non-Main *)
    if singleton_lookup
    then (
      if Ident.equal (Tracelet.id_of_block block) Tracelet.id_main
      then (
        (* Discovery Main *)
        let target_stk = Rstack.concretize_top key.r_stk in
        Node.update_rule gate_tree (Node.done_ target_stk);
        S.add_phi key (Riddler.discover_main key mv);
        result_pusher (Lookup_result.ok key.x);
        Lookup_result.ok_lwt key.x)
      else
        (* Discovery Non-Main *)
        let key_first = Lookup_key.to_first key S.x_first in
        let node_child, lookup_first =
          find_or_add_task key_first block gate_tree
        in
        gate_tree := { !gate_tree with rule = Node.to_first node_child };
        S.add_phi key (Riddler.discover_non_main key S.x_first mv);
        (* result_pusher (Some (Lookup_result.ok key.x)) *)
        let%lwt _ =
          (* Lwt_stream.iter (fun x -> result_pusher (Some x)) lookup_first *)
          Lwt_stream.iter
            (fun _ -> result_pusher (Lookup_result.ok key.x))
            lookup_first
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
          S.add_phi key (Riddler.discard key mv);
          let%lwt _ = Lwt_stream.iter (fun x -> result_pusher x) lookup_sub in
          Lookup_result.ok_lwt key.x
      (* Record End *)
      | Some (Value_record r) -> (
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
              S.add_phi key (Riddler.alias_key key key');
              let%lwt _ =
                Lwt_stream.iter (fun x -> result_pusher x) lookup_key
              in
              Lookup_result.ok_lwt key.x
          | None ->
              Node.update_rule gate_tree Node.mismatch;
              S.add_phi key (Riddler.mismatch key);
              Lookup_result.fail_lwt key.x)
      | _ ->
          Node.update_rule gate_tree Node.mismatch;
          S.add_phi key (Riddler.mismatch key);
          Lookup_result.fail_lwt key.x
end

(* let module RS = (val (module struct
   let state = state
   end) : Ruler.Ruler_state)
   in *)

(* ruler = ( (Ruler.Make RS) : Ruler.R)
   and *)
