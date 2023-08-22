open Core

(* TODO: `make dtest` reports
   > Hashtbl: mutation not allowed during iteration *)

let block_map_of_expr e =
  let solution, visited, _ar = Main.analyze e in
  let block_map = ref (Dj_common.Cfg_of_source.block_map_of_expr e) in
  (* TODO: result map is better to be clause to map *)
  let result_map = Main.build_result_map solution visited in
  let para_to_fun_def_map = Jayil.Ast_tools.make_para_to_fun_def_mapping e in
  let id_to_clause_map = Jayil.Ast_tools.clause_mapping e in

  (* We need to mark all the result_map information in block_map.
     Given current implementation (or even for general goodness), we cannot mutate one map during its iteration. (I may have done this) More specifically, I will iterate along the keys but mutate its values. It may work, however, not in the current _functional_ data structure.
  *)
  (* `block_map` is a bad-designed block-index functional map. The workflow to update is:
     1. find the matching block
     2. functionally update the block to get the block'
     3. save the block' to get a new map'
     4. store the map' to the reference of the old map

     For this scenario, one block can have multiple matching clauses e.g.
     when we have {f -> {v}}, we can heve clauses `r1 = f a1; r2 = f a2`
  *)
  let update_clauses block =
    let open Dj_common.Cfg in
    let clauses_mapper cls =
      List.map cls ~f:(fun cl ->
          let cat' =
            match (cl.cat, cl.clause) with
            | Direct, Clause (_, Value_body (Value_function _)) -> Direct
            | Fun, _ -> Fun
            | Cond, _ -> Cond
            | App [], Clause (_, Var_body (Var (f, _))) ->
                let dsts =
                  let vs = Hashtbl.find_exn result_map f in
                  vs |> Set.to_list
                  |> List.filter_map ~f:(function
                       | AInt -> None
                       | ABool _ -> None
                       | Any -> None
                       | AClosure (x, _, _) ->
                           Some (Jayil.Ast.Ident_map.find x para_to_fun_def_map))
                in
                App dsts
            | _, _ -> failwith "impossible pair"
          in
          { cl with cat = cat' })
    in
    match block.kind with
    | Main -> { block with clauses = clauses_mapper block.clauses }
    | Fun fb -> { block with clauses = clauses_mapper block.clauses }
    | Cond cb -> { block with clauses = clauses_mapper block.clauses }
  in

  (* update all clauses in each blocks *)
  block_map := Jayil.Ast.Ident_map.map update_clauses !block_map ;

  (* update fun and cond block

     iterate all key, result_value pairs.
     for each key, find its clauses and case the clause
      1. application clause `x = f _`. Iterate the possible closures results for f. If we are in the f's closure (function) body, we may come from `x`. The fid of _closure (function) body_ is retrieved from `para_to_fun_def_map`, the block associated with fid is retrieved from `block_map`, then we update the block by adding a new possible callsite `x`.
      2. condition clause `x = c ? e1 : e2`. The design is `x` points to a `cond_both` block, and `e1` and `e2` points to a `cond_case` block. A `cond_both` blocks contains two optional `cond_case` blocks. A `cond_case` block has a `possible` field. `cfg_of_course` creats these three blocks, with optional values not-none and `possible` to be `true`. Then the analysis updates the `possible` to be false and the optional block to be `none` if unreachable.
  *)
  Hashtbl.iteri result_map ~f:(fun ~key:x ~data:vs ->
      let open Dj_common.Cfg in
      let open Jayil.Ast in
      let cl = Ident_map.find x id_to_clause_map in
      match cl with
      | Clause (Var (xc, _), Appl_body (Var (f, _), Var _)) ->
          Hashtbl.find_exn result_map f
          |> Set.to_list
          |> List.iter ~f:(function
               | AInt -> ()
               | ABool _ -> ()
               | Any -> ()
               | AClosure (x, _, _) ->
                   let fid = Jayil.Ast.Ident_map.find x para_to_fun_def_map in
                   let block_map' =
                     Jayil.Ast.Ident_map.update_stdlib fid
                       (function
                         | Some block ->
                             let fblock = cast_to_fun_block_info block in
                             let fblock' =
                               {
                                 fblock with
                                 callsites = xc :: fblock.callsites;
                               }
                             in
                             Some { block with kind = Fun fblock' }
                         | None -> failwith "must have this fblock")
                       !block_map
                   in
                   block_map := block_map')
      | Clause (Var (xc, _), Conditional_body (Var (c, _), _, _)) ->
          let vs = Hashtbl.find_exn result_map c in
          let cond_both = find_cond_blocks xc !block_map in
          let may_be_true =
            Set.exists ~f:(function ABool true -> true | _ -> false) vs
          in
          let may_be_false =
            Set.exists ~f:(function ABool false -> true | _ -> false) vs
          in
          let set_impossible beta =
            let beta_block =
              if beta
              then Option.value_exn cond_both.else_
              else Option.value_exn cond_both.then_
            in
            set_block_impossible block_map beta_block
          in
          if not may_be_true then set_impossible true ;
          if not may_be_false then set_impossible false ;
          ()
      | _ -> ()) ;

  (* block_map := Jayil.Ast.Ident_map.map update_block !block_map ; *)
  !block_map
