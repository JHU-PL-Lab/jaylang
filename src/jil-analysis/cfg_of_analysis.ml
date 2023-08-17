open Core

(* TODO: `make dtest` reports
   > Hashtbl: mutation not allowed during iteration *)

let to_cfg e =
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

  (* let update_block block =
       let open Dj_common.Cfg in
       match block.kind with
       | Main -> block
       | Fun fb ->
           (* ask all Appl clauses to see if they call *)
           let callsites =
             []
             (* Hashtbl.fold result_map ~init:[] ~f:(fun ~key:x ~data:vs acc ->
                if Set.exists vs ~f:(fun v -> block.id) then x::acc else
                acc) *)
           in
           { block with kind = Fun { fb with callsites } }
       | Cond cb -> { block with kind = Cond cb }
     in *)

  (* update all clauses in each blocks *)
  block_map := Jayil.Ast.Ident_map.map update_clauses !block_map ;
  (* update fun and cond block *)
  Hashtbl.iteri result_map ~f:(fun ~key:x ~data:vs ->
      let open Jayil.Ast in
      let cl = Ident_map.find x id_to_clause_map in
      match cl with Clause (_, Appl_body (Var (f, _), Var _)) -> () | _ -> ()) ;

  (* block_map := Jayil.Ast.Ident_map.map update_block !block_map ; *)
  !block_map
