open Core

(* TODO: `make dtest` reports
   > Hashtbl: mutation not allowed during iteration *)

let to_cfg e =
  let solution, visited, _ar = Main.analyze e in
  let block_map = ref (Dj_common.Cfg_of_source.block_map_of_expr e) in
  let result_map = Main.build_result_map solution visited in
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
  let block_mapper x vs block =
    let open Dj_common.Cfg in
    let clauses_mapper cls =
      List.map cls ~f:(fun cl -> { cl with clause = cl.clause })
    in
    match block.kind with
    | Main -> { block with clauses = clauses_mapper block.clauses }
    | Fun fb -> { block with clauses = clauses_mapper block.clauses }
    | Cond cb -> { block with clauses = clauses_mapper block.clauses }
  in

  Hashtbl.iteri result_map ~f:(fun ~key:x ~data:vs ->
      let block_map' = Jayil.Ast.Ident_map.map (block_mapper x vs) !block_map in
      block_map := block_map') ;
  !block_map
