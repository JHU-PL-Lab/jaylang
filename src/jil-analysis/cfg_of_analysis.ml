open Core

(* TODO: `make dtest` reports
   > Hashtbl: mutation not allowed during iteration *)

let to_cfg e =
  let _ar = Main.analyze e in
  let map = ref (Dj_common.Cfg_of_source.block_map_of_expr e) in
  Main.visited
  |> Hash_set.iter ~f:(fun key ->
         let _, _, _, e = key in
         let cl = Abs_exp.clause_of_e_exn e in
         let rv = Main.solution key in
         ()) ;
  !map
