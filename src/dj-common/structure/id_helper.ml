open Core

let core_map_of_id_map :
      'a 'b. f:('a -> 'b) -> 'a Jayil.Ast.Ident_map.t -> 'b Map.M(Id).t =
 fun ~f map ->
  map |> Jayil.Ast.Ident_map.enum |> Jayil.Ast.bat_list_of_enum
  |> List.map ~f:(fun (k, v) -> (k, f v))
  |> Map.of_alist_exn (module Id)

let core_set_of_id_set ~f set =
  set |> Jayil.Ast.Ident_set.enum |> Jayil.Ast.bat_list_of_enum
  |> List.map ~f:(fun k -> f k)
  |> Set.of_list (module Id)
