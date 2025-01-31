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

(* let typing s =
   let id = Parse.longident (Lexing.from_string "x") in
   s |> Lexing.from_string |> Parse.implementation
   |> Typemod.type_structure !Toploop.toplevel_env
   |> (fun (a, b, _, _, _) -> b)
   |> Printtyp.tree_of_type_declaration id *)
(* |> Printtyped.interface Format.std_formatter *)
