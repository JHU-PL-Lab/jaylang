open Core

(* let core_map_of_id_map :
        'a 'b. ?f:('a -> 'b) -> 'a Jayil.Ast.Ident_map.t -> 'b Map.M(Id).t =
   fun ?(f = Fn.id) map ->
    map |> Jayil.Ast.Ident_map.enum |> Jayil.Ast.bat_list_of_enum
    |> List.map ~f:(fun (k, v) -> (k, f v))
    |> Map.of_alist_exn (module Id) *)

let core_map_of_id_map :
      'a 'b. f:('a -> 'b) -> 'a Jayil.Ast.Ident_map.t -> 'b Map.M(Id).t =
 fun ~f map ->
  map |> Jayil.Ast.Ident_map.enum |> Jayil.Ast.bat_list_of_enum
  |> List.map ~f:(fun (k, v) -> (k, f v))
  |> Map.of_alist_exn (module Id)

(* let id_but_poly : 'a 'b. 'a -> 'b = fun (x : 'a) : 'b -> x *)

let core_set_of_id_set ~f set =
  set |> Jayil.Ast.Ident_set.enum |> Jayil.Ast.bat_list_of_enum
  |> List.map ~f:(fun k -> f k)
  |> Set.of_list (module Id)
(*
   let example1 map : Id.t Map.M(Id).t = core_map_of_id_map ~f:Fn.id map

   let example2 map : (Id.t * Id.t) Map.M(Id).t =
     core_map_of_id_map ~f:(fun x -> (x, x)) map

   let my_map : 'a 'b. f:('a -> 'b) -> 'a -> 'b = fun ~f x -> f x
   let (_ : int) = my_map ~f:(fun x -> x) 3
   let (_ : int * int) = my_map ~f:(fun x -> (x, x)) 3

   let my_map' : 'a 'b. ?f:('a -> 'b) -> 'a -> 'b = fun ?(f = fun x -> x) x -> f x *)

(* let my_map : 'a 'b. f:('a -> 'b) -> 'a -> 'b = fun ~f x -> f x
   let (_ : int) = my_map ~f:(fun x -> x) 3
   let (_ : int * int) = my_map ~f:(fun x -> (x, x)) 3
   let my_map' x = my_map ~f:Fn.id x
   let (_ : int) = my_map' 3
   let (_ : int * int) = my_map ~f:(fun x -> (x, x)) 3 *)
