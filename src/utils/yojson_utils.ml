(**
   This module contains utility functions for writing Yojson codec generators.
*)

open Batteries;;
open Yojson.Safe;;

let set_to_yojson element_to_yojson enumerator set =
  `List
    (
      set
      |> enumerator
      |> Enum.map element_to_yojson
      |> List.of_enum
    )
;;

let map_to_yojson key_to_yojson value_to_yojson enumerator map =
  `List
    (
      map
      |> enumerator
      |> Enum.map
        (fun (k,v) -> `List [key_to_yojson k; value_to_yojson v])
      |> List.of_enum
    )
;;

module type To_yojson_type =
sig
  type t
  val to_yojson : t -> json
end;;

module Set_to_yojson(S : Set.S)(Y : To_yojson_type with type t = S.elt) =
struct
  let to_yojson = set_to_yojson Y.to_yojson S.enum;;
end;;

module Map_to_yojson(M : Map.S)(Y : To_yojson_type with type t = M.key) =
struct
  let to_yojson value_to_yojson =
    map_to_yojson Y.to_yojson value_to_yojson M.enum
  ;;
end;;
