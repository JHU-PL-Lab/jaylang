(**
   This module contains utility functions for writing Yojson codec generators.
*)

open Batteries;;

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
