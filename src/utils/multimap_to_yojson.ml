open Batteries;;

open Multimap;;
open Yojson_utils;;

module Make
    (M : Multimap_sig)
    (K_yojson : To_yojson_type with type t = M.key)
    (V_yojson : To_yojson_type with type t = M.value)
=
struct
  let to_yojson m =
    `List
      (
        M.enum_by_key m
        |> Enum.map
          (fun (k,v) ->
             `List [ K_yojson.to_yojson k
                   ; `List ( M.S.enum v
                             |> Enum.map V_yojson.to_yojson
                             |> List.of_enum
                           )
                   ]
          )
        |> List.of_enum
      )
  ;;
end;;
