(**
   This module contains utility functions for writing Yojson codec generators.
*)

open Batteries;;
open Yojson.Safe;;

val set_to_yojson : ('t -> json) -> ('s -> 't Enum.t) -> 's -> json

val map_to_yojson :
  ('k -> json) -> ('v -> json) -> ('m -> ('k * 'v) Enum.t) -> 'm -> json
