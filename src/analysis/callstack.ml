
open Core

module T = struct
  type t = Lang.Ast.Callsight.t list
    [@@deriving compare, sexp]
end

include T

module Map = Map.Make (T)

