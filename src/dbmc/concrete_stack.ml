open Core

module T = struct
  type t = (Id.t * Id.t) list
  [@@deriving sexp, compare, equal, show { with_path = false }, hash]
end

include T
include Comparator.Make (T)

let empty : t = []

let to_string v = v |> sexp_of_t |> Sexp.to_string_mach

let of_string s = s |> Sexp.of_string |> t_of_sexp

let of_ast_id stk =
  List.map stk ~f:(fun (cs, fid) -> (Id.of_ast_id cs, Id.of_ast_id fid))

let to_ast_id stk =
  List.map stk ~f:(fun (cs, fid) -> (Id.to_ast_id cs, Id.to_ast_id fid))
