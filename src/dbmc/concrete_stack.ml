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
