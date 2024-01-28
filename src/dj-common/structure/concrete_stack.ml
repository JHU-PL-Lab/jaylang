open Core

module T = struct
  (* The first is for callsite (position) and the second is for function (choice)
     It can also applies on condition where the first is callsite and the second is Id.tt or Id.ff
  *)
  type t = (Id.t * Id.t) list
  [@@deriving sexp, compare, equal, show { with_path = false }, hash]
end

include T
include Comparator.Make (T)

let empty : t = []
let to_string v = v |> sexp_of_t |> Sexp.to_string_mach
let of_string s = s |> Sexp.of_string |> t_of_sexp
let to_list t = t
let of_list t = t
let push frame stk = frame :: stk
let equal_flip s1 s2 = equal (List.rev s1) s2
