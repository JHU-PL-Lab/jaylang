open Core

module T = struct
  (* The first is for callsite (position) and the second is for function (choice)
     It can also applies on condition where the first is callsite and the second is Id.tt or Id.ff
  *)
  type t =
    { l : (Id.t * Id.t) list
    ; n : int [@sexp.ignore][@show.ignore][@hash.ignore] }
  [@@deriving sexp, compare, equal, show { with_path = false }, hash]
end

include T
include Comparator.Make (T)

let empty : t = { l = [] ; n = 0 }
let to_string v = v |> sexp_of_t |> Sexp.to_string_mach
let of_string s = s |> Sexp.of_string |> t_of_sexp
let to_list t = t.l
let of_list l = { l ; n = List.length l }
let push frame stk = { l = frame :: stk.l ; n = stk.n + 1 }
let equal_flip s1 s2 = equal ({ s1 with l = List.rev s1.l}) s2
let length t = t.n