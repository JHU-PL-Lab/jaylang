open Core
open Dj_common

module type K = sig
  val k : int
end

module CS = struct
  module T = struct
    type t = (Id.t * Id.t) list
    [@@deriving sexp, compare, equal, show { with_path = false }, hash]
  end

  include T
  include Comparator.Make (T)
end

module Make (K : K) = struct
  let k = K.k

  include CS

  let empty : t = []
  let to_string v = v |> sexp_of_t |> Sexp.to_string_mach
  let of_string s = s |> Sexp.of_string |> t_of_sexp
  let to_list t = t
  let of_list t = t

  let push frame stk =
    let r = frame :: stk in
    if List.length r <= k then r else List.drop_last_exn r
end

module CS_0 = Make (struct
  let k = 0
end)

module CS_1 = Make (struct
  let k = 1
end)

module CS_2 = Make (struct
  let k = 2
end)

module CS_3 = Make (struct
  let k = 3
end)

module CS_4 = Make (struct
  let k = 4
end)
