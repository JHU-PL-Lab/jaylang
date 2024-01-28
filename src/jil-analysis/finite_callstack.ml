open Core
open Dj_common

module type K = sig
  val k : int
end

module CS = struct
  module T = struct
    type t = (Id.t * Id.t) list [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Comparator.Make (T)
end

module type C = sig
  include module type of struct
    include CS
  end
  (* include module type of CS.T
     include Comparator.S *)

  val k : int
  val empty : t
  val to_string : t -> string
  val of_string : string -> t
  val of_list : (Id.t * Id.t) list -> t
  val to_list : t -> (Id.t * Id.t) list
  val push : Id.t * Id.t -> t -> t
  val pp : t Fmt.t
end

module Make (K : K) : C = struct
  include CS

  let k = K.k
  let empty : t = []
  let to_string v = v |> sexp_of_t |> Sexp.to_string_mach
  let of_string s = s |> Sexp.of_string |> t_of_sexp
  let pp = Fmt.of_to_string to_string
  let to_list t = t
  let of_list t = t

  let push frame stk =
    let r = frame :: stk in
    if List.length r <= k then r else List.drop_last_exn r
end

module CS_0 = Make (struct
  let k = 0
end)
(*
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
   end) *)
