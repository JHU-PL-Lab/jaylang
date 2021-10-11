open Core
open Fix

module Expr = struct
  module T = struct
    type t = Int of int | Binary of t * t | Dummy
    [@@deriving sexp, compare, equal, hash, show { with_path = false }]
  end

  include T
  include Comparator.Make (T)
end

module ExprMap : IMPERATIVE_MAPS with type key = Expr.t = struct
  type key = Expr.t

  type 'data t = (key, 'data) Hashtbl.t

  let create () = Hashtbl.create (module Expr)

  let add key data map = Hashtbl.add_exn map ~key ~data

  let find key map =
    match Hashtbl.find map key with Some v -> v | None -> raise Caml.Not_found

  let clear map = Hashtbl.clear map

  let iter f map = Hashtbl.iteri map ~f:(fun ~key ~data -> f key data)
end

module IntP = Prop.Set (struct
  type t = int

  let empty = 0

  let equal s1 s2 = s1 = s2
end)

module F = Fix.Make (ExprMap) (IntP)

let sum expr = F.lfp (fun _expr _request -> 10) expr

open Expr

let () =
  let answer = sum Dummy in
  print_endline @@ string_of_int answer
