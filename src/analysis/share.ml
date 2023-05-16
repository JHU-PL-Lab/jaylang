open Core
open Fix

module Id = struct
  module T = struct
    type t = Jayil.Ast.ident = Ident of string
    [@@deriving sexp, compare, equal, hash]

    let hash = Hashtbl.hash
    let show (Ident s) = s
    let pp oc (Ident s) = Fmt.pf oc "%s" s
    let pp_list oc ids = Fmt.(pf oc "%a" (Dump.list pp) ids)
  end

  include T
  include Comparator.Make (T)
end

module IdMap : IMPERATIVE_MAPS with type key = Id.t = struct
  type key = Id.t
  type 'data t = (key, 'data) Hashtbl.t

  let create () = Hashtbl.create (module Id)
  let add key data map = Hashtbl.add_exn map ~key ~data
  let find key map = Hashtbl.find_exn map key
  let clear map = Hashtbl.clear map
  let iter f map = Hashtbl.iteri map ~f:(fun ~key ~data -> f key data)
end

module ExprMap : IMPERATIVE_MAPS with type key = Jayil.Ast.expr = struct
  type key = Jayil.Ast.expr
  type 'data t = (key, 'data) Hashtbl.Poly.t

  let create () = Hashtbl.Poly.create ()
  let add key data map = Hashtbl.Poly.add_exn map ~key ~data

  let find key map =
    match Hashtbl.Poly.find map key with
    | Some v -> v
    | None -> raise Caml.Not_found

  let clear map = Hashtbl.Poly.clear map
  let iter f map = Hashtbl.Poly.iteri map ~f:(fun ~key ~data -> f key data)
end

module ClauseMap : IMPERATIVE_MAPS with type key = Jayil.Ast.clause = struct
  type key = Jayil.Ast.clause
  type 'data t = (key, 'data) Hashtbl.Poly.t

  let create () = Hashtbl.Poly.create ()
  let add key data map = Hashtbl.Poly.add_exn map ~key ~data

  let find key map =
    match Hashtbl.Poly.find map key with
    | Some v -> v
    | None -> raise Caml.Not_found

  let clear map = Hashtbl.Poly.clear map
  let iter f map = Hashtbl.Poly.iteri map ~f:(fun ~key ~data -> f key data)
end

module IdSet = Prop.Set (struct
  type t = Set.M(Id).t

  let empty = Set.empty (module Id)
  let equal s1 s2 = Set.equal s1 s2
end)

let first_id program =
  let open Jayil.Ast in
  let (Expr clauses) = program in
  let (Clause (Var (x, _), _)) = List.hd_exn clauses in
  x

let last_id program =
  let open Jayil.Ast in
  let (Expr clauses) = program in
  let (Clause (Var (x, _), _)) = List.last_exn clauses in
  x

let load filename = Dj_common.File_utils.read_source filename
