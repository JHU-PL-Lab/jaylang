open Core

module T = struct
  type t = Odefa_ast.Ast.ident = Ident of string
  [@@deriving sexp, compare, equal, hash]

  let hash = Hashtbl.hash
end

include T
include Comparator.Make (T)

let show (Ident s) = s

let pp oc (Ident s) = Fmt.pf oc "%s" s

let pp_list oc ids = Fmt.(pf oc "%a" (Dump.list pp) ids)

let cond_fid b = if b then Ident "$tt" else Ident "$ff"
