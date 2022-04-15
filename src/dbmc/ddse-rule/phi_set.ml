open Core

type t = Set.M(Phi).t

let empty = Set.empty (module Phi)
let add s t = Set.add s t
let union s1 s2 = Set.union s1 s2
let compare s1 s2 = Set.compare_direct s1 s2
let to_list s = Set.to_list s

let print phis =
  Log.Export.SLog.app (fun m ->
      m "Phis: %a"
        Fmt.(Dump.list string)
        (List.map ~f:Z3.Expr.to_string (Set.to_list phis)))
