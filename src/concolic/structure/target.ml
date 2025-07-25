
open Core
open Concolic_common

(* we can safely use this when making because it's promised that we never make a target twice *)
let uid = Utils.Counter.create ()

type 'k t =
  { path_n : int
  ; uniq_id : int
  ; path : 'k Path.t }

let make (path : 'k Path.t) : 'k t =
  { path_n = Path.length path
  ; uniq_id = Utils.Counter.next uid
  ; path }

let path { path ; _ } = path

let to_expressions ({ path ; _ } : 'k t) : (bool, 'k) Overlays.Typed_smt.t list =
  Path.to_dirs path
  |> List.map ~f:Direction.to_expression

(*
  SUPER IMPORTANT NOTE:
  * The concolic evaluator is currently built to only make each
    target once. Therefore, we can create a unique id upon target
    creation, and that is safe to use when comparing.
  * This will break if the concolic evaluator does not have this
    property, and it won't break loudly, so the developer must be
    very careful that this assumption continues to hold.
  * If only equality were needed, we would use `a == b`, as it
    does the same.
*)
let compare (a : 'k t) (b : 'k t) : int =
  Int.compare a.uniq_id b.uniq_id

let path_n ({ path_n ; _ } : 'k t) : int =
  path_n
