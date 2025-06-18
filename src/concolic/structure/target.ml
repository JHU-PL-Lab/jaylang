
(* we can safely use this when making because it's promised that we never make a target twice *)
let uid = Utils.Counter.create ()

type t =
  { path_n : int
  ; uniq_id : int
  ; path : Path.t }

let make (path : Path.t) : t =
  { path_n = Path.length path
  ; uniq_id = Utils.Counter.next uid
  ; path }

let to_expressions ({ path ; _ } : t) : bool Expression.t list =
  Path.to_exprs path

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
let compare (a : t) (b : t) : int =
  Int.compare a.uniq_id b.uniq_id

let path_n ({ path_n ; _ } : t) : int =
  path_n
