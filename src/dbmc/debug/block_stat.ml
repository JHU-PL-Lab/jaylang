type t = { mutable visits : int }

let pp oc r = Fmt.int oc r.visits