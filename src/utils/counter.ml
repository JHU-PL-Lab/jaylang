(**
  Module [Counter].

  Sometimes it's helpful to increment a counter,
  but we may run into trouble if parallel computations
  share a counter. We ease this problem by locking the
  counter with a mutex.
*)

module C = Safe_cell.Make (Int)

type t = C.t
let create () = C.create 0

let next (t : t) : int = C.map ((+) 1) t
