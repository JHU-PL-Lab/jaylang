
type t = 
  | Escaped (* nondeterminism is allowed because determinism levels are escaped *)
  | Depth of int (* nested in this many levels of forced determinism *)

let is_determinism_allowed (x : t) : bool =
  match x with
  | Escaped -> true
  | Depth n -> n = 0

let zero : t = Depth 0

let escaped : t = Escaped

let incr : t -> t = function
  | Escaped -> Escaped (* TODO: should this be depth 1? *)
  | Depth i -> Depth (i + 1)
