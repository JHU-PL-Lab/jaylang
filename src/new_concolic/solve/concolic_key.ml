
open Core

(* TODO: make key an int key or bool key *)
type t = { uniq_id : int }
  [@@unboxed][@@deriving sexp]

let compare (a : t) (b : t) : int =
  Int.compare a.uniq_id b.uniq_id

let equal (a : t) (b : t) : bool =
  Int.(a.uniq_id = b.uniq_id)

let create uniq_id =
  { uniq_id }

let to_string ({ uniq_id } : t) : string =
  Format.sprintf "stepkey_$%d" uniq_id

let uniq_id ({ uniq_id } : t) : int =
  uniq_id