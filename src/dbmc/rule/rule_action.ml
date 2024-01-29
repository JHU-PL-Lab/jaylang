open Core
open Dj_common

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

type t =
  | Leaf of Lookup_status.t
  (* Alias | Not_ *)
  | Direct of { pub : Lookup_key.t }
  (* Binop *)
  | Both of { pub1 : Lookup_key.t; pub2 : Lookup_key.t }
  (* Non-main *)
  | Map of { pub : Lookup_key.t; map : Lookup_key.t -> Lookup_key.t }
  (* Pattern *)
  | MapSeq of {
      pub : Lookup_key.t;
      map : Lookup_result.t -> Lookup_result.t * Z3.Expr.expr list;
    }
  (* Fun Enter Local | Fun Exit | Cond Top | Cond Btm | Record | Fun Enter Nonlocal *)
  | Bind_like of {
      precursor : Lookup_key.t;
      next : int -> Lookup_key.t -> Z3.Expr.expr option * Lookup_key.t option;
      bounded : bool;
    }
  | Bind_list_like of {
      precursor : Lookup_key.t;
      next :
        int -> Lookup_key.t -> Z3.Expr.expr option * Lookup_key.t list option;
      bounded : bool;
    }
  (* Fun Enter Local | Fun Enter Nonlocal | Cond Btm *)
  | Join of { elements : t list; bounded : bool }
