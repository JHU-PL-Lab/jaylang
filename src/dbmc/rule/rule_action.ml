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

let pp oc = function
  | Leaf status -> Fmt.pf oc "Leaf %a" Lookup_status.pp_short status
  | Direct a -> Fmt.pf oc "Direct %a" Lookup_key.pp a.pub
  | Both a -> Fmt.pf oc "Both %a & %a" Lookup_key.pp a.pub1 Lookup_key.pp a.pub2
  | Map a -> Fmt.pf oc "Map %a" Lookup_key.pp a.pub
  | MapSeq a -> Fmt.pf oc "MapSeq %a" Lookup_key.pp a.pub
  | Bind_like a -> Fmt.pf oc "Bind %a then .." Lookup_key.pp a.precursor
  | Bind_list_like a ->
      Fmt.pf oc "Bind_list %a then .." Lookup_key.pp a.precursor
  | Join a -> Fmt.pf oc "Join .."
