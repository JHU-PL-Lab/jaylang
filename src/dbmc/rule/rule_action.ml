open Core
open Dj_common

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

type t =
  (* Mismatch *)
  | Withered of { sub : Lookup_key.t; phis : Z3.Expr.expr list }
  (* Value main *)
  | Leaf of { sub : Lookup_key.t; phis : Z3.Expr.expr list }
  (* Alias | Not_ *)
  | Direct of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      phis : Z3.Expr.expr list;
    }
  (* Binop *)
  | Both of {
      sub : Lookup_key.t;
      pub1 : Lookup_key.t;
      pub2 : Lookup_key.t;
      phis : Z3.Expr.expr list;
    }
  (* Non-main *)
  | Map of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      map : Lookup_result.t -> Lookup_result.t;
      phis : Z3.Expr.expr list;
    }
  (* Pattern *)
  | MapSeq of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      map : int -> Lookup_result.t -> Lookup_result.t * Z3.Expr.expr list;
      phis : Z3.Expr.expr list;
    }
  (* Fun Enter Local | Fun Exit | Cond Top | Cond Btm *)
  | Chain of {
      (* Chain is almost bind *)
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      next : Lookup_key.t -> Lookup_result.t -> t option;
      phis : Z3.Expr.expr list;
    }
  (* Record | Fun Enter Nonlocal *)
  (* A sequence can be viewed as a full-fledged chain. *)
  | Sequence of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      next : int -> Lookup_result.t -> (Z3.Expr.expr * t) option;
      phis : Z3.Expr.expr list;
    }
    (* Fun Enter Local | Fun Enter Nonlocal | Cond Btm *)
  | Or_list of {
      sub : Lookup_key.t;
      elements : t list;
      unbound : bool;
      phis : Z3.Expr.expr list;
    }
(* We don't need a vanilla bind here because we don't need a general callback.
   If we directly notify the expected handler on pub's result to the sub, don't use this.
   If we use the pub's result to generate your edge, the function in record only needs
   to return that edge. The `run_edge` will handle on the edge.
*)
(* | Direct_bind of {
     sub : Lookup_key.t;
     pub : Lookup_key.t;
     block : Cfg.block;
     cb : Lookup_key.t -> Lookup_result.t -> unit Lwt.t;
   } *)

(* | Or_seq of {
     sub : Lookup_key.t;
     pub : Lookup_key.t;
     block : Cfg.block;
     update_i : unit -> unit;
   } *)

(* for a compositiona edge, if a seq with indexed at key will be used later,
     the user needs to `init_list_counter` eagerly once.
   The reason for lazy init is the callback may never be called at all. The fix of
   infinitive list cannot be applied before calling the SMT solver.
*)

let phis_of = function
  | Withered e -> e.phis
  | Leaf e -> e.phis
  | Direct e -> e.phis
  | Map e -> e.phis
  | MapSeq e -> e.phis
  | Both e -> e.phis
  | Chain e -> e.phis
  | Sequence e -> e.phis
  | Or_list e -> e.phis

let sub_of = function
  | Withered e -> e.sub
  | Leaf e -> e.sub
  | Direct e -> e.sub
  | Map e -> e.sub
  | MapSeq e -> e.sub
  | Both e -> e.sub
  | Chain e -> e.sub
  | Sequence e -> e.sub
  | Or_list e -> e.sub

let source_of = function
  | Chain e -> Some e.pub
  | Sequence e -> Some e.pub
  | Direct e -> Some e.pub
  | Or_list e -> None
  | Leaf _e -> failwith "source_of: Leaf"
  | Map _e -> failwith "source_of: Map"
  | MapSeq _e -> failwith "source_of: MapSeq"
  | Both _ -> failwith "source_of: Both"
  | Withered _ -> failwith "source_of: Withered"
