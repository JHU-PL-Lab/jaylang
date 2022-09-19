open Core
open Dj_common

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

type t =
  (* Mismatch *)
  | Withered of { phis : Z3.Expr.expr list }
  (* Value main *)
  | Leaf of { sub : Lookup_key.t; phis : Z3.Expr.expr list }
  (* Alias | Not_ *)
  | Direct of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      phis : Z3.Expr.expr list;
    }
  (* Binop *)
  | Both of {
      sub : Lookup_key.t;
      pub1 : Lookup_key.t;
      pub2 : Lookup_key.t;
      block : Cfg.block;
      phis : Z3.Expr.expr list;
    }
  (* Non-main *)
  | Map of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : Lookup_result.t -> Lookup_result.t;
      phis : Z3.Expr.expr list;
    }
  (* Pattern *)
  | MapSeq of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : int -> Lookup_result.t -> Lookup_result.t * Z3.Expr.expr list;
      phis : Z3.Expr.expr list;
    }
  (* Fun Exit | Cond Top | Cond Btm *)
  | Chain of {
      (* Chain is almost bind *)
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      next : Lookup_key.t -> Lookup_result.t -> t option;
      phis : Z3.Expr.expr list;
    }
  (* Fun Enter Local | Fun Enter Nonlocal *)
  | Or_list of {
      sub : Lookup_key.t;
      nexts : t list;
      unbound : bool;
      phis : Z3.Expr.expr list;
    }
  (* Record *)
  | Sequence of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      next : int -> Lookup_result.t -> (Z3.Expr.expr * t) option;
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
