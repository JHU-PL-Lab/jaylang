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
      map : int -> Lookup_result.t -> Lookup_result.t * Z3.Expr.expr list;
    }
  (* Fun Enter Local | Fun Exit | Cond Top | Cond Btm | Record | Fun Enter Nonlocal *)
  | Chain of {
      pub : Lookup_key.t;
      next : int -> Lookup_key.t -> Z3.Expr.expr option * t option;
      bounded : bool;
    }
  (* Fun Enter Local | Fun Enter Nonlocal | Cond Btm *)
  | Or_list of { elements : t list; bounded : bool }
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

let chain_then_direct pre source =
  let next _ _r =
    (* cond_top *)
    (* true *)
    (* if Riddler.eager_check S.state S.config key_x2
         [ Riddler.(eqz key_x2 (bool_ choice)) ] *)
    (None, Some (Direct { pub = source }))
  in
  Chain { pub = pre; next; bounded = true }

let listen_but_use source value = Map { pub = source; map = Fn.const value }
