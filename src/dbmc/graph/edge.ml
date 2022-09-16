open Core

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

type t =
  (* Value main *)
  | Leaf of { sub : Lookup_key.t }
  (* Alias | Not_ *)
  | Direct of { sub : Lookup_key.t; pub : Lookup_key.t; block : Cfg.block }
  (* Binop *)
  | Both of {
      sub : Lookup_key.t;
      pub1 : Lookup_key.t;
      pub2 : Lookup_key.t;
      block : Cfg.block;
    }
  (* Non-main *)
  | Map of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : Lookup_result.t -> Lookup_result.t;
    }
  (* Pattern *)
  | MapSeq of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : int -> Lookup_result.t -> Lookup_result.t * Z3.Expr.expr list;
    }
  (* Fun Exit | Cond Top | Cond Btm *)
  | Chain of {
      (* Chain is almost bind *)
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      next : Lookup_key.t -> Lookup_result.t -> t option;
    }
  (* Fun Enter Local | Fun Enter Nonlocal *)
  | Or_list of { sub : Lookup_key.t; nexts : t list }
  (* Record *)
  | Sequence of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      next : int -> Lookup_result.t -> (Z3.Expr.expr * t) option;
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
