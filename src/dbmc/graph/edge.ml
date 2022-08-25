open Core

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

(* type lazy_edge = {
     pub_fn : Lookup_key.t -> Lookup_key.t;
     result_fn : Lookup_result.t -> unit;
   } *)

type t =
  (* Value main *)
  | Leaf of { sub : Lookup_key.t }
  | Lazy_edge of t Lazy.t
  (* Alias *)
  (* Not_ *)
  | Direct of { sub : Lookup_key.t; pub : Lookup_key.t; block : Cfg.block }
  (* Pattern *)
  | Direct_map of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : Lookup_result.t -> Lookup_result.t;
    }
  (* Fun Exit *)
  (* Chain is a weak bind *)
  | Chain of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      next : Lookup_key.t -> Lookup_result.t -> t option;
    }
  (* Cond Top *)
  (* Cond Btm *)
  | Static_bind of {
      (* It's a bind that doesn't reply on the previous result *)
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      pre_next_check : Lookup_result.t -> bool;
      next : t;
    }
  (* Record *)
  | Static_bind_with_seq of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      seq_on_pub : int -> Lookup_result.t -> bool;
    }
  (* Binop *)
  | Both of {
      sub : Lookup_key.t;
      pub1 : Lookup_key.t;
      pub2 : Lookup_key.t;
      block : Cfg.block;
    }
  (* Fun Enter Local *)
  | Or_list of { sub : Lookup_key.t; or_list : t list }
  (* Fun Enter Nonlocal *)
  | Seq_for_sub of {
      sub : Lookup_key.t;
      pub_with_cbs :
        (Lookup_key.t
        * Cfg.block
        * (int -> Lookup_key.t -> Lookup_result.t -> unit Lwt.t))
        list;
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
