open Core

(* Edge for command and edge for the dependency result is not the same
   This one is more like an edge for command.
*)

(* type lazy_edge = {
     pub_fn : Lookup_key.t -> Lookup_key.t;
     result_fn : Lookup_result.t -> unit;
   } *)

type t =
  | Leaf of { sub : Lookup_key.t }
  | Direct of { sub : Lookup_key.t; pub : Lookup_key.t; block : Cfg.block }
  | Direct_map of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      map : Lookup_result.t -> Lookup_result.t;
    }
  | Direct_bind of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      cb : Lookup_key.t -> Lookup_result.t -> unit Lwt.t;
    }
  | Both of {
      sub : Lookup_key.t;
      pub1 : Lookup_key.t;
      pub2 : Lookup_key.t;
      block : Cfg.block;
    }
  | Or_list of {
      sub : Lookup_key.t;
      pub_with_blocks : (Lookup_key.t * Cfg.block) list;
    }
  | Or_list_two_phases of {
      sub : Lookup_key.t;
      pub_details :
        (Lookup_key.t
        * Cfg.block
        * (Lookup_key.t -> Lookup_result.t -> unit Lwt.t))
        list;
    }
  | Two_phases of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block_pub : Cfg.block;
      pub_lazy : Lookup_key.t;
      block_lazy : Cfg.block;
    }
  | Two_phases_lazy of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      lazy_edge : t Lazy.t;
      pre_lazy_check : Lookup_result.t -> bool;
    }
  | Seq_on_pub of {
      sub : Lookup_key.t;
      pub : Lookup_key.t;
      block : Cfg.block;
      seq_on_pub : int -> Lookup_result.t -> bool;
    }
  | Seq_for_sub of {
      sub : Lookup_key.t;
      pub_with_cbs :
        (Lookup_key.t
        * Cfg.block
        * (int -> Lookup_key.t -> Lookup_result.t -> unit Lwt.t))
        list;
    }
