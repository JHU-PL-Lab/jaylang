open! Core

(* Hashtbl.t is mutable by default.
   Using explicit *mutable* is for replacing a new one easier.
*)

module State = struct
  module Unroll_S :
    Unroll.S_sig
      with type message = Lookup_result.t
       and type result = Lookup_result.t = struct
    type message = Lookup_result.t
    type result = Lookup_result.t
    type key = Lookup_key.t

    let equal_message (m1 : Lookup_result.t) (m2 : Lookup_result.t) =
      Id.equal m1.from m2.from
  end

  module Unroll = Unroll.Make (Lookup_key) (Unroll_S)

  type t = {
    (* program *)
    first : Id.t;
    target : Id.t;
    program : Odefa_ast.Ast.expr;
    block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t;
    source_map : Odefa_ast.Ast.clause Odefa_ast.Ast.Ident_map.t Lazy.t;
    (* graph attr *)
    root_node : Node.ref_t;
    mutable tree_size : int;
    (* central: node attr *)
    node_map : (Lookup_key.t, Node.ref_t) Hashtbl.t;
    (* constraints *)
    mutable phis_z3 : Z3.Expr.expr list;
    phi_map : (Lookup_key.t, Z3.Expr.expr) Hashtbl.t;
    input_nodes : Lookup_key.t Hash_set.t;
    (* cvar *)
    lookup_created : Lookup_key.t Hash_set.t;
    lookup_alert : Lookup_key.t Hash_set.t;
    (* lookup *)
    unroll : Unroll.t;
    (* debug *)
    noted_phi_map : (Lookup_key.t, (string * Z3.Expr.expr) list) Hashtbl.t;
    node_set : (Lookup_key.t, bool) Hashtbl.t;
    node_get : (Lookup_key.t, int) Hashtbl.t;
    rstk_picked : (Rstack.t, bool) Hashtbl.t;
  }
end
