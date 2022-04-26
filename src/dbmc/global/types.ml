open! Core

(* Hashtbl.t is mutable by default.
   Using explicit *mutable* is for replacing a new one easier.
*)

module State = struct
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
    (* pvar *)
    lookup_created : Lookup_key.t Hash_set.t;
    smt_lists : (Lookup_key.t, int) Hashtbl.t;
    lookup_alert : Lookup_key.t Hash_set.t;
    (* lookup *)
    (* unroll : Unrolls.U_dbmc.t; *)
    (* debug *)
    noted_phi_map : (Lookup_key.t, (string * Z3.Expr.expr) list) Hashtbl.t;
    node_set : (Lookup_key.t, bool) Hashtbl.t;
    node_get : (Lookup_key.t, int) Hashtbl.t;
    rstk_picked : (Rstack.t, bool) Hashtbl.t;
  }
end
