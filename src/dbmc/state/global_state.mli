open! Core
open! Types
open Dj_common

include module type of struct
  include State
end

val create : Global_config.t -> Jayil.Ast.expr -> State.t
val clear_phis : State.t -> unit
val add_phi : State.t -> Lookup_detail.t -> Z3.Expr.expr -> unit
val detail_alist : State.t -> (Lookup_key.t * Lookup_detail.t) list
