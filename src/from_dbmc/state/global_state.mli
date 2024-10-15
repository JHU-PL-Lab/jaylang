open! Core
open! Types
open Dj_common

include module type of struct
  include State
end

val create : Global_config.t -> Jayil.Ast.expr -> State.t

val add_phi :
  ?is_external:bool -> State.t -> Lookup_detail.t -> Z3.Expr.expr -> unit

val detail_alist : State.t -> (Lookup_key.t * Lookup_detail.t) list
val run_if_fresh : State.t -> Lookup_key.t -> (unit -> unit) -> unit
val add_detail_if_fresh : State.t -> Id.t -> Lookup_key.t -> unit
val scheduler_run : State.t -> unit Lwt.t
