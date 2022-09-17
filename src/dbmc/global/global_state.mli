open! Core
open! Types
open Dj_common

include module type of struct
  include State
end

val create : Global_config.t -> Jayil.Ast.expr -> State.t

val clear_phis : State.t -> unit

val pvar_picked : State.t -> Lookup_key.t -> bool