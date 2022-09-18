open! Core
open! Types
open Dj_common

include module type of struct
  include State
end

val create : Global_config.t -> Jayil.Ast.expr -> State.t
val job_key_compare : Lookup_key.t -> Lookup_key.t -> int
val clear_phis : State.t -> unit
val pvar_picked : State.t -> Lookup_key.t -> bool