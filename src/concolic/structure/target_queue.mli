(**
  File: target_queue.mli
  Purpose: store targets for easy popping

  Detailed description:
    Target queues are functional priorities queues that allow for
    quick retrieval of targets. They are mixed and matched by merging.

    This is just a trivial data structure holding targets.

  Dependencies:
    Options -- used to bound the queues
    Target -- is stored in the queues
*)

open Concolic_common

module Make (K : Overlays.Typed_smt.KEY) : sig
  module type S = sig
    type t
    val of_options : (unit, t) Options.Arrow.t
    val push_list : t -> K.t Target.t list -> t
    val remove : t -> K.t Target.t -> t
    val peek : t -> K.t Target.t option
  end

  module DFS : S
  (** [DFS] is bounded depth-first search. But actually we find it
      fruitful to do depth-first in increments, so it is sort of
      breadth-first in chunks, where chunks are explored depth-first. *)

  module BFS : S
  (** [BFS] is breadth-first search. *)

  module Uniform : S
  (** [Uniform] has random priority. *)

  module Merge (_ : S) (_ : S) : S
  (** [Merge (A) (B)] randomly pops like [A] and [B] with equal priority. *)

  module All : S
  (** [All] is [Merge (Merge (BFS) (DFS)) (Uniform)]. That is, DFS and BFS are
      chosen 1/4 of the time each, and Uniform is the other 1/2 of the time. *)
end
