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

module Make (K : Smt.Symbol.KEY) : sig
  module type S = sig
    type t
    val make : Options.t -> t
    val push_list : t -> K.t Target.t list -> t
    val remove : t -> K.t Target.t -> t
    val pop : t -> (K.t Target.t * t) option
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
  (** [Merge (A) (B)] pops from [A] and [B] in a round robin style. *)

  module All : S
  (** [All] is [Merge (Merge (BFS) (DFS)) (Uniform)]. That is, DFS and BFS are
      chosen 1/4 of the time each, and Uniform is the other 1/2 of the time. *)
end
