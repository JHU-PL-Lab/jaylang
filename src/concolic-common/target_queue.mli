
module type S = sig
  type k
  type t
  val make : Options.t -> t
  val push_list : t -> k Target.t list -> t
  val remove : t -> k Target.t -> t
  val pop : t -> (k Target.t * t) option
end

module Make (K : Smt.Symbol.KEY) : sig
  module type S = S with type k = K.t

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
