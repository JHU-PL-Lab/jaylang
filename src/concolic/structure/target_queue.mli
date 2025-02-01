
module type S = sig
  type t
  val of_options : (unit, t) Options.Arrow.t
  val push_list : t -> Target.t list -> t
  val remove : t -> Target.t -> t
  val pop : t -> (Target.t * t) option
end

module DFS : S
module BFS : S
module Uniform : S

module Merge (_ : S) (_ : S) : S

module All : S
