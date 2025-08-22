
open Core

module type S = sig
  type k
  type t
  val make : Options.t -> t
  val push_list : t -> k Target.t list -> t
  val remove : t -> k Target.t -> t
  val pop : t -> (k Target.t * t) option
end

module type MAKE = functor (K : Smt.Symbol.KEY) -> S with type k = K.t

module Make_Q (K : Smt.Symbol.KEY) = struct
  module KTarget = struct
    type t = K.t Target.t
    let compare = Target.compare
  end

  include Psq.Make (KTarget) (Int)
end

module Make_BFS : MAKE = functor (K : Smt.Symbol.KEY) -> struct
  module Q = Make_Q (K)

  type k = K.t
  type t = BFS of Q.t [@@unboxed]

  let empty : t = BFS Q.empty

  let make (_options : Options.t) : t =
    empty

  let push_one (BFS q : t) (target : k Target.t) : t =
    BFS (Q.push target (Target.path_n target) q)

  let push_list (x : t) (ls : k Target.t list) : t =
    List.fold ls ~init:x ~f:push_one

  let remove (BFS q : t) (target : k Target.t) : t =
    BFS (Q.remove target q)

  let pop (BFS q : t) : (k Target.t * t) option =
    match Q.pop q with
    | Some ((target, _), t) -> Some (target, BFS t)
    | None -> None
end

module Make_DFS : MAKE = functor (K : Smt.Symbol.KEY) -> struct
  module Q = Make_Q (K)

  type k = K.t
  type t =
    { q      : Q.t (* will use negative target depth as priority in order to prefer deeper targets *)
    ; stride : int }

  let make (options : Options.t) : t =
    { q = Q.empty ;
      stride = options.max_tree_depth / options.n_depth_increments
    }

  (*
    We push targets such that higher number of strides is worse priority, but within
    the same stride count, the higher path_n is better priority.
  *)
  let push_one ({ q ; stride } as x : t) (target : k Target.t) : t =
    let n = Target.path_n target in
    let n_strides = n / stride + 1 in
    { x with q = Q.push target (n_strides * stride - n mod stride) q }

  let push_list (x : t) (ls : k Target.t list) : t =
    List.fold ls ~init:x ~f:push_one

  let pop ({ q ; _ } as x : t) : (k Target.t * t) option =
    match Q.pop q with
    | Some ((target, _), t) -> Some (target, { x with q = t })
    | None -> None

  let remove ({ q ; _ } as x : t) (target : k Target.t) : t =
    { x with q = Q.remove target q }
end

module Make_uniform : MAKE = functor (K : Smt.Symbol.KEY) -> struct
  module Q = Make_Q (K)

  type k = K.t
  type t = Uniform of Q.t [@@unboxed]

  let empty : t = Uniform Q.empty

  let make (_options : Options.t) : t =
    empty

  let push_one (Uniform q : t) (target : k Target.t) : t =
    Uniform (Q.push target (Interp_common.Rand.any_pos_int ()) q)

  let push_list (q : t) (ls : k Target.t list) : t =
    List.fold ls ~init:q ~f:push_one

  let remove (Uniform q : t) (target : k Target.t) : t =
    Uniform (Q.remove target q)

  let pop (Uniform q : t) : (k Target.t * t) option =
    match Q.pop q with
    | Some ((target, _), t) -> Some (target, Uniform t)
    | None -> None
end

module Merge (P : S) (Q : S with type k = P.k) : S with type k = P.k = struct
  type k = P.k
  type t = 
    { p : P.t
    ; q : Q.t 
    ; turn : [ `P | `Q ] }

  let make (options : Options.t) : t =
    { p = P.make options
    ; q = Q.make options
    ; turn = `P }

  let push_list ({ p ; q ; turn } : t) (ls : k Target.t list) : t =
    { p = P.push_list p ls
    ; q = Q.push_list q ls
    ; turn }

  let remove ({ p ; q ; turn } : t) (target : k Target.t) : t =
    { p = P.remove p target
    ; q = Q.remove q target
    ; turn }

  let pop ({ p ; q ; turn } : t) : (k Target.t * t) option =
    match turn with
    | `P -> begin
      match P.pop p with
      | None -> 
        if Option.is_some (Q.pop q)
        then failwith "Invariant failure: merged target queues have different sizes"
        else None
      | Some (target, p') -> Some (target, { p = p' ; q = Q.remove q target ; turn = `Q })
    end
    | `Q -> begin
      match Q.pop q with
      | None ->
        if Option.is_some (P.pop p)
        then failwith "Invariant failure: merged target queues have different sizes"
        else None
      | Some (target, q') -> Some (target, { q = q' ; p = P.remove p target ; turn = `P })
    end
  
end

module Make_merge (Make_P : MAKE) (Make_Q : MAKE) : MAKE = functor (K : Smt.Symbol.KEY) -> Merge (Make_P (K)) (Make_Q (K))

module Make_all = Make_merge (Make_merge (Make_BFS) (Make_DFS)) (Make_uniform)

module Make (K : Smt.Symbol.KEY) = struct
  module type S = S with type k = K.t

  module DFS = Make_DFS (K)
  module BFS = Make_BFS (K)
  module Uniform = Make_uniform (K)

  module Merge = Merge

  module All = Merge (Merge (BFS) (DFS)) (Uniform)
end
