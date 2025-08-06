
open Core
open Concolic_common

module type S = sig
  type k
  type t
  val make : Options.t -> t
  val push_list : t -> k Target.t list -> t
  val remove : t -> k Target.t -> t
  val pop : t -> (k Target.t * t) option
end

(*
  TODO: is it possible to have K.t be a type parameter?
    It seems no because then 'k is unbound in Psq.Make
    while trying to turn the Target.t type constructor into
    some 0-arity type.
*)
module Make (K : Smt.Symbol.KEY) = struct
  module KTarget = struct
    type t = K.t Target.t
    let compare = Target.compare
  end

  module type S = S with type k = K.t

  (*
    Note that in a psq, the same priority can exist for multiple keys (e.g. two different targets
    can both have priority 0), and the one that was pushed most recently is popped first.

    Also note that targets are compared using the order they are created.
  *)
  module Q = Psq.Make (KTarget) (Int) (* functional priority search queue *)

  module Uniform = struct
    type k = K.t
    type t = Q.t

    let empty : t = Q.empty

    let make (_options : Options.t) : t =
      empty

    let push_one (q : t) (target : KTarget.t) : t =
      Q.push target (Interp_common.Rand.any_pos_int ()) q

    let push_list (q : t) (ls : KTarget.t list) : t =
      List.fold ls ~init:q ~f:push_one

    let remove (q : t) (target : KTarget.t) : t =
      Q.remove target q

    let pop (q : t) : (KTarget.t * t) option =
      match Q.pop q with
      | Some ((target, _), t) -> Some (target, t)
      | None -> None
  end

  (*
    Because many new targets get added at once from a single run that are not like the neighbors of a node
    in a typical tree search, no list-like queue can really ensure we visit the shallowest neighbors first.
    Thus, we use a priority search queue with the depth of the targets as the priority.
  *)
  module BFS = struct
    type k = K.t
    type t = Bfs of Q.t [@@unboxed]

    let return q = Bfs q

    let empty : t = return Q.empty

    let make (_options : Options.t) : t =
      empty

    let push_one (Bfs q : t) (target : KTarget.t) : t =
      return
      @@ Q.push target (Target.path_n target) q

    let push_list (x : t) (ls : KTarget.t list) : t =
      List.fold ls ~init:x ~f:push_one

    let remove (Bfs q : t) (target : KTarget.t) : t =
      return
      @@ Q.remove target q

    let pop (Bfs q : t) : (KTarget.t * t) option =
      match Q.pop q with
      | Some ((target, _), t) -> Some (target, Bfs t)
      | None -> None
  end

  module DFS = struct
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
    let push_one ({ q ; stride } as x : t) (target : KTarget.t) : t =
      let n = Target.path_n target in
      let n_strides = n / stride + 1 in
      { x with q = Q.push target (n_strides * stride - n mod stride) q }

    let push_list (x : t) (ls : KTarget.t list) : t =
      List.fold ls ~init:x ~f:push_one

    let pop ({ q ; _ } as x : t) : (KTarget.t * t) option =
      match Q.pop q with
      | Some ((target, _), t) -> Some (target, { x with q = t })
      | None -> None

    let remove ({ q ; _ } as x : t) (target : KTarget.t) : t =
      { x with q = Q.remove target q }
  end

  module Merge (P : S) (Q : S) : S = struct
    type k = K.t
    type t = 
      { p : P.t
      ; q : Q.t 
      ; turn : [ `P | `Q ] }

    let make (options : Options.t) : t =
      { p = P.make options
      ; q = Q.make options
      ; turn = `P }

    let push_list ({ p ; q ; turn } : t) (ls : KTarget.t list) : t =
      { p = P.push_list p ls
      ; q = Q.push_list q ls
      ; turn }

    let remove ({ p ; q ; turn } : t) (target : KTarget.t) : t =
      { p = P.remove p target
      ; q = Q.remove q target
      ; turn }

    let pop ({ p ; q ; turn } : t) : (KTarget.t * t) option =
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

  module All = Merge (Merge (BFS) (DFS)) (Uniform)
end
