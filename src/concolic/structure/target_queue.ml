
open Core
open Concolic_common

(*
  TODO: is it possible to have K.t be a type parameter?
    It seems no because then 'k is unbound in Psq.Make
    while trying to turn the Target.t type constructor into
    some 0-arity type.
*)
module Make (K : Overlays.Typed_smt.KEY) = struct
  type k = K.t

  module KTarget = struct
    type t = k Target.t
    let compare = Target.compare
  end

  module type S = sig
    type t
    val of_options : (unit, t) Options.Arrow.t
    val push_list : t -> KTarget.t list -> t
    val remove : t -> KTarget.t -> t
    val peek : t -> KTarget.t option
  end

  let opt_fst = Option.map ~f:Tuple2.get1

  (*
    Note that in a psq, the same priority can exist for multiple keys (e.g. two different targets
    can both have priority 0), and the one that was pushed most recently is popped first.

    Also note that targets are compared using the order they are created.
  *)
  module Q = Psq.Make (KTarget) (Int) (* functional priority search queue *)

  module Uniform = struct
    type t = Q.t

    let empty : t = Q.empty

    let of_options : (unit, t) Options.Arrow.t =
      Options.Arrow.arrow (fun () -> empty)

    let push_one (q : t) (target : KTarget.t) : t =
      Q.push target (Interp_common.Rand.any_pos_int ()) q

    let push_list (q : t) (ls : KTarget.t list) : t =
      List.fold ls ~init:q ~f:push_one

    let remove (q : t) (target : KTarget.t) : t =
      Q.remove target q

    let peek (q : t) : KTarget.t option =
      opt_fst @@ Q.min q
  end

  (*
    Because many new targets get added at once from a single run that are not like the neighbors of a node
    in a typical tree search, no list-like queue can really ensure we visit the shallowest neighbors first.
    Thus, we use a priority search queue with the depth of the targets as the priority.
  *)
  module BFS = struct
    type t = Bfs of Q.t [@@unboxed]

    let return q = Bfs q

    let empty : t = return Q.empty

    let of_options : (unit, t) Options.Arrow.t =
      Options.Arrow.arrow (fun () -> empty)

    let push_one (Bfs q : t) (target : KTarget.t) : t =
      return
      @@ Q.push target (Target.path_n target) q

    let push_list (x : t) (ls : KTarget.t list) : t =
      List.fold ls ~init:x ~f:push_one

    let peek (Bfs q : t) : KTarget.t option =
      opt_fst @@ Q.min q

    let remove (Bfs q : t) (target : KTarget.t) : t =
      return
      @@ Q.remove target q
  end

  module DFS = struct
    type t =
      { q      : Q.t (* will use negative target depth as priority in order to prefer deeper targets *)
      ; stride : int }

    let of_options : (unit, t) Options.Arrow.t =
      Options.Arrow.make
      @@ fun (r : Options.t) () -> { q = Q.empty ; stride = r.max_tree_depth / r.n_depth_increments }

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

    let peek ({ q ; _ } : t) : KTarget.t option =
      opt_fst @@ Q.min q

    (* let is_empty ({ q ; _ } : t) : bool =
      Q.is_empty q *)

    let remove ({ q ; _ } as x : t) (target : KTarget.t) : t =
      { x with q = Q.remove target q }
  end

  module Merge (P : S) (Q : S) : S = struct
    type t = P.t * Q.t 

    let of_options : (unit, t) Options.Arrow.t =
      let open Options.Arrow.Infix in
      P.of_options &&& Q.of_options

    let push_list ((p, q) : t) (ls : KTarget.t list) : t =
      P.push_list p ls, Q.push_list q ls

    let remove ((p, q) : t) (target : KTarget.t) : t =
      P.remove p target, Q.remove q target

    let peek ((p, q) : t) : KTarget.t option =
      if Interp_common.Rand.bool ()
      then
        match P.peek p with
        | None -> Q.peek q
        | t -> t
      else
        match Q.peek q with
        | None -> P.peek p
        | t -> t
  end

  module All = Merge (Merge (BFS) (DFS)) (Uniform)
end
