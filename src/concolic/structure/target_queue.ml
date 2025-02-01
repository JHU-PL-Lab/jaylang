
open Core

module type S = sig
  type t
  val of_options : (unit, t) Options.Arrow.t
  val push_list : t -> Target.t list -> t
  val remove : t -> Target.t -> t
  val pop : t -> (Target.t * t) option
end

(*
  Note that in a psq, the same priority can exist for multiple keys (e.g. two different targets
  can both have priority 0), and the one that was pushed most recently is popped first.
*)
module Q = Psq.Make (Target) (Int) (* functional priority search queue *)

module Uniform = struct
  type t = Q.t

  let empty : t = Q.empty

  let of_options : (unit, t) Options.Arrow.t =
    Options.Arrow.arrow (fun () -> empty)

  let push_one (q : t) (target : Target.t) : t =
    Q.push target (C_random.any_pos_int ()) q

  let push_list (q : t) (ls : Target.t list) : t =
    List.fold ls ~init:q ~f:push_one

  let remove (q : t) (target : Target.t) : t =
    Q.remove target q

  let pop (q : t) : (Target.t * t) option =
    Option.map (Q.pop q) ~f:(function (target, _), q -> target, q)
end

(*
  This implementation of BFS is somewhat dishonest. It is breadth-first in that shallower targets
  are popped first, but the lexicographically smaller targets of the same depth are popped first, which is
  not typical. 

  Further, because many new targets get added at once from a single run that are not like the neighbors of a
  node in a typical tree search, no list-like queue can really ensure we visit the shallowest neighbors first.
  Thus, we use a priority search queue with the depth of the targets as the priority.

  To be sure that targets found first are dequeued first, we could use run number as the tie-breaker in priority.
*)
module BFS = struct
  type t = Bfs of Q.t [@@unboxed]

  let return q = Bfs q

  let empty : t = return Q.empty

  let of_options : (unit, t) Options.Arrow.t =
    Options.Arrow.arrow (fun () -> empty)

  let push_one (Bfs q : t) (target : Target.t) : t =
    return
    @@ Q.push target (Target.path_n target) q

  let push_list (x : t) (ls : Target.t list) : t =
    List.fold ls ~init:x ~f:push_one

  let pop (Bfs q : t) : (Target.t * t) option =
    Option.map (Q.pop q) ~f:(function (target, _), q -> target, return q)

  let remove (Bfs q : t) (target : Target.t) : t =
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
  let push_one ({ q ; stride } as x : t) (target : Target.t) : t =
    let n = Target.path_n target in
    let n_strides = n / stride + 1 in
    { x with q = Q.push target (n_strides * stride - n mod stride) q }

  let push_list (x : t) (ls : Target.t list) : t =
    List.fold ls ~init:x ~f:push_one

  let pop ({ q ; _ } as x : t) : (Target.t * t) option =
    match Q.pop q with
    | None -> None
    | Some ((target, _), q) -> Some (target, { x with q })

  let is_empty ({ q ; _ } : t) : bool =
    Q.is_empty q

  let remove ({ q ; _ } as x : t) (target : Target.t) : t =
    { x with q = Q.remove target q }
end

module Merge (P : S) (Q : S) : S = struct
  type t = P.t * Q.t 

  let of_options : (unit, t) Options.Arrow.t =
    let open Options.Arrow.Infix in
    P.of_options &&& Q.of_options

  let push_list ((p, q) : t) (ls : Target.t list) : t =
    P.push_list p ls, Q.push_list q ls

  let remove ((p, q) : t) (target : Target.t) : t =
    P.remove p target, Q.remove q target

  let pop ((p, q) : t) : (Target.t * t) option =
    if C_random.bool ()
    then
      match P.pop p with
      | None -> Option.map (Q.pop q) ~f:(fun (target, q) -> target, (P.remove p target, q))
      | Some (target, p) -> Some (target, (p, Q.remove q target))
    else
      match Q.pop q with
      | None -> Option.map (P.pop p) ~f:(fun (target, p) -> target, (p, Q.remove q target))
      | Some (target, q) -> Some (target, (P.remove p target, q))
end

module All = Merge (Merge (BFS) (DFS)) (Uniform)
