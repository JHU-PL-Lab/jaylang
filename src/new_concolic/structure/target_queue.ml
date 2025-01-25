
open Core
open Options.Fun.Infix

module Pop_kind = struct
  type t =
    | DFS
    | BFS
    | Uniform (* uniformly randomly sample from all of the targets *)
    | Random (* randomly choose one of the above pop kinds *)

  let random () =
    BFS
    (* match C_random.int 3 with
    | 0 -> DFS
    | 1 -> BFS
    | _ -> Uniform *)
  end

(*
  Note that in a psq, the same priority can exist for multiple keys (e.g. two different targets
  can both have priority 0), and the one that was pushed most recently is popped first.
*)
module Q = Psq.Make (Target) (Int) (* functional priority search queue *)

module R = struct
  type t = Q.t

  let empty : t = Q.empty

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

  let push_one (Bfs q : t) (target : Target.t) : t =
    return
    @@ Q.push target target.path_n q

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

  let of_options : (unit, t) Options.Fun.a =
    Options.Fun.make
    @@ fun (r : Options.t) () -> { q = Q.empty ; stride = r.max_tree_depth / r.n_depth_increments }

  let empty : t = Options.Fun.appl of_options Options.default ()

  (*
    We push targets such that higher number of strides is worse priority, but within
    the same stride count, the higher path_n is better priority.
  *)
  let push_one ({ q ; stride } as x : t) (target : Target.t) : t =
    let n_strides = target.path_n / stride + 1 in
    { x with q = Q.push target (n_strides * stride - target.path_n mod stride) q }

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

type t =
  { dfs     : DFS.t
  ; bfs     : BFS.t
  ; uniform : R.t }
  
let empty : t =
  { dfs = DFS.empty
  ; bfs = BFS.empty
  ; uniform = R.empty }

let of_options : (unit, t) Options.Fun.a =
  DFS.of_options
  ^>> fun dfs -> { empty with dfs }

let push_list ({ dfs ; bfs ; uniform } : t) (ls : Target.t list) : t =
  { dfs = DFS.push_list dfs ls
  ; bfs = BFS.push_list bfs ls 
  ; uniform = R.push_list uniform ls (* give random priority *)
  }

let remove (x : t) (target : Target.t) : t =
  { bfs = BFS.remove x.bfs target
  ; dfs = DFS.remove x.dfs target
  ; uniform = R.remove x.uniform target }

let rec pop ?(kind : Pop_kind.t = DFS) (x : t) : (Target.t * t) option =
  match kind with
  | DFS -> begin
    match DFS.pop x.dfs with
    | Some (target, dfs) -> Some (target, remove { x with dfs } target)
    | None -> None
  end
  | BFS -> begin
    match BFS.pop x.bfs with
    | Some (target, bfs) -> Some (target, remove { x with bfs } target)
    | None -> None
  end
  | Uniform ->
    begin
    match Q.pop x.uniform with
    | Some ((target, _), uniform) -> Some (target, remove { x with uniform } target)
    | None -> None
    end
  | Random -> pop ~kind:(Pop_kind.random ()) x