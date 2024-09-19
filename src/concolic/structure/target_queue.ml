open Core

[@@@ocaml.warning "-37"]

module Pop_kind =
  struct
    type t =
      | DFS
      | BFS
      | Uniform (* uniformly randomly sample from all of the targets *)
      | By_ast_branch (* prioritizes the targets whose AST branches have been hit the least *)
      | Random (* randomly choose one of the above pop kinds *)

    let random () =
      match Random.int 2 with
      | 0 -> DFS
      | 1 -> BFS
      | _ -> Uniform
  end

(*
  Note that in a psq, the same priority can exist for multiple keys (e.g. two different targets
  can both have priority 0), and the one that was pushed most recently is popped first.
*)
module Q = Psq.Make (Target) (Int) (* functional priority search queue *)

module R =
  struct
    type t = Q.t

    let empty : t = Q.empty

    let push_one (q : t) (target : Target.t) : t =
      Q.push target (Random.int Int.max_value) q

    let push_list (q : t) (ls : Target.t list) : t =
      List.fold ls ~init:q ~f:push_one

    let remove (q : t) (target : Target.t) : t =
      Q.remove target q

    let pop (q : t) : (Target.t * t) option =
      Option.map (Q.pop q) ~f:(function (target, _), q -> target, q)
  end

(*
  This implementation of BFS is somewhat dishonest. It is bread-first in that shallower targets
  are popped first, but the lexicographically smaller targets of the same depth are popped first, which is
  not typical. It is concise but not fully in depth-first spirit, so I leave it commented for now.
*)
(* module BFS =
  struct
    type t = Bfs of Q.t

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
  end *)

module BFS =
  struct
    type t =
      { m : int (* worst priority in the queue. *)
      ; q : Q.t }

    (* default priority for only element in queue *)
    let default_prio = 0

    let empty : t =
      { m = default_prio (* note that max priority is actually the *least prioritized* item. Lower prio is popped first. *)
      ; q = Q.empty }

    let push_one ({ q ; m } : t) (target : Target.t) : t =
      (* use `push` so that if it is already in the queue, it is not moved to the back. To move to the back, use `add`. *)
      { q = Q.push target m q ; m = m + 1 }

    let push_list (x : t) (ls : Target.t list) : t =
      List.fold ls ~init:x ~f:push_one

    let pop ({ q ; _ } as x : t) : (Target.t * t) option =
      Option.map (Q.pop q) ~f:(function (target, _), q -> target, { x with q })

    let remove ({ q ; _ } as x: t) (target : Target.t) : t =
      { x with q = Q.remove target q }
  end

module DFS =
  struct
    type t =
      { q      : Q.t (* will use negative target depth as priority in order to prefer deeper targets *)
      ; stride : int }

    let of_options : (unit, t) Options.Fun.t =
      Options.Fun.make
      @@ fun (r : Options.t) -> fun (() : unit) ->
        { q = Q.empty
        ; stride = r.max_tree_depth / r.n_depth_increments }

    let empty : t = Options.Fun.run of_options Options.default ()

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

(*
  I want to have a heap that allows me to get the minimum-hit branch of those with targets.
  Then pop a target with that AST branch.
  I also need to remember how many times every branch has been hit so that it has the correct
  count when it is put in the heap.

  I think this warrants a refactor where I can create a target queue that has such a heuristic.
  I want to be able to use multiple heuristics, so I should join the target queues.
*)

module By_ast_branch =
  struct
    module BQ = Psq.Make (Branch) (Int)

    module M = Map.Make (Branch)

    type t =
      { branch_hist : BQ.t
      ; branch_map  : Q.t M.t }

    let empty : t =
      { branch_hist = BQ.empty
      ; branch_map  = M.empty }

    let hit_branches ({ branch_hist ; _ } as x : t) (ls : Branch.t list) : t =
      { x with branch_hist =
        List.fold ls ~init:branch_hist ~f:(fun acc branch ->
          BQ.update branch (function
            | Some p -> Some (p + 1) (* increment number of times the branch has been hit *)
            | None -> Some 1 (* first time seeing this branch. It has been hit once *)
          ) acc
        )
      }

    let pop (x : t) : (Target.t * t) option =
      let open Option.Let_syntax in
      let rec pop branch_hist =
        BQ.pop branch_hist
        >>= fun ((branch, _), remaining_hist) -> begin (* Some branch has been hit a fewest number of times *)
          Map.find x.branch_map branch
          >>= Q.pop (* check if the least-hit branch has a target queue *)
          >>| (fun ((target, _), new_q) -> target, Map.set x.branch_map ~key:branch ~data:new_q) (* pop the best target and set the new queue (without that target) to be in the branch map *)
          |> function
            | None -> pop remaining_hist (* couldn't find a target for this branch, so try again with all of the other branches *)
            | y -> y (* found a target, so return *)
        end
    in
    pop x.branch_hist
    >>| fun (target, branch_map) -> target, { x with branch_map }

    let remove ({ branch_map ; _ } as x : t) (target : Target.t) : t =
      { x with branch_map =
          Map.change branch_map (Branch.Runtime.to_ast_branch target.branch) ~f:(function
            | Some q -> Some (Q.remove target q)
            | None -> None
          )
      }

    let push_one (branch_map : Q.t M.t) (target : Target.t) : Q.t M.t =
      Map.update branch_map (Branch.Runtime.to_ast_branch target.branch) ~f:(function
        | Some q -> Q.push target (-1 * target.path_n) q (* push branch approximately like BFS *)
        | None -> Q.push target (-1 * target.path_n) Q.empty
      ) 

    let push_list (x : t) (ls : Target.t list) : t =
      { x with branch_map = List.fold ls ~init:x.branch_map ~f:push_one }
  end

type t =
  { dfs     : DFS.t
  ; bfs     : BFS.t
  ; uniform : R.t
  ; by_branch : By_ast_branch.t }
  
let empty : t =
  { dfs = DFS.empty
  ; bfs = BFS.empty
  ; uniform = R.empty
  ; by_branch = By_ast_branch.empty }

let of_options : (unit, t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (() : unit) ->
    { empty with dfs = Options.Fun.run DFS.of_options r () }

(* Deeper targets are at the back of [ls] *)
let push_list ({ dfs ; bfs ; uniform ; by_branch } : t) (ls : Target.t list) : t =
  { dfs = DFS.push_list dfs ls
  ; bfs = BFS.push_list bfs (List.rev ls) (* reverse so that deeper targets have worse priority *)
  ; uniform = R.push_list uniform ls (* give random priority *)
  ; by_branch = By_ast_branch.push_list by_branch ls
  }

let remove (x : t) (target : Target.t) : t =
  { bfs = BFS.remove x.bfs target
  ; dfs = DFS.remove x.dfs target
  ; uniform = R.remove x.uniform target
  ; by_branch = By_ast_branch.remove x.by_branch target }

let hit_branches (x : t) (ls : Branch.t list) : t =
  { x with by_branch = By_ast_branch.hit_branches x.by_branch ls }

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
  | By_ast_branch -> begin
    match By_ast_branch.pop x.by_branch with
    | Some (target, by_branch) -> Some (target, remove { x with by_branch } target)
    | None -> None
  end
  | Random -> pop ~kind:(Pop_kind.random ()) x