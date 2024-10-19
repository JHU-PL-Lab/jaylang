open Core
open Options.Fun.Infix

module Pop_kind =
  struct
    type t =
      | DFS
      | BFS
      | Uniform (* uniformly randomly sample from all of the targets *)
      | By_ast_branch (* prioritizes the targets whose AST branches have been hit the least *)
      | Random (* randomly choose one of the above pop kinds *)

    let random () =
      match C_random.int 4 with
      | 0 -> DFS
      | 1 -> BFS
      | 2 -> Uniform
      | _ -> By_ast_branch
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
module BFS =
  struct
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

module DFS =
  struct
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

module By_ast_branch =
  struct

    (* queue heap: heap of branches and the number of times they've been hit, and their nonempty queue *)
    module QH =
      struct
        module BQ = Psq.Make (Branch) (struct
          type t = int * DFS.t
          let compare (a, _) (b, _) = Int.compare a b
        end)

        type t = BQ.t

        let empty : t = BQ.empty

        let incr (x : t) (branch : Branch.t) : t =
          BQ.update branch (function
            | Some (c, q) -> Some (c + 1, q)
            | None -> None
          ) x

        let pop (x : t) : (Target.t * t) option =
          let open Option.Let_syntax in
          BQ.min x (* get branch with fewest hits that has at least one target *)
          >>= fun (branch, (c, q)) -> q (* extract the queue for the branch *)
          |> DFS.pop (* pop best target *)
          >>| fun (target, remaining_q) ->
            target
            , if DFS.is_empty remaining_q
              then BQ.remove branch x (* delete the queue if that was the last target *)
              else BQ.add branch (c, remaining_q) x (* otherwise just update the queue *)

        let remove (x : t) (target : Target.t) : t =
          BQ.update (Target.to_ast_branch target) (function
            | None -> None
            | Some (c, q) ->
              let new_q = DFS.remove q target in
              if DFS.is_empty new_q
              then None (* delete the queue from the heap if this was the last target *)
              else Some (c, new_q) (* otherwise just keep the remainder *)
          ) x

        (* Push the target. If the queue for the target was previously empty, then we need to know the hit count. *)
        let push (x : t) (target : Target.t) (hit_count : int) (r : Options.t) : t =
          BQ.update (Target.to_ast_branch target) (function
            | Some (c, q) -> Some (c, DFS.push_one q target)
            | None -> Some (hit_count, DFS.push_one (Options.Fun.appl DFS.of_options r ()) target)
          ) x

      end

    module Hist =
      struct
        module M = Map.Make (Branch)
        type t = int M.t

        let empty : t = M.empty

        let count (x : t) (target : Target.t) : int =
          target
          |> Target.to_ast_branch
          |> Map.find x
          |> Option.value ~default:0

        let incr (x : t) (branch : Branch.t) : t =
          Map.update x branch ~f:(function
            | Some c -> c + 1
            | None -> 1
          )
      end

    type t =
      { options : Options.t (* needed to create DFS with correct depth increments *)
      ; q_heap  : QH.t (* queue heap (see module above) *)
      ; hist    : Hist.t } (* counts number of hits for each branch *)

    let empty : t =
      { options = Options.default
      ; q_heap  = QH.empty
      ; hist    = Hist.empty }

    let of_options : (unit, t) Options.Fun.a =
      Options.Fun.make
      @@ fun (r : Options.t) () -> { empty with options = r }

    let hit_branches (x : t) (ls : Branch.t list) : t =
      let rec loop heap hist = function
      | [] -> { x with q_heap = heap ; hist }
      | branch :: tl -> loop (QH.incr heap branch) (Hist.incr hist branch) tl
      in
      loop x.q_heap x.hist ls

    let pop (x : t) : (Target.t * t) option =
      Option.map ~f:(fun (target, q_heap) -> target, { x with q_heap })
      @@ QH.pop x.q_heap

    let remove (x : t) (target : Target.t) : t =
      { x with q_heap = QH.remove x.q_heap target }

    let push_list (x : t) (ls : Target.t list) : t =
      let rec loop ls heap =
        match ls with
        | [] -> { x with q_heap = heap }
        | target :: tl ->
          loop tl
          @@ QH.push heap target (Hist.count x.hist target) x.options
      in
      loop ls x.q_heap
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

let of_options : (unit, t) Options.Fun.a =
  (DFS.of_options &&& By_ast_branch.of_options)
  ^>> fun (dfs, by_branch) -> { empty with dfs ; by_branch }

let push_list ({ dfs ; bfs ; uniform ; by_branch } : t) (ls : Target.t list) : t =
  { dfs = DFS.push_list dfs ls
  ; bfs = BFS.push_list bfs ls 
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