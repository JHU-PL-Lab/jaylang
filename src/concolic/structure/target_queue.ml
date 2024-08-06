open Core

[@@@ocaml.warning "-37"]

module Pop_kind =
  struct
    type t =
      | DFS
      | BFS
      | Prioritize_uncovered
      | Uniform (* uniformly randomly sample from all of the targets *)
      | Random (* randomly choose one of the above pop kinds *)

    let random () =
      match Random.int 3 with
      | 0 -> DFS
      | 1 -> BFS
      | 2 -> Uniform
      | _ -> Prioritize_uncovered (* NOTE: this is unreachable right now *)
  end

module Q = Psq.Make (Target) (Int) (* functional priority search queue *)

module Priority =
  struct
    type t = Front | Back 
  end

module T =
  struct
    type t =
      { m    : int (* maximum (i.e. least prioritized) priority in the queue. *)
      ; q    : Q.t
      ; prio : Priority.t }
      (* Note: we will not update m if the item associated with m is pushed to the front because
          we only want some priority that is guaranteed to be the worst, and m still is in that case. *)

    (* default priority for only element in queue *)
    let default_prio = 0

    let empty (prio : Priority.t): t = 
      { m = default_prio + 1 (* note that max priority is actually the *least prioritized* item. Lower prio is better. *)
      ; q = Q.empty
      ; prio }

    let push_one ({ q ; m ; prio } as x : t) (target : Target.t) : t =
      match prio with
      | Front -> begin
        match Q.min q with (* O(1) access of most prioritized *) 
        | None -> { x with q = Q.push target default_prio q } (* queue was empty *)
        | Some (_, best_prio) -> { x with q = Q.push target (best_prio - 1) q }  (* push target with best priority *)
      end
      | Back -> begin
        (* use `push` so that if it is already in the queue, it is not moved to the back. To move to the back, use `add`. *)
        { x with q = Q.push target m q ; m = m + 1 }
      end

    (*
      Notes:    
      * If priority is Front, then the first targets in ls get worse priority
      * If priority is Back, then the first targets in ls get better priority
    *)
    let push_list (x : t) (ls : Target.t list) : t =
      List.fold
        ls
        ~init:x
        ~f:push_one

    let pop ({ q ; _ } as x : t) : (Target.t * t) option =
      match Q.pop q with
      | None -> None
      | Some ((target, _), q) -> Some (target, { x with q })

    let remove ({ q ; _ } as x : t) (target : Target.t) : t =
      { x with q = Q.remove target q }
  end

module DFS =
  struct
    type t = Dfs of Q.t

    let default_prio = 0

    let empty = Dfs Q.empty

    let return q = Dfs q

    let push_one (Dfs q : t) (target : Target.t) : t =
      match Q.min q with
      | None -> return @@ Q.push target default_prio q (* queue was empty *)
      | Some (_, best_prio) -> return @@ Q.push target (best_prio - 1) q (* push with better, lower prio *)

    let push_list (x : t) (ls : Target.t list) : t =
      List.fold  
        ls
        ~init:x
        ~f:push_one

    let pop (Dfs q : t) : (Target.t * t) option =
      match Q.pop q with
      | None -> None
      | Some ((target, _), q) -> Some (target, return q)

    let is_empty (Dfs q : t) : bool =
      Q.is_empty q

    let remove (Dfs q : t) (target : Target.t) : t =
      return @@ Q.remove target q
  end

module DFS_tower =
  struct
    type t = (DFS.t * int) list
    (*
      TODO: 
      * Think about how we don't need to use the list, but instead we can use priority to partition the targets 
      * Only issue is we would do this with some "large base" number, but the theoritical limit for number of targets is just too big
      * Instead could approximate (with room for incorrectness in a stupidly massive case) by just partitioning the integer space
    *)

    let empty = [] (* placeholder. No meaning here *)

    let of_options : (unit -> t) Options.Fun.t =
      Options.Fun.make
      @@ fun (r : Options.t) -> fun () ->
        List.init
          r.n_depth_increments
          ~f:(fun i -> DFS.empty, (i + 1) * r.max_tree_depth / r.n_depth_increments)

    let push_one (x : t) (target : Target.t) : t =
      let d = List.length target.path.forward_path in
      let rec loop = function
      | [] -> []
      | (dfs, n) :: tl when d <= n -> (DFS.push_one dfs target, n) :: tl
      | hd :: tl -> hd :: loop tl
      in
      loop x

    let push_list (x : t) (ls : Target.t list) : t =
      List.fold  
        ls
        ~init:x
        ~f:push_one

    let pop (x : t) : (Target.t * t) option =
      let rec loop = function
      | [] -> None
      | (dfs, n) as hd :: tl ->
        match DFS.pop dfs with
        | Some (target, dfs) -> Some (target, (dfs, n) :: tl)
        | None -> begin
          match loop tl with
          | None -> None
          | Some (target, tl) -> Some (target, hd :: tl)
        end
      in
      loop x

    let remove (x : t) (target : Target.t) : t =
      List.map
        x
        ~f:(fun (dfs, i) -> DFS.remove dfs target, i)

  end

type t =
  { dfs_tower : DFS_tower.t
  ; bfs : T.t
  ; uniform : Q.t
  ; hit : Q.t } (* prioritized by number of times the target has been hit *)
  
let empty : t =
  { dfs_tower = DFS_tower.empty
  ; bfs = T.empty Back
  ; uniform = Q.empty
  ; hit = Q.empty }

let with_options : (t -> t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with dfs_tower = Options.Fun.appl DFS_tower.of_options r () }

(* Deeper targets are at the front of [ls] *)
let push_list ({ dfs_tower ; bfs ; hit ; uniform } : t) (ls : Target.t list) (hits : int list) : t =
  { dfs_tower = DFS_tower.push_list dfs_tower (List.rev ls) (* reverse so that deeper targets have better priority *)
  ; bfs = T.push_list bfs ls
  ; uniform = List.fold ls ~init:uniform ~f:(fun acc k -> Q.push k (Random.int Int.max_value) acc) (* give random priority *)
  ; hit = hit (* List.fold2_exn ls hits ~init:hit ~f:(fun acc k p -> Q.push k p acc) *) }

let remove (x : t) (target : Target.t) : t =
  { bfs = T.remove x.bfs target
  ; dfs_tower = DFS_tower.remove x.dfs_tower target
  ; uniform = Q.remove target x.uniform
  ; hit = x.hit (* Q.remove target x.hit *) }

let rec pop ?(kind : Pop_kind.t = DFS) (x : t) : (Target.t * t) option =
  match kind with
  | DFS -> begin
    match DFS_tower.pop x.dfs_tower with
    | Some (target, dfs_tower) -> Some (target, remove { x with dfs_tower } target)
    | None -> None
  end
  | BFS -> begin
    match T.pop x.bfs with
    | Some (target, bfs) -> Some (target, remove { x with bfs } target)
    | None -> None
  end
  | Uniform ->
    begin
    match Q.pop x.uniform with
    | Some ((target, _), uniform) -> Some (target, remove { x with uniform } target)
    | None -> None
    end
  | Prioritize_uncovered -> 
    failwith "commented out for speed because currently is unused"
    (* begin
    match Q.pop x.hit with
    | Some ((target, _), hit) -> Some (target, remove { x with hit } target)
    | None -> None
  end *)
  | Random -> pop ~kind:(Pop_kind.random ()) x