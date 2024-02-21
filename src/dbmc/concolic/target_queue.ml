open Core

module Q = Psq.Make (Target) (Int) (* functional priority search queue *)

type t =
  { m : int (* maximum (i.e. least prioritized) priority in the queue. *)
  ; q : Q.t }
  (* Note: we will not update m if the item associated with m is pushed to the front because
      we only want some priority that is guaranteed to be the worst, and m still is in that case. *)

(* default priority for only element in queue *)
let default_prio = 0
let default_allowed_pushes = 50 (* only allow this many new targets per run *) (* maybe make this scale with program size *)
let empty : t = 
  { m = default_prio + 1 (* note that max priority is actually the *least prioritized* item. Lower prio is better. *)
  ; q = Q.empty }
(* let is_empty : t -> bool = Q.is_empty *)


let push_one ?(priority : [ `Front | `Back ] = `Front) ({ q ; m } as x : t) (target : Target.t) : t =
  match priority with
  | `Front -> begin
    match Q.min q with (* O(1) access of most prioritized *) 
    | None -> { x with q = Q.push target default_prio q } (* queue was empty *)
    | Some (_, best_prio) -> { x with q = Q.push target (best_prio - 1) q }  (* push target with best priority *)
  end
  | `Back -> begin
    (* use `push` so that if it is already in the queue, it is not moved to the back. To move to the back, use `add`. *)
    { q = Q.push target m q ; m = m + 1 }
  end

(* TODO: allow option for number of targets to keep. Currently is hardcoded *)
let push_list (x : t) (ls : Target.t list) : t =
  List.fold_until
    (List.rev ls) (* Earlier targets get pushed last and hence have best priority when using `Front *)
    ~init:(x, 1)
    ~f:(fun (acc, n) target ->
      if n > default_allowed_pushes
      then Stop acc
      else Continue (push_one ~priority:`Back acc target, n + 1)) (* hardcoded BFS *)
    ~finish:Tuple2.get1

(* For more efficiency, can just get the best priority once and add manually without `push_one` *)
(* I deprecate this because it is not compatible with pushing to different parts of the queue *)
(* let push_list_deprecated ({ q ; _ } as x : t) (ls : Target.t list) : t =
  let n = List.length ls in
  let old_best_prio =
    match Q.min q with
    | Some (_, best_prio) -> best_prio
    | None -> default_prio
  in
  let new_best_prio = old_best_prio - n in
  let new_queue =
    ls
    |> List.mapi ~f:(fun i target -> target, i + new_best_prio)
    |> Q.of_list
  in
  if Q.is_empty q
  then { x with q = new_queue }
  else { x with q = Q.(q ++ new_queue) } (* merge the two queues *) *)

let pop ({ q ; _ } as x : t) : (Target.t * t) option =
  match Q.pop q with
  | None -> None
  | Some ((target, _), q) -> Some (target, { x with q })

(* let to_string (queue : t) : string =
  queue
  |> Q.to_priority_list
  |> List.to_string ~f:(fun (target, i) -> let open Target in Format.sprintf "(target:%s, priority:%d)\n" (Branch.Runtime.to_string target.child.branch) i) *)