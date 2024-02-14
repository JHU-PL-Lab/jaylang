open Core

module Q = Psq.Make (Target) (Int)
type t = Q.t

let empty : t = Q.empty
(* let is_empty : t -> bool = Q.is_empty *)

(* default priority for least prioritized element in the queue. *)
let default_prio = 0

(* let push_one (queue : t) (target : Target.t) : t =
  match Q.min queue with (* O(1) access of most prioritized *) 
  | None -> Q.push target default_prio queue (* queue was empty *)
  | Some (_, best_prio) -> Q.push target (best_prio - 1) queue  (* push target with best priority *) *)

(* For more efficiency, can just get the best priority once and add manually without `push_one` *)
let push_list (queue : t) (ls : Target.t list) : t =
  let n = List.length ls in
  let old_best_prio =
    match Q.min queue with
    | Some (_, best_prio) -> best_prio
    | None -> default_prio
  in
  let new_best_prio = old_best_prio - n in
  let new_queue =
    ls
    |> List.mapi ~f:(fun i target -> target, i + new_best_prio)
    |> Q.of_list
  in
  if Q.is_empty queue
  then new_queue
  else Q.(queue ++ new_queue) (* merge the two queues *)

let pop (queue : t) : (Target.t * t) option =
  match Q.pop queue with
  | None -> None
  | Some ((target, _), q) -> Some (target, q)

(* let to_string (queue : t) : string =
  queue
  |> Q.to_priority_list
  |> List.to_string ~f:(fun (target, i) -> let open Target in Format.sprintf "(target:%s, priority:%d)\n" (Branch.Runtime.to_string target.child.branch) i) *)