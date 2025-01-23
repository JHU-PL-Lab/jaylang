
open Core

module type NODE = sig
  type 'a child

  type t = 
    | Int_children of int child list
    | Bool_children of { true_child : bool child ; false_child : bool child }
    | Pruned

  val empty : t

  val add_stem : t -> Stem.t -> t * Target.t list

  (* val set_timeout_target : t -> Target.t -> t *)

  val set_unsat_target : t -> Target.t -> t

  val formulas_of_target : t -> Target.t -> bool C_sudu.Gexpr.t list
end

module type CHILD = sig
  type node

  type 'a t =
    | Hit of { claim : 'a Claim.t ; node : node }
    | Target of { claim : 'a Claim.t }
    | Unsatisfiable
    | Solver_timeout
end

module rec Node :
  NODE with 
  type 'a child := 'a Child.t 
  = struct
  type t = 
    | Int_children of int Child.t list
    | Bool_children of { true_child : bool Child.t ; false_child : bool Child.t }
    | Pruned

  let empty : t = Pruned

 

  (* let of_stem (stem : Stem.t) : t * Target.t list =
    failwith "unimplemented" *)
    (* let rec make_node acc_node stem acc_targets path =
      match stem with
      | Stem.Root { beginning_at = initial_target } ->
        acc_node, acc_targets TODO: concat the initial path *)

  let add_stem (_tree : t) (_stem : Stem.t) : t * Target.t list =
    failwith "unimplemented"
    (* let rec loop path parent finish =
      match path with
      |  *)

  let set_unsat_target (_tree : t) (_target : Target.t) : t =
    failwith "unimplemented"

  (* let set_timeout_target (_tree : t) (_target : Target.t) : t =
    failwith "unimplemented" *)

  let formulas_of_target (_tree : t) (_target : Target.t) : bool C_sudu.Gexpr.t list =
    failwith "unimplemented"

  (* let child_node_exn (node : t) (dir : 'a Direction.t) : t * 'a Claim.t = *)

end

and Child : 
  CHILD with 
  type node := Node.t
  = struct
  type 'a t =
    | Hit of { claim : 'a Claim.t ; node : Node.t }
    | Target of { claim : 'a Claim.t }
    | Unsatisfiable
    | Solver_timeout
end

type t = 
  { root : Node.t
  ; target_queue : Target_queue.t }

let empty : t =
  { root = Node.empty
  ; target_queue = Target_queue.empty }

let of_options : (unit, t) Options.Fun.a =
  let open Options.Fun.Infix in
  Target_queue.of_options
  ^>> (fun target_queue ->
    { empty with target_queue })

let add_stem ({ root ; target_queue } : t) (stem : Stem.t) : t =
  let new_root, new_targets = Node.add_stem root stem in
  { root = new_root
  ; target_queue = Target_queue.push_list target_queue new_targets }

let pop_sat_target ?(kind : Target_queue.Pop_kind.t option) (r : t) : (t * Target.t * Input_feeder.t) option Lwt.t =
  let pop_target ({ target_queue ; _ } as r : t) : (t * Target.t) option =
    Option.map (Target_queue.pop ?kind target_queue) ~f:(fun (target, queue) ->
      { r with target_queue = queue }, target
    )
  in

  let rec next (r : t) =
    let%lwt () = Lwt.pause () in
    match pop_target r with
    | None -> Lwt.return None
    | Some ({ root ; _ } as r, target) ->
      Node.formulas_of_target root target
      |> C_sudu.solve
      |> function
        | C_sudu.Solve_status.Unsat -> next { r with root = Node.set_unsat_target root target }
        | Unknown -> failwith "unimplemented solver timeout" (* would want to convey that we pruned the tree if this happens *)
        | Sat model -> Lwt.return @@ Option.return ({ r with root }, target, Input_feeder.from_model model )
  in

  next r
