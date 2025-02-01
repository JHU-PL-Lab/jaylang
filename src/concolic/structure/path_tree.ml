
open Core

module type NODE = sig
  type 'a edge

  type t =
    | Hit_with_int_children of int edge list
    | Hit_with_bool_children of { true_ : bool edge ; false_ : bool edge }
    | Target
    | Unsat
    | SolverTimeout
    | Leaf

  val empty : t

  val add_stem : t -> Stem.t -> t * Target.t list

  val formulas_of_target : t -> Target.t -> bool C_sudu.Gexpr.t list

  val set_unsat_target : t -> Target.t -> t
end

module type EDGE = sig
  type node

  type 'a t = 
    { constraint_ : 'a Claim.t
    ; goes_to     : node }
end

module rec Node : NODE with type 'a edge := 'a Edge.t = struct
  type t =
    | Hit_with_int_children of int Edge.t list
    | Hit_with_bool_children of { true_ : bool Edge.t ; false_ : bool Edge.t }
    | Target
    | Unsat
    | SolverTimeout
    | Leaf

  let empty : t = Leaf

  let rec set_at_path (tree : t) (path : Path.t) ~(to_add : t) : t =
    match path.forward_path with
    | [] -> to_add
    | dir :: tl ->
      let go t = set_at_path t { forward_path = tl } ~to_add in
      match dir, tree with
      | B True_direction, Hit_with_bool_children r -> 
        Hit_with_bool_children
          { r with true_ = { r.true_ with goes_to = go r.true_.goes_to } }
      | B False_direction, Hit_with_bool_children r -> 
        Hit_with_bool_children
          { r with false_ = { r.false_ with goes_to = go r.false_.goes_to } }
      | I Case_int i, Hit_with_int_children ls ->
        Hit_with_int_children (
          List.map ls ~f:(fun edge ->
            match edge.constraint_ with
            | Equality (_, Case_int i') when i' = i ->
              { edge with goes_to = go edge.goes_to }
            | _ -> edge
            )
        )
      | I Case_default { not_in }, Hit_with_int_children ls ->
        Hit_with_int_children (
          List.map ls ~f:(fun edge ->
            match edge.constraint_ with
            | Equality (_, Case_default { not_in = not_in' })
              when Int.Set.equal (Int.Set.of_list not_in) (Int.Set.of_list not_in') -> (* this is a little slow tbh *)
              { edge with goes_to = go edge.goes_to }
            | _ -> edge
            )
        )
      | _ -> failwith "Wrong type of branching in path tree; path is not compatible with the tree"

  (*
    We add a stem by making a tree out of it and then attaching that tree
    to where the target's old node was.

    SUPER IMPORTANT NOTE:
    * It is here that we create targets, and with this method, each target
      is only created once. We never make the same target twice. For this
      reason, we can make some assumptions in the Target module to get
      faster comparison.
    * Ensure that if this code ever changes, ever, that the assumption made
      in Target is still safe.
  *)
  let add_stem (tree : t) (stem : Stem.t) : t * Target.t list =
    let target_of_claim (type a) (claim : a Claim.t) (path_until_claim : Path.Reverse.t) : Target.t =
      Claim.direction claim
      |> Direction.pack
      |> fun p -> Path.Reverse.cons p path_until_claim
      |> Target.make
    in
    let rec make_tree (acc_node : Node.t) (acc_targets : Target.t list) (stem : Stem.t) (path : Path.Reverse.t) =
      match stem with
      | Root -> acc_node, acc_targets, Path.empty
      | Beginning_from target -> acc_node, acc_targets, Target.to_path target
      | Bool_branch { claim ; tail } -> begin
        (* Story of evaluation: We stepped down a bool branch to hit acc after taking the tail *)
        let path_to_here = Path.Reverse.drop_hd_exn path in
        let this_edge : bool Edge.t = { constraint_ = claim ; goes_to = acc_node } in
        let target_edge : bool Edge.t = { constraint_ = Claim.flip claim ; goes_to = Target } in
        let new_target = target_of_claim target_edge.constraint_ path_to_here in
        make_tree
          (match claim with
           | Equality (_, True_direction) -> Hit_with_bool_children { true_ = this_edge ; false_ = target_edge }
           | Equality (_, False_direction) -> Hit_with_bool_children { true_ = target_edge ; false_ = this_edge }
          )
          (new_target :: acc_targets)
          tail
          path_to_here
      end
      | Int_branch { claim ; other_cases ; tail } ->
        let path_to_here = Path.Reverse.drop_hd_exn path in
        let this_edge : int Edge.t = { constraint_ = claim ; goes_to = acc_node } in
        let other_edges : int Edge.t list = List.map other_cases ~f:(fun constraint_ -> Edge.{ constraint_ ; goes_to = Target }) in
        let new_targets = List.map other_edges ~f:(fun { constraint_ ; _ } -> target_of_claim constraint_ path_to_here) in
        make_tree
          (Hit_with_int_children (this_edge :: other_edges))
          (new_targets @ acc_targets)
          tail
          path_to_here
    in
    let new_tree, targets, path_to_new_tree = make_tree Leaf [] stem (Stem.to_rev_path stem) in
    set_at_path tree path_to_new_tree ~to_add:new_tree, targets

  (*
    It is obviously a common pattern to trace a path and act on the edge, but it is not intuitive
    IMO to abstract this because of the number of captured parameters during the tracing of the
    path, and it is not a simple "map".
  *)
  let formulas_of_target (tree : t) (target : Target.t) : bool C_sudu.Gexpr.t list =
    let rec loop (tree : t) (path : Path.t) =
      match path.forward_path with
      | [] -> []
      | dir :: tl ->
        let tl_path : Path.t = { forward_path = tl } in
        let edge =
          match dir, tree with
          | B True_direction, Hit_with_bool_children r -> `B r.true_
          | B False_direction, Hit_with_bool_children r -> `B r.false_
          | I Case_int i, Hit_with_int_children ls ->
            `I (List.find_exn ls ~f:(fun edge ->
                match edge.constraint_ with
                | Equality (_, Case_int i') when i' = i -> true
                | _ -> false
                )
              )
          | I Case_default { not_in }, Hit_with_int_children ls ->
            `I (List.find_exn ls ~f:(fun edge ->
                match edge.constraint_ with
                | Equality (_, Case_default { not_in = not_in' })
                  when Int.Set.equal (Int.Set.of_list not_in) (Int.Set.of_list not_in') -> (* this is a little slow tbh *)
                  true
                | _ -> false
                )
              )
          | _ -> failwith "Wrong type of branching in path tree; path is not compatible with the tree"
        in
        match edge with
        | `B e -> (Claim.to_formula e.constraint_) :: loop e.goes_to tl_path
        | `I e -> (Claim.to_formula e.constraint_) :: loop e.goes_to tl_path
    in
    loop tree
    @@ Target.to_path target


  let set_unsat_target (tree : t) (target : Target.t) : t =
    set_at_path tree ~to_add:Unsat
    @@ Target.to_path target
end

and Edge : EDGE with type node := Node.t = struct
  type 'a t = 
    { constraint_ : 'a Claim.t
    ; goes_to     : Node.t }
end

module Make (TQ : Target_queue.S) = struct
  type t = 
    { root : Node.t
    ; target_queue : TQ.t }

  let of_options : (unit, t) Options.Arrow.t =
    let open Options.Arrow.Infix in
    TQ.of_options
    >>^ (fun target_queue ->
      { root = Node.empty ; target_queue })

  let add_stem ({ root ; target_queue } : t) (stem : Stem.t) : t =
    let new_root, new_targets = Node.add_stem root stem in
    { root = new_root
    ; target_queue = TQ.push_list target_queue new_targets }

  let pop_sat_target (r : t) : (t * Target.t * Input_feeder.t) option Lwt.t =
    let pop_target ({ target_queue ; _ } as r : t) : (t * Target.t) option =
      Option.map (TQ.pop target_queue) ~f:(fun (target, queue) ->
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
end
