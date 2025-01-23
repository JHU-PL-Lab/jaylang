
open Core

(*
  Edges have claims that must be satisfied to walk the edge.
  Node is either
    * Hit (and int or bool children)
    * Target
    * Unsat
    * Solver timeout
    * Leaf (for root node and any "pruned" nodes that are end of interpretation)
*)

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

  (*
    First, make a tree out of the stem. We get a complete tree that potentially starts from a target.
    If there is no target, then we just get the tree. Done.
    If there is a target, then we have to trace that target down, and when we step onto the last direction,
    then we replace that child (which is Target of { claim }) with the tree we just made.

    Process:
    * Make tree of stem:
      * Step up the stem (because we start at the bottom), at each step creating targets for each neighbor.
        * The node we just made (which has a child pointing to the previous node) is higher in the tree than the previous node
      * When we hit the root, then we know where this new tree begins from the old one

    I have confirmed the `make_tree` code with an example on bool paths. It is the correct order.

    TODO:
    * It may be more intuitive to have an Edge module where edges are just claims.
    * Then the children are a list of edges each with a child, where a child is Unsat, SolverTimeout, etc.
    * If the child is Hit, then it has children. But we would also need a Leaf or Pruned for when interp ended.
    * This also doesn't handle the root case as nicely..
  *)
  let add_stem (_tree : t) (stem : Stem.t) : t * Target.t list =

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
    let _new_tree, _targets, _path_to_new_tree = make_tree Leaf [] stem (Stem.to_rev_path stem) in
    failwith "unim"

  let formulas_of_target = fun _ -> failwith "unimplemented"

  let set_unsat_target = fun _ -> failwith "unimplemented"
end

and Edge : EDGE with type node := Node.t = struct
  type 'a t = 
    { constraint_ : 'a Claim.t
    ; goes_to     : Node.t }
end

(* module type NODE = sig
  type 'a child

  type t = 
    | Int_children of int child list
    | Bool_children of { true_child : bool child ; false_child : bool child }
    | Leaf

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
    | Leaf

  let empty : t = Leaf


  (* let of_stem (stem : Stem.t) : t * Target.t list =
    failwith "unimplemented" *)
    (* let rec make_node acc_node stem acc_targets path =
      match stem with
      | Stem.Root { beginning_at = initial_target } ->
        acc_node, acc_targets TODO: concat the initial path *)

  let replace_child (type a) (tree : t) (dir : a Direction.t) (new_child : a Child.t) : t =
    failwith "unimpl"




    (* let tree_of_stem (stem : Stem.t) : t * Target.t list * Path.t =
      let rec loop = function
      (* THIS IS TOTALLY WRONG *)
        | Stem.Root -> Leaf, [], Path.empty
        | Beginning_from target -> Leaf, [], Target.to_path target
        | Bool_branch { claim ; tail } -> begin
          let node, targets, p = loop tail in
          let hit_child = Child.Hit { claim ; node } in
          match claim with
          | Equality (e, True_direction) ->
            Bool_children { true_child = hit_child ; false_child = Target { claim = Equality (e, False_direction) } }, targets, p (* TODO: add target here and below *)
          | Equality (e, False_direction) ->
            Bool_children { true_child = Target { claim = Equality (e, True_direction)} ; false_child = hit_child }, targets, p
        end
        | Int_branch { claim ; other_cases ; tail } ->
          let node, targets, p = loop tail in
          let hit_child = Child.Hit { claim ; node } in
          failwith "unimple"
        in
      loop stem
    in
    let new_tree, targets, path_to_new_tree = tree_of_stem stem in
    let rec place_at_path t = function
      | [] -> new_tree
      | dir :: tl -> failwith "unimpl" 
    in
    place_at_path tree path_to_new_tree.forward_path, targets *)






  let set_unsat_target (_tree : t) (_target : Target.t) : t =
    failwith "unimplemented"

  (* let set_timeout_target (_tree : t) (_target : Target.t) : t =
    failwith "unimplemented" *)

  let formulas_of_target (_tree : t) (_target : Target.t) : bool C_sudu.Gexpr.t list =
    failwith "unimplemented"

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
end *)

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
