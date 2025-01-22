
(* open Core *)

module type NODE = sig
  type 'a child

  type t = 
    | Int_children of int child list
    | Bool_children of { true_child : bool child ; false_child : bool child }
    | Pruned
end

module type CHILD = sig
  type node

  type 'a t =
    | Hit of { claim : 'a Claim.t ; node : node }
    | Target of { claim : 'a Claim.t }
    | Unsatisfiable
    (* Note: not worrying about solver timeout right now *)
end

module rec Node :
  NODE with 
  type 'a child := 'a Child.t 
  = struct
  type t = 
    | Int_children of int Child.t list
    | Bool_children of { true_child : bool Child.t ; false_child : bool Child.t }
    | Pruned

  (* let empty : t = Pruned

  let of_stem (stem : Stem.t) : t * Target.t list =
    failwith "unimplemented"
    (* let rec make_node acc_node stem acc_targets path =
      match stem with
      | Stem.Root { beginning_at = initial_target } ->
        acc_node, acc_targets TODO: concat the initial path *)

  let add_stem (tree : t) (stem : Stem.t) : t * Target.t list =
    failwith "unimplemented"
    (* let rec loop path parent finish =
      match path with
      |  *)


  let stub : int = 0 *)

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
end

(* TODO *)
module Target_queue = struct type t end

type t = 
  { root : Node.t
  ; target_queue : Target_queue.t }
