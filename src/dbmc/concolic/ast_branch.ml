open Core
open Jayil

module Status =
  struct
    (* TODO: add payload for input *)
    type t =
      | Hit
      | Unhit
      | Unsatisfiable
      | Reached_max_step (* TODO: consider runs with other inputs to try to lower step count *)
      | Unreachable (* any unhit branch whose parent is unsatisfiable *)
      [@@deriving variants, compare]

    let to_string x = Variants.to_name x |> String.capitalize

    let is_hit = function Hit -> true | _ -> false
  end

module Direction =
  struct
    type t =
      | True_direction
      | False_direction
      [@@deriving equal, compare, sexp]

    let to_string = function
      | True_direction -> "true"
      | False_direction -> "false"

    let other_direction = function
      | True_direction -> False_direction
      | False_direction -> True_direction

    let to_value_bool = function
      | True_direction -> Ast.Value_bool true
      | False_direction -> Ast.Value_bool false

    let of_bool b = if b then True_direction else False_direction
  end

module Branch_status =
  struct
    type t = Direction.t -> Status.t

    let both_unhit = fun _ -> Status.Unhit

    (* more general [hit] *)
    let set (f : t) (direction : Direction.t) (new_status : Status.t) : t =
      function
      | d when Direction.equal d direction -> new_status
      | d -> f d

    let hit (f : t) (direction : Direction.t) : t =
      set f direction Status.Hit

    (* TODO: remove ident and let T do it *)
    let to_string (f : t) (Ast.Ident s : Ast.ident) : string =
      Printf.sprintf "%s: True=%s; False=%s\n"
        s
        (Status.to_string @@ f True_direction)
        (Status.to_string @@ f False_direction)

    let print (x : t) (id : Ast.ident) : unit =
      to_string x id
      |> Core.print_string

    (* changes any occurance of old_status to new_status *)
    let map (x : t) (old_status : Status.t) (new_status : Status.t) : t =
      function
      | d when Status.compare (x d) old_status = 0 -> new_status
      | d -> x d
  end


(* A branch of the ast is either on the false side or true side *)
module T = 
 struct
    (* ident is the variable name of the clause, not the condition variable name *)
    type t =
      { branch_ident : Ast.ident
      ; direction : Direction.t }

    let to_string { branch_ident = Ast.Ident s ; direction } =
      s ^ ":" ^ Direction.to_string direction

    let of_ident_and_bool (branch_ident : Ast.ident) (dir : bool) : t =
      { branch_ident ; direction = Direction.of_bool dir }
  end

include T

(* Maps branch ident (the variable of the branch clause) to a branch status *)
module Status_store =
  struct
    type t = Branch_status.t Ast.Ident_map.t

    (* Ident_map is Batteries.Map *)
    let empty = Ast.Ident_map.empty

    let print (map : t) : unit = 
      Format.printf "\nBranch Information:\n";
      Ast.Ident_map.iter (Core.Fn.flip Branch_status.print) map

    let add_branch_id (map : t) (id : Ast.ident) : t =
      Ast.Ident_map.add id Branch_status.both_unhit map

    let get_unhit_branch (map : t) : T.t option =
      map
      |> Ast.Ident_map.to_seq
      |> Batteries.Seq.find_map (fun (key, data) ->
          match data Direction.True_direction, data Direction.False_direction with
          | Status.Unhit, _ -> Some T.{ branch_ident = key ; direction = Direction.True_direction }
          | _, Status.Unhit -> Some T.{ branch_ident = key ; direction = Direction.False_direction }
          | _ -> None
        )

    let set_branch_status (status : Status.t) (map : t) (branch : T.t) : t =
      Ast.Ident_map.update_stdlib
        branch.branch_ident
        (function
        | Some branch_status -> Some (Branch_status.set branch_status branch.direction status)
        | None -> failwith "unbound branch")
        map

    let hit_branch (map : t) (branch : T.t) : t =
      Printf.printf "Hitting: %s: %s\n" (let (Ast.Ident x) = branch.branch_ident in x) (Direction.to_string branch.direction);
      set_branch_status Status.Hit map branch

    let set_unsatisfiable = set_branch_status Status.Unsatisfiable
    
    let set_reached_max_step = set_branch_status Status.Reached_max_step

    let get_status (map : t) (branch : T.t) : Status.t =
      match Ast.Ident_map.find_opt branch.branch_ident map with
      | Some branch_status -> branch_status branch.direction
      | None -> failwith "unbound branch"

    (* gets new target by setting to other direction or calling get_unhit_branch *)
    (* ^ currently only sets to other direction *)
    (* deleted because moving target to branch_solver *)
    (* let hit_and_get_new_target (map : t) (hit_target : Target.t) : t * Target.t option =
      hit_branch map hit_target,
      Some T.{ hit_target with direction = Direction.other_direction hit_target.direction } *)

    (* precondition: the branch is in the map *)
    let is_hit (map : t) (branch : T.t) : bool =
      branch.direction
      |> Ast.Ident_map.find branch.branch_ident map (* is a function from direction to status *)
      |> function
        | Status.Hit -> true
        | _ -> false

    let rec find_branches (e : Ast.expr) (m : t) : t =
      let open Ast in
      let (Expr clauses) = e in
      List.fold clauses ~init:m ~f:(fun m clause -> find_branches_in_clause clause m)

    and find_branches_in_clause (clause : Ast.clause) (m : t) : t =
      let open Ast in
      let (Clause (Var (x, _), cbody)) = clause in
      match cbody with
      | Conditional_body (_, e1, e2) ->
        add_branch_id m x
        |> find_branches e1
        |> find_branches e2
      | Value_body (Value_function (Function_value (_, e))) ->
        find_branches e m
      | _ -> m (* no branches in non-conditional or value *)

    (* map any unhit to unreachable *)
    let finish (map : t) : t =
      Ast.Ident_map.map (fun b -> Branch_status.map b Status.Unhit Status.Unreachable) map
  end

