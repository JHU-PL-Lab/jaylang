open Core
open Jayil


let generate_lookup_key (x : Ast.ident) (stk : Dj_common.Concrete_stack.t) : Lookup_key.t =
  { x
  ; r_stk = Rstack.from_concrete stk
  ; block = Dj_common.Cfg.{ id = x ; clauses = [] ; kind = Main } }

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

(*
  While this just copies Lookup_key, I find it helpful to try to make
  them distinct for easier reading to tell what is a condition (Condition_key.t),
  and what is any general variable (Lookup_key.t).
*)
module Condition_key = Lookup_key



module Status =
  struct
    (* TODO: add payload for input *)
    type t =
      | Hit
      | Unhit
      | Unsatisfiable
      | Found_abort
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


module Ast_branch = 
 struct
    type t =
      { branch_ident    : Ast.ident
      ; condition_ident : Ast.ident
      ; direction       : Direction.t }

    let to_string { branch_ident = Ast.Ident s ; direction ; _ } =
      s ^ ":" ^ Direction.to_string direction

    let of_idents_and_bool (branch_ident : Ast.ident) (condition_ident : Ast.ident) (dir : bool) : t =
      { branch_ident ; condition_ident ; direction = Direction.of_bool dir }
  end


module Runtime =
  struct
    type t =
      { branch_key    : Lookup_key.t
      ; condition_key : Condition_key.t
      ; direction     : Direction.t }
      [@@deriving compare, sexp]

    let to_expr ({condition_key ; direction ; _ } : t) : Z3.Expr.expr =
      Riddler.eqv condition_key (Direction.to_value_bool direction)

    let to_branch ({ branch_key ; condition_key ; direction } : t) : Ast_branch.t =
      Ast_branch.{ branch_ident = branch_key.x ; condition_ident = condition_key.x ; direction }

    let to_string ({ branch_key ; condition_key ; direction } : t) : string =
      Lookup_key.to_string branch_key
      ^ "; condition: "
      ^ Lookup_key.to_string condition_key
      ^ " = "
      ^ Direction.to_string direction

    let other_direction (x : t) : t =
      { x with direction = Direction.other_direction x.direction }

    let print_option (x : t option) : unit =
      let target_branch_str = 
        match x with 
        | None -> "None"
        | Some target -> to_string target
      in 
      Format.printf "\nTarget branch: %s\n" target_branch_str
  end

module Status_store =
  struct
    (*
      TODO: consider restructuring to just have a status for each AST branch instead
        of trying to ignore duplication and mapping identifier to a branch status.    
        This causes some unnecessary complication, I think.
    *)
    module Branch_status :
      sig
        type t = Direction.t -> Status.t
        (** [t] is a branch direction and its status. *)
        
        val both_unhit : t
        (** [both_unhit] is a branch status that has both directions as unhit. *)

        val hit : t -> Direction.t -> t
        (** [hit x dir] is a new branch status with the direction [dir] now hit. The
            other direction gets the same status as [x]. *)

        val to_string : t -> Ast.ident -> string
        val print : t -> Ast.ident -> unit
      end
      =
      struct
        type t = Direction.t -> Status.t
        (* Avoid duplication by letting a status only take direction and output a status.
          Thus a status does not have to be associated with a branch identifier at all. *)

        let both_unhit = fun _ -> Status.Unhit

        (* more general [hit] *)
        let set (f : t) (direction : Direction.t) (new_status : Status.t) : t =
          function
          | d when Direction.equal d direction -> new_status
          | d -> f d

        let hit (f : t) (direction : Direction.t) : t =
          set f direction Status.Hit

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
      

    type t = Branch_status.t Ast.Ident_map.t

    (* Ident_map is Batteries.Map *)
    let empty = Ast.Ident_map.empty

    let print (map : t) : unit = 
      Format.printf "\nBranch Information:\n";
      Ast.Ident_map.iter (Core.Fn.flip Branch_status.print) map

    let add_branch_id (map : t) (id : Ast.ident) : t =
      Ast.Ident_map.add id Branch_status.both_unhit map

    let get_unhit_branch (map : t) : Ast_branch.t option =
      map
      |> Ast.Ident_map.to_seq
      |> Batteries.Seq.find_map (fun (key, data) ->
          match data Direction.True_direction, data Direction.False_direction with
          | Status.Unhit, _ -> Some Ast_branch.{ branch_ident = key ; direction = Direction.True_direction }
          | _, Status.Unhit -> Some Ast_branch.{ branch_ident = key ; direction = Direction.False_direction }
          | _ -> None
        )

    let set_branch_status (status : Status.t) (map : t) (branch : Ast_branch.t) : t =
      Ast.Ident_map.update_stdlib
        branch.branch_ident
        (function
        | Some branch_status -> Some (Branch_status.set branch_status branch.direction status)
        | None -> failwith "unbound branch")
        map

    let hit_branch (map : t) (branch : Ast_branch.t) : t =
      Printf.printf "Hitting: %s: %s\n" (let (Ast.Ident x) = branch.branch_ident in x) (Direction.to_string branch.direction);
      set_branch_status Status.Hit map branch

    let set_unsatisfiable = set_branch_status Status.Unsatisfiable
    
    let set_reached_max_step = set_branch_status Status.Reached_max_step

    let get_status (map : t) (branch : Ast_branch.t) : Status.t =
      match Ast.Ident_map.find_opt branch.branch_ident map with
      | Some branch_status -> branch_status branch.direction
      | None -> failwith "unbound branch"

    (* precondition: the branch is in the map *)
    let is_hit (map : t) (branch : Ast_branch.t) : bool =
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