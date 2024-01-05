open Core
open Jayil

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

module Status =
  struct
    (* TODO: add payload for input *)
    type t =
      | Hit
      | Unhit
      | Unsatisfiable
      | Found_abort
      | Reach_max_step
      | Missed
      | Unreachable (* any unhit branch whose parent is unsatisfiable *)
      [@@deriving variants, compare, sexp]

    let to_string x = Variants.to_name x |> String.capitalize

    let is_hit = function Hit -> true | _ -> false

    let is_valid_change (new_status : t) (old_status : t) : bool =
      match old_status with
      | Unhit -> true (* everything starts as unhit, and it can always be overwritten *)
      | Unsatisfiable -> false (* it should be impossible to hit or re-solve for an unsatisfiable branch *)
      | Unreachable -> false (* we only ever can determine unreachability as a final step, so it can't be overwritten *)
      | Missed -> begin
        match new_status with
        | Hit | Unsatisfiable | Found_abort | Reach_max_step -> true
        | Unhit | Unreachable | Missed -> false
        end
      | Hit -> begin
        match new_status with
        | Found_abort | Reach_max_step -> true
        | Unhit | Unreachable | Missed | Unsatisfiable | Hit -> false
        end
      | Reach_max_step -> begin
        match new_status with
        | Found_abort | Hit (* TODO: allow hits to represent completing the branch *) -> true
        | Unhit | Unreachable | Missed | Unsatisfiable | Reach_max_step -> false
        end
      | Found_abort -> begin
        match new_status with
        | Hit -> true
        | Unhit | Unreachable | Missed | Unsatisfiable | Reach_max_step | Found_abort -> false
        end
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

module T = 
 struct
    type t =
      { branch_ident : Ast.Ident_new.t
      ; direction    : Direction.t }
      [@@deriving sexp, compare]

    let to_string { branch_ident = Ast.Ident s ; direction } =
      s ^ ":" ^ Direction.to_string direction

    let of_ident_and_bool (branch_ident : Ast.ident) (dir : bool) : t =
      { branch_ident ; direction = Direction.of_bool dir }
  end

include T

module Runtime =
  struct
    type t =
      { branch_key    : Lookup_key.t
      ; condition_key : Lookup_key.t
      ; direction     : Direction.t }
      [@@deriving compare, sexp]

    let to_expr ({condition_key ; direction ; _ } : t) : Z3.Expr.expr =
      Riddler.eqv condition_key (Direction.to_value_bool direction)

    let to_ast_branch ({ branch_key ; direction ; _ } : t) : T.t =
      T.{ branch_ident = branch_key.x ; direction }

    let to_string ({ branch_key ; condition_key ; direction } : t) : string =
      Lookup_key.to_string branch_key
      ^ "; condition: "
      ^ Lookup_key.to_string condition_key
      ^ " = "
      ^ Direction.to_string direction

    let other_direction (x : t) : t =
      { x with direction = Direction.other_direction x.direction }

    let print_target_option (x : t option) : unit =
      let target_branch_str = 
        match x with 
        | None -> "None"
        | Some target -> to_string target
      in 
      Format.printf "\nTarget branch: %s\n" target_branch_str

    let (^-) a b = a ^ "-" ^ b

    (* hyphen is invalid in variable names, so this key is unique from any variable *)
    let to_abort_pick_key (branch : t) : Lookup_key.t =
      let dir_string = Direction.to_string branch.direction in 
      let x = branch.branch_key.x ^- dir_string ^- "abort" in
      { x
      ; r_stk = Rstack.empty
      ; block = Dj_common.Cfg.{ id = x ; clauses = [] ; kind = Main } }

    let to_max_step_pick_key (branch : t) : Lookup_key.t = 
      let dir_string = Direction.to_string branch.direction in 
      let x = branch.branch_key.x ^- dir_string ^- "max_step" in
      { x
      ; r_stk = Rstack.empty
      ; block = Dj_common.Cfg.{ id = x ; clauses = [] ; kind = Main } }
  end

module Status_store =
  struct
    module M = Map.Make (T)
    type t = Status.t M.t [@@deriving compare] (* will do sexp conversions manually *)

    let is_hit (map : t) (branch : T.t) : bool =
      match Map.find map branch with
      | Some Status.Hit -> true
      | _ -> false

    let empty = M.empty

    let add_branch_id (map : t) (id : Ast.ident) : t =
      let set_unhit = function
        | Some _ -> failwith "adding non-new branch ident"
        | None -> Status.Unhit
      in
      map
      |> Fn.flip Map.update { branch_ident = id ; direction = Direction.True_direction } ~f:set_unhit
      |> Fn.flip Map.update { branch_ident = id ; direction = Direction.False_direction } ~f:set_unhit

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

    let to_list (map : t) : (string * Status.t * Status.t) list =
      Map.to_alist map ~key_order:`Increasing
      |> List.chunks_of ~length:2
      |> List.map ~f:(function
        | [ ({ branch_ident = Ident a; direction = True_direction }, true_status)
          ; ({ branch_ident = Ident b; direction = False_direction }, false_status) ]
          when String.(a = b) ->
          (a, true_status, false_status)
        | _ -> failwith "malformed status store during to_list"
      )

    (* depends on well-formedness of the map from loading in branches *)
    let print (map : t) : unit = 
      Format.printf "\nBranch Information:\n";
      map
      |> to_list
      |> List.iter ~f:(fun (s, true_status, false_status) ->
          Printf.printf "%s: True=%s; False=%s\n"
            s
            (Status.to_string true_status)
            (Status.to_string false_status)
        )

    (* TODO: we can maintain a list of unhit branches and just peek the hd to improve time complexity *)
    let get_unhit_branch (map : t) : T.t option =
      map
      |> Map.to_alist
      |> List.find_map ~f:(fun (branch, status) ->
          match status with
          | Status.Unhit | Missed -> Some branch
          | _ -> None
        )

    let set_branch_status ~(new_status : Status.t) (map : t) (branch : T.t) : t =
      Map.update map branch ~f:(function
        | Some old_status when Status.is_valid_change new_status old_status -> new_status
        | Some old_status -> old_status
        | None -> failwith "unbound branch" 
      )

    let get_status (map : t) (branch : T.t) : Status.t =
      match Map.find map branch with
      | Some status -> status
      | None -> failwith "unbound branch"

    (* map any unhit to unreachable *)
    let finish (map : t) : t =
      Map.map map ~f:(function Status.Unhit -> Status.Unreachable | x -> x)

    module Sexp_conversions =
      struct
        module My_tuple =
          struct
            (* branch name, true status, false status *)
            type t = string * Status.t * Status.t [@@deriving sexp]
          end

        (* Convert to tuple list of ident, true status, false status.
          This is atrociously inefficient, but it is only used for small maps. *)
        let sexp_of_t (map : t) : Sexp.t =
          map
          |> to_list
          |> List.sexp_of_t My_tuple.sexp_of_t
        
        let t_of_sexp (sexp : Sexp.t) : t =
          sexp
          |> List.t_of_sexp My_tuple.t_of_sexp
          |> List.fold ~init:empty ~f:(fun acc (id, true_status, false_status) ->
            acc
            |> Map.set ~key:({ branch_ident = Ast.Ident id ; direction = Direction.True_direction}) ~data:true_status
            |> Map.set ~key:({ branch_ident = Ast.Ident id ; direction = Direction.False_direction}) ~data:false_status
          )
      end

    include Sexp_conversions
  end