open Core

let num_allowed_max_step = 3

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

module Input =
  struct
    (* int is input value *)
    type t = (Lookup_key.t * int) list [@@deriving compare, sexp]
  end

module Status =
  struct
    (* ignore payloads on compare because they are nondeterministic *)
    type t =
      | Hit of (Input.t list [@compare.ignore])[@sexp.list]
      | Unhit
      | Unsatisfiable
      | Found_abort of (Input.t list [@compare.ignore])[@sexp.list]
      | Reach_max_step of (int [@compare.ignore])
      | Missed
      | Unreachable_because_abort
      | Unreachable_because_max_step
      | Unreachable (* any unhit branch whose parent is unsatisfiable *)
      [@@deriving variants, compare, sexp]

    (* ignores payload *)
    let to_string x = Variants.to_name x |> String.capitalize

    let is_hit = function
      | Hit _ | Found_abort _ | Reach_max_step _ -> true
      | _ -> false

    let update (new_status : t) (old_status : t) : t =
      match old_status with
      | Unsatisfiable -> begin
        match new_status with
        | Unsatisfiable | Unreachable -> old_status
        | _ -> failwith "tried to change unsatisfiable status" 
      end
      | Hit ls -> begin
        match new_status with
        | Hit ls' -> Hit (ls' @ ls)
        | Found_abort ls' -> Found_abort (ls' @ ls)
        | Reach_max_step _  -> new_status (* TODO: don't overwrite? *)
        | _ -> old_status
      end
      | Unhit -> new_status
      | Found_abort ls -> begin
        match new_status with
        | Hit _ | Found_abort _ | Reach_max_step _ -> failwith "rehitting abort"
        | _ -> old_status
      end
      | Reach_max_step count -> begin
        match new_status with
        | Reach_max_step count' -> Reach_max_step (count + count')
        | Found_abort _ -> new_status
        | _ -> old_status (* TODO: allow hits to represent completing the branch without problem *)
      end
      | Missed -> new_status
      | Unreachable
      | Unreachable_because_abort
      | Unreachable_because_max_step -> old_status
  end

module Status_store =
  struct
    module M = Map.Make (Branch)
    type t = Status.t M.t [@@deriving compare] (* will do sexp conversions manually *)

    let is_hit (map : t) (branch : Branch.t) : bool =
      match Map.find map branch with
      | Some status when Status.is_hit status -> true
      | _ -> false

    let is_valid_target (map : t) (branch : Branch.t) : bool =
      match Map.find map branch with
      | Some Unhit
      | Some Missed -> true
      | Some Reach_max_step i -> i <= num_allowed_max_step
      | _ -> false

    let empty = M.empty

    let add_branch_id (map : t) (id : Jayil.Ast.ident) : t =
      let set_unhit = function
        | Some _ -> failwith "adding non-new branch ident"
        | None -> Status.Unhit
      in
      map
      |> Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.True_direction } ~f:set_unhit
      |> Fn.flip Map.update Branch.{ branch_ident = id ; direction = Branch.Direction.False_direction } ~f:set_unhit

    let rec find_branches (e : Jayil.Ast.expr) (m : t) : t =
      let open Jayil.Ast in
      let (Expr clauses) = e in
      List.fold clauses ~init:m ~f:(fun m clause -> find_branches_in_clause clause m)

    and find_branches_in_clause (clause : Jayil.Ast.clause) (m : t) : t =
      let open Jayil.Ast in
      let (Clause (Var (x, _), cbody)) = clause in
      match cbody with
      | Conditional_body (_, e1, e2) ->
        add_branch_id m x
        |> find_branches e1
        |> find_branches e2
      | Value_body (Value_function (Function_value (_, e))) ->
        find_branches e m
      | _ -> m (* no branches in non-conditional or value *)

    let of_expr (e : Jayil.Ast.expr) : t =
      find_branches e empty

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
    let get_unhit_branch (map : t) : Branch.t option =
      map
      |> Map.to_alist
      |> List.find_map ~f:(fun (branch, status) ->
          match status with
          | Status.Unhit | Missed -> Some branch
          | _ -> None
        )

    let set_branch_status ~(new_status : Status.t) (map : t) (branch : Branch.t) : t =
      Map.update map branch ~f:(function
        | Some old_status -> Status.update new_status old_status
        | None -> failwith "unbound branch" 
      )

    let get_status (map : t) (branch : Branch.t) : Status.t =
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
            |> Map.set ~key:({ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.True_direction}) ~data:true_status
            |> Map.set ~key:({ branch_ident = Jayil.Ast.Ident id ; direction = Branch.Direction.False_direction}) ~data:false_status
          )
      end

    include Sexp_conversions
  end

(*
  There will be a runtime branch tracker that dives into branches and such. But it stores
  the stuff so well that I want to keep one of them in the global state and merge all others
  into it

  I can move the logic for Branch.Status_store into here. That would make a lot more sense.
  And then I'll need to think about where the logic goes for tracking hits because this is
  becoming a lot like session.

  TODO: for status store, have a list of abort and max step branches. Then simply query the
    runtime tracker for those after running, and keep a formula set of those branches. Can add
    them into 

  What I would like to do is have the session hold a branch tracker. I would prefer not to
  wrap the runtime branch tracker in Session.Concolic, but it makes sense to only have concolic
  interface with session. For now assume that concolic has a runtime branch tracker.
  Then it finishes with a list of new targets that are just ast branches. It will also be either
  OK, reach max step, or found abort. Concolic can actually handle this because it has the max
  step and abort and stuff, so we can just tell runtime to exit to global, and then report to
  session where it was found. However we need to ask for current parent where the exception was hit
  to appropriately record it. This can be returned from exit_until_global, but what about case when
  reaching max step while in global already?

  I think all I've really done here is made a better branch solver. I didn't accomplish much...

  I just need to track AST targets. I then take the top target, try to solve (and use status store
  for max step and abort formulas), and run if possible. Then collect by merging runtime,
  appending targets, updating status store (which will have payloads, and max step has a counter),


  TODO: keep list of any branches that have been hit. No need to for target stack.
    Just keep targeting the hit branches until they all have a conclusive status.
    Any remaining Unhits are unreachable for unknown reason. Maybe just keep as Unhit.

  TODO: why make a new tracker/solver for every run? Just pass in the one that holds everything
    and is already in the global scope. No extra merging necessary. This won't run into conflicts
    because any abort or similar formulas are never added directly. They are always pickable.
    However, for efficiency sake, I might like to use sets instead of lists.
*)

module Branch_set = Set.Make (Branch)

(*
  This just tracks how branches are hit in a single run. So we only use hits, and
  at most one branch will have max step or abort.

  This should track target.
*)
module Runtime =
  struct

    (* I think the concolic session should track inputs, not this *)
    module Fail_status = 
      struct
        type t =
          | Ok
          | Found_abort of Branch.t
          | Reach_max_step of Branch.t (* TODO: handle max step in global *)
      end
    
    type t =
      { hit_stack    : Branch.t list
      ; hit_branches : Branch_set.t
      ; fail_status  : Fail_status.t
      ; target       : Branch.t option
      ; hit_target   : bool }

    let empty : t =
      { hit_stack    = []
      ; hit_branches = Branch_set.empty
      ; fail_status  = Fail_status.Ok
      ; target       = None
      ; hit_target   = false }

    let with_target (target : Branch.t) : t =
      { empty with target = Some target }

    let hit_branch (x : t) (branch : Branch.t) : t =
      { x with 
        hit_stack = branch :: x.hit_stack
      ; hit_branches = Set.add x.hit_branches branch
      ; hit_target =
        match x.target with
        | None -> false
        | Some target -> Branch.compare target branch = 0 }
      
    let exit_branch (x : t) : t =
      { x with hit_stack = List.tl_exn x.hit_stack }

    let found_abort (x : t) : t =
      match x.hit_stack with
      | [] -> failwith "abort found in global scope. Failing"
      | branch :: _ -> { x with fail_status = Fail_status.Found_abort branch }

    let reach_max_step (x : t) : t =
      match x.hit_stack with
      | [] -> failwith "max step reached in global scope. Failing because unimplemented"
      | branch :: _ -> { x with fail_status = Fail_status.Reach_max_step branch }
  end

type t =
  { status_store      : Status_store.t
  ; pending_targets   : Branch.t list
  ; abort_branches    : Branch_set.t
  ; max_step_branches : Branch_set.t }

let empty : t =
  { status_store      = Status_store.empty
  ; pending_targets   = []
  ; abort_branches    = Branch_set.empty
  ; max_step_branches = Branch_set.empty }

let of_expr (expr : Jayil.Ast.expr) : t =
  { empty with status_store = Status_store.of_expr expr }

let set_unsatisfiable (x : t) (branch : Branch.t) : t =
  { x with status_store =
    Status_store.set_branch_status
      x.status_store
      branch
      ~new_status:Status.Unsatisfiable }

let collect_runtime (x : t) (runtime : Runtime.t) (input : Input.t) : t =
  let status_store = 
    Set.fold
      runtime.hit_branches
      ~init:x.status_store
      ~f:(Status_store.set_branch_status ~new_status:(Status.Hit [input]))
  in
  let pending_targets = (* TODO: consider if hit target? *)
    (runtime.hit_branches
    |> Set.to_list
    |> List.map ~f:Branch.other_direction)
    @ x.pending_targets
  in
  let x = { x with pending_targets } in
  match runtime.fail_status with
  | Ok -> { x with status_store }
  | Found_abort branch ->
    { x with status_store =
      Status_store.set_branch_status
        status_store
        branch
        ~new_status:(Status.Found_abort [input])
    ; abort_branches = Set.add x.abort_branches branch
    }
  | Reach_max_step branch ->
    { x with status_store =
      Status_store.set_branch_status
        status_store
        branch
        ~new_status:(Status.Reach_max_step 1)
    ; max_step_branches = Set.add x.max_step_branches branch
    }

let rec next_target (x : t) : Branch.t option * t =
  match x.pending_targets with
  | [] -> None, x
  | hd :: tl ->
    if Status_store.is_valid_target x.status_store hd
    then Some hd, { x with pending_targets = tl }
    else next_target { x with pending_targets = tl }

let get_aborts ({ abort_branches ; _ } : t) : Branch.t list =
  Set.to_list abort_branches

let get_max_steps ({ max_step_branches ; _ } : t) : Branch.t list =
  Set.to_list max_step_branches

let finish (x : t) : t =
  { x with status_store = Status_store.finish x.status_store }

let print ({ status_store ; _ } : t) : unit =
  Status_store.print status_store

let status_store ({ status_store ; _ } : t) : Status_store.t =
  status_store