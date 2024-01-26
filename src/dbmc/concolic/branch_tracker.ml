open Core

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

module rec Status :
  sig
    module type S =
      sig
        type t [@@deriving compare, sexp]
        val to_string : t -> string
        val is_hit : t -> bool
        val update : t -> t -> t
        val is_valid_target : t -> bool
        val unhit : unit -> t (* must do this so that all types are function types to make safe recursive module *)
        val exceeds_max_step_allowance : t -> int -> bool
        val finish : t -> bool -> t
        val missed_store : t -> Status_store.Without_payload.t option
      end

    module T : 
      sig
        type t =
          | Hit of (Input.t list [@compare.ignore])[@sexp.list]
          | Unhit
          | Unsatisfiable
          | Found_abort of (Input.t list [@compare.ignore])[@sexp.list]
          | Reach_max_step of (int [@compare.ignore])
          | Missed of Status_store.Without_payload.t
          | Unreachable_because_abort
          | Unreachable_because_max_step
          | Unknown of (int [@compare.ignore])
          | Unreachable (* any unhit branch whose parent is unsatisfiable *)
        include S with type t := t
      end

    include module type of T

    module Without_payload : 
      sig
        type t =
          | Hit
          | Unhit
          | Unsatisfiable
          | Found_abort
          | Reach_max_step
          | Missed
          | Unreachable_because_abort
          | Unreachable_because_max_step
          | Unknown
          | Unreachable
        include S with type t := t
        val t_of_with_payload : T.t -> t
        val with_payload_of_t : t -> T.t
      end
  end
  =
  struct

    module type S =
      sig
        type t [@@deriving compare, sexp]
        val to_string : t -> string
        val is_hit : t -> bool
        val update : t -> t -> t
        val is_valid_target : t -> bool
        val unhit : unit -> t
        val exceeds_max_step_allowance : t -> int -> bool
        val finish : t -> bool -> t
        val missed_store : t -> Status_store.Without_payload.t option
      end

    (* ignore payloads on compare because they are nondeterministic *)
    (* TODO: no need to ignore inputs because we have without payload *)
    module T =
      struct
        type t =
          | Hit of (Input.t list [@compare.ignore])[@sexp.list]
          | Unhit
          | Unsatisfiable
          | Found_abort of (Input.t list [@compare.ignore])[@sexp.list]
          | Reach_max_step of (int [@compare.ignore])
          | Missed of (Status_store.Without_payload.t [@compare.ignore])
          | Unreachable_because_abort
          | Unreachable_because_max_step
          | Unknown of (int [@compare.ignore])
          | Unreachable (* any unhit branch whose parent is unsatisfiable *)
          [@@deriving variants, compare, sexp]

        let unhit () = Unhit

        (* sometimes ignores payload *)
        let to_string = function
          | Reach_max_step count as x -> (Variants.to_name x |> String.capitalize) ^ Int.to_string count
          | x -> Variants.to_name x |> String.capitalize

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
            | Reach_max_step _  -> new_status
            | _ -> old_status
          end
          | Unhit -> new_status
          | Found_abort ls -> begin
            match new_status with
            (* Allow this because solver might accidentally hit, quite like missing target *)
            (* | Hit _ | Found_abort _ | Reach_max_step _ -> failwith "rehitting abort" *)
            | _ -> old_status
          end
          | Reach_max_step count -> begin
            match new_status with
            | Reach_max_step count' -> Reach_max_step (count + count')
            | Found_abort _ -> new_status
            | _ -> old_status
          end
          | Unknown count -> begin
            match new_status with
            | Unknown count' -> Unknown (count + count')
            | _ -> new_status
          end
          | Missed _ -> new_status
          | Unreachable
          | Unreachable_because_abort
          | Unreachable_because_max_step -> old_status

        let n_allowed_unknown_solves = 2

        let is_valid_target (x : t) : bool =
          match x with
          | Unhit -> true
          | Unknown count when count <= n_allowed_unknown_solves -> true
          | Missed _ -> false (* handled in Branch_tracker, which is bad design. See get_target *)
          | _ -> false

        let exceeds_max_step_allowance (x : t) (n : int) : bool =
          match x with
          | Reach_max_step m when m > n -> true
          | _ -> false

        let finish (x : t) (has_quit : bool) : t =
          match x with
          | Unhit when has_quit -> Unknown 1
          | Unhit -> Unreachable
          | Reach_max_step _ -> Hit [] (* TODO: input payload and max step count *)
          | Missed _ -> Unhit
          | _ -> x

        let missed_store = function
          | Missed s -> Some s
          | _ -> None
      end

    include T


    (* TODO: the signature S doesn't make that much sense without a payload, so should probably not use it *)
    module Without_payload =
      struct
        type t =
          | Hit
          | Unhit
          | Unsatisfiable
          | Found_abort
          | Reach_max_step
          | Missed
          | Unreachable_because_abort
          | Unreachable_because_max_step
          | Unknown
          | Unreachable
          [@@deriving variants, compare, sexp]

        let unhit () = Unhit

        let to_string (x : t) : string =
          String.capitalize
          @@ Variants.to_name x

        let is_hit = function
          | Hit | Found_abort | Reach_max_step -> true
          | _ -> false

        let t_of_with_payload (x : T.t) : t =
          match x with
          | T.Hit _ -> Hit
          | Unhit -> Unhit
          | Unsatisfiable -> Unsatisfiable
          | Found_abort _ -> Found_abort
          | Reach_max_step _ -> Reach_max_step
          | Missed _ -> Missed
          | Unreachable_because_abort -> Unreachable_because_abort
          | Unreachable_because_max_step -> Unreachable_because_max_step
          | Unknown _ -> Unknown
          | Unreachable -> Unreachable

        let with_payload_of_t (x : t) : T.t =
          match x with
          | Hit -> T.Hit []
          | Unhit -> Unhit
          | Unsatisfiable -> Unsatisfiable
          | Found_abort -> Found_abort []
          | Reach_max_step -> Reach_max_step 1
          | Missed -> Missed Status_store.Without_payload.empty
          | Unreachable_because_abort -> Unreachable_because_abort
          | Unreachable_because_max_step -> Unreachable_because_max_step
          | Unknown -> Unknown 1
          | Unreachable -> Unreachable

        let update (new_status : t) (old_status : t) : t =
          match old_status with
          | Unsatisfiable -> begin
            match new_status with
            | Unsatisfiable | Unreachable -> old_status
            | _ -> failwith "tried to change unsatisfiable status" 
          end
          | Hit -> begin
            match new_status with
            | Found_abort -> Found_abort
            | Reach_max_step  -> new_status
            | _ -> old_status
          end
          | Found_abort -> old_status
          | Reach_max_step -> begin
            match new_status with
            | Found_abort -> new_status
            | _ -> old_status
          end
          | Unhit
          | Unknown 
          | Missed -> new_status
          | Unreachable
          | Unreachable_because_abort
          | Unreachable_because_max_step -> old_status

        let is_valid_target (x : t) : bool =
          match x with
          | Unhit
          | Unknown -> true
          | Missed
          | _ -> false

        let exceeds_max_step_allowance (x : t) (_ : int) : bool =
          match x with
          | Reach_max_step -> true
          | _ -> false

        let finish (x : t) (has_quit : bool) : t =
          match x with
          | Unhit when has_quit -> Unknown
          | Unhit -> Unreachable
          | Reach_max_step -> Hit
          | Missed -> Unhit
          | _ -> x

        let missed_store _ = None
      end
  end

and Status_store :
  sig
    module Branch_map : Map.S
    module type S = 
      sig
        type status_t
        type t [@@deriving sexp, compare]
        val empty : t
        val of_expr : Jayil.Ast.expr -> t
        val print : t -> unit
        val add_branch_id : t -> Jayil.Ast.ident -> t
        val set_branch_status : new_status:status_t -> t -> Branch.t -> t
        val is_hit : t -> Branch.t -> bool
        val get_status : t -> Branch.t -> status_t
        val find_branches : Jayil.Ast.expr -> t -> t
        val finish : t -> int -> bool -> t
        val contains : t -> status_t -> bool
      end
    
    module type ST =
      sig
        type status_t
        type t = status_t Branch_map.t [@@deriving sexp, compare]
        include S with type status_t := status_t and type t := t
      end

    module type SS =
      sig
        include ST
        val is_valid_target : t -> Branch.t -> bool
        val exceeds_max_step_allowance : t -> Branch.t -> int -> bool
      end

    module Without_payload : SS with type status_t := Status.Without_payload.t
    include SS with type status_t := Status.t
    val without_payload : t -> Without_payload.t
  end
  =
  struct
    module Branch_map = Map.Make (Branch)
    module type S = 
      sig
        type status_t
        type t [@@deriving sexp, compare]
        val empty : t
        val of_expr : Jayil.Ast.expr -> t
        val print : t -> unit
        val add_branch_id : t -> Jayil.Ast.ident -> t
        val set_branch_status : new_status:status_t -> t -> Branch.t -> t
        val is_hit : t -> Branch.t -> bool
        val get_status : t -> Branch.t -> status_t
        val find_branches : Jayil.Ast.expr -> t -> t
        val finish : t -> int -> bool -> t
        val contains : t -> status_t -> bool
      end

    module type ST =
      sig
        type status_t
        type t = status_t Branch_map.t [@@deriving sexp, compare]
        include S with type status_t := status_t and type t := t
      end

    module type SS =
      sig
        include ST
        val is_valid_target : t -> Branch.t -> bool
        val exceeds_max_step_allowance : t -> Branch.t -> int -> bool
      end

    module Make (Status : Status.S) : SS with type status_t := Status.t =
      struct
        module M = Branch_map
        type t = Status.t M.t [@@deriving compare]

        let is_hit (map : t) (branch : Branch.t) : bool =
          match Map.find map branch with
          | Some status -> Status.is_hit status
          | None -> false

        let is_valid_target (map : t) (branch : Branch.t) : bool =
          match Map.find map branch with
          | Some status -> Status.is_valid_target status
          | None -> false

        let empty = M.empty

        let add_branch_id (map : t) (id : Jayil.Ast.ident) : t =
          let set_unhit = function
            | Some _ -> failwith "adding non-new branch ident"
            | None -> Status.unhit ()
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

        let get_unhit_branch (map : t) : Branch.t option =
          map
          |> Map.to_alist
          |> List.find_map ~f:(fun (branch, status) ->
              if not @@ Status.is_hit status (* fix: not hit is not same as unhit. Should be Unhit, Missed, and Unknown *)
              then Some branch
              else None
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

        let exceeds_max_step_allowance (map : t) (branch : Branch.t) (allowed_max_step : int) : bool =
          match Map.find map branch with
          | Some status when Status.exceeds_max_step_allowance status allowed_max_step -> true
          | _ -> false

        let finish (map : t) (allowed_max_step : int) (has_quit : bool) : t =
          Map.map map ~f:(Fn.flip Status.finish has_quit)

        let contains (map : t) (status : Status.t) : bool =
          Map.exists map ~f:(fun v -> Status.compare v status = 0)
           
        (* Custom sexp conversions so that it's easier to hand-write the sexp files *)
        module Sexp_conversions =
          struct
            module My_tuple =
              struct
                (* branch name, true status, false status *)
                type t = string * Status.t * Status.t [@@deriving sexp]
              end

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

    module Without_payload = Make (Status.Without_payload)
    include Make (Status.T)

    let without_payload (m : t) : Without_payload.t =
      Map.map m ~f:Status.Without_payload.t_of_with_payload
  end

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
      ; new_targets  : Branch.t list
      ; fail_status  : Fail_status.t
      ; target       : Branch.t option
      ; hit_target   : bool }

    let empty : t =
      { hit_stack    = []
      ; hit_branches = Branch_set.empty
      ; new_targets  = []
      ; fail_status  = Fail_status.Ok
      ; target       = None
      ; hit_target   = true }

    let with_target (target : Branch.t) : t =
      { empty with target = Some target ; hit_target = false }

    let hit_branch (x : t) (branch : Branch.t) : t =
      { x with 
        hit_stack = branch :: x.hit_stack
      ; hit_branches = Set.add x.hit_branches branch
      ; new_targets = if Set.mem x.hit_branches branch then x.new_targets else Branch.other_direction branch :: x.new_targets
      ; hit_target =
        match x.target with
        | None -> true
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
  ; max_step_branches : Branch_set.t
  ; allowed_max_step  : int }

(* number of times a branch can hit max step before being considered off limits in future runs *)
let default_allowed_max_step = 3

let empty : t =
  { status_store      = Status_store.empty
  ; pending_targets   = []
  ; abort_branches    = Branch_set.empty
  ; max_step_branches = Branch_set.empty
  ; allowed_max_step  = default_allowed_max_step }

let of_expr (expr : Jayil.Ast.expr) : t =
  { empty with status_store = Status_store.of_expr expr }

let set_allowed_max_step (x : t) (allowed_max_step : int) : t =
  { x with allowed_max_step }

let set_unsatisfiable (x : t) (branch : Branch.t) : t =
  { x with status_store =
    Status_store.set_branch_status
      x.status_store
      branch
      ~new_status:Status.Unsatisfiable }

let set_status (x : t) (branch : Branch.t) (status : Status.t) : t =
  { x with status_store =
    Status_store.set_branch_status
      x.status_store
      branch
      ~new_status:status }

let collect_runtime (x : t) (runtime : Runtime.t) (input : Input.t) : t =
  (* add hit branches *)
  let status_store = 
    Set.fold
      runtime.hit_branches
      ~init:x.status_store
      ~f:(Status_store.set_branch_status ~new_status:(Status.Hit [input]))
  in
  (* overwrite x with new targets added *)
  let x = { x with pending_targets = runtime.new_targets @ x.pending_targets } in
  (* add in aborts or max steps *)
  match runtime.fail_status with
  | Ok -> { x with status_store }
  | Found_abort branch ->
    { x with status_store =
      Status_store.set_branch_status
        status_store
        branch
        ~new_status:(Status.Found_abort [input])
    ; abort_branches = Set.add x.abort_branches branch }
  | Reach_max_step branch ->
    let status_store =
      (* add one to max step count *)
      Status_store.set_branch_status
        status_store
        branch
        ~new_status:(Status.Reach_max_step 1)
    in
    (* handle missed target *)
    let status_store =
      if not runtime.hit_target
      then
        begin
        Status_store.set_branch_status
          status_store
          (Option.value_exn runtime.target) (* safe because hit_target is true if target is None *)
          ~new_status:(Status.Missed (Status_store.without_payload status_store)) (* TODO: set the missed branch to Missed in without payload *)
        end
      else
        status_store
    in
    { x with
      status_store
    ; max_step_branches = 
      if Status_store.exceeds_max_step_allowance status_store branch x.allowed_max_step
      then Set.add x.max_step_branches branch
      else x.max_step_branches }

let rec next_target (x : t) : Branch.t option * t =
  let is_valid_target hd =
    Status_store.is_valid_target x.status_store hd
    || begin
      match Status.missed_store (Status_store.get_status x.status_store hd) with
      | None -> false (* is not valid target and is not missed, so nothing else to check: must just be invalid target *)
      | Some store -> 
        (* The status is "missed" with some status store *)
        (* This is a valid target if the current status store (i.e. branch info) has changed since the last miss *)
        Status_store.Without_payload.compare (Status_store.without_payload x.status_store) store <> 0
    end
  in
  (* Format.printf "Pending targets: %s\n" (List.to_string ~f:Branch.to_string x.pending_targets); *)
  match x.pending_targets with
  | [] -> None, x
  | hd :: tl ->
    if is_valid_target hd
    then Some hd, x (* don't pop off target in case of miss. If it gets hit, then it is easily ignored on the next run *)
    else next_target { x with pending_targets = tl }

let get_aborts ({ abort_branches ; _ } : t) : Branch.t list =
  Set.to_list abort_branches

let get_max_steps ({ max_step_branches ; _ } : t) : Branch.t list =
  Set.to_list max_step_branches

let finish (x : t) (has_quit : bool) : t =
  { x with status_store = Status_store.finish x.status_store x.allowed_max_step has_quit }

let print ({ status_store ; _ } : t) : unit =
  Status_store.print status_store

let status_store ({ status_store ; _ } : t) : Status_store.t =
  status_store