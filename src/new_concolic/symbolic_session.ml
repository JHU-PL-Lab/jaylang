
open Core

module Status = struct
  type t =
    | Found_abort of (Input_feeder.Input.t list [@compare.ignore])
    | Type_mismatch of (Input_feeder.Input.t list [@compare.ignore])
    | Finished_interpretation of { pruned : bool ; reached_max_step : bool ; stem : Stem.t }
    (* [@@deriving compare, sexp] *)

  let prune (s : t) : t =
    match s with
    | Finished_interpretation r -> Finished_interpretation { r with pruned = true }
    | _ -> s
end

module Depth_tracker = struct
  type t =
    { cur_depth    : int (* branch depth *)
    ; max_depth    : int (* only for conditional branch depth *)
    ; is_max_step  : bool
    ; is_max_depth : bool } 
    (** [t] helps track if we've reached the max tree depth and thus should stop creating formulas *)

  let empty : t =
    { cur_depth    = 0
    ; max_depth    = Options.default.max_tree_depth
    ; is_max_step  = false
    ; is_max_depth = false }

  let with_options : (t, t) Options.Fun.a =
    Options.Fun.make
    @@ fun (r : Options.t) -> fun (x : t) -> { x with max_depth = r.max_tree_depth }

  let incr_branch (x : t) : t =
    { x with cur_depth = x.cur_depth + 1 ; is_max_depth = x.max_depth <= x.cur_depth }

  let hit_max_step (x : t) : t =
    { x with is_max_step = true }
end

(* These don't change during the session, so keep them in one record to avoid so much copying *)
module Session_consts = struct
  type t = 
    { target       : Target.t option
    ; input_feeder : Input_feeder.t
    ; max_step     : int } 

  let default : t =
    { target       = None
    ; input_feeder = Input_feeder.zero
    ; max_step     = Options.default.global_max_step }
end

module T =
struct
  type t =
    { stem           : Stem.t
    ; consts         : Session_consts.t
    ; status         : [ `In_progress | `Found_abort | `Type_mismatch | `Diverged ]
    ; rev_inputs     : Input_feeder.Input.t list
    ; depth_tracker  : Depth_tracker.t }
end

include T

let empty : t =
  { stem           = Stem.empty
  ; consts         = Session_consts.default
  ; status         = `In_progress
  ; rev_inputs     = []
  ; depth_tracker  = Depth_tracker.empty }

let with_options : (t, t) Options.Fun.a =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with depth_tracker = Options.Fun.appl Depth_tracker.with_options r x.depth_tracker
    ; consts = { x.consts with max_step = r.global_max_step } }

let make (target : Target.t) (input_feeder : Input_feeder.t) : t =
  { empty with consts = { empty.consts with target = Some target ; input_feeder }
  ; stem = Stem.of_target target }

let get_max_step (s : t) : int =
  s.consts.max_step

let get_int_input (key : Concolic_key.t) (s : t) : t * Value.t =
  let i = s.consts.input_feeder.get_int key in
  { s with rev_inputs = Int i :: s.rev_inputs }
  , Value.VInt (i, Expression.int_key key)

let get_bool_input (key : Concolic_key.t) (s : t) : t * Value.t =
  let b = s.consts.input_feeder.get_bool key in
  { s with rev_inputs = Bool b :: s.rev_inputs }
  , Value.VBool (b, Expression.bool_key key)

let has_reached_target (s : t) : bool =
  match s.consts.target with
  | Some target -> s.depth_tracker.cur_depth >= target.path_n
  | None -> true

let set_lazy_stem (s : t) (e : 'a Expression.t) (lazy_stem : Stem.t Lazy.t) : t =
  if Expression.is_const e
  then s (* nothing to do if the branch cannot be solved for *)
  else
    let after_incr =
      { s with depth_tracker = Depth_tracker.incr_branch s.depth_tracker }
    in
    if after_incr.depth_tracker.is_max_depth || Fn.non has_reached_target s
    then after_incr (* we're too deep to track formulas, so don't bother to push the branch *)
    else { after_incr with stem = force lazy_stem }

let hit_branch (dir : bool Direction.t) (e : bool Expression.t) (s : t) : t =
  set_lazy_stem s e @@ lazy (Stem.push_branch s.stem dir e)

let hit_case (dir : int Direction.t) (e : int Expression.t) ~(other_cases : int list) (s : t) : t =
  set_lazy_stem s e @@ lazy (Stem.push_case s.stem dir e other_cases)

let diverge (s : t) : t =
  { s with status = `Diverged }

let abort (s : t) : t =
  { s with status = `Found_abort }

let type_mismatch (s : t) : t =
  { s with status = `Type_mismatch }

let reach_max_step (s : t) : t =
  { s with depth_tracker = Depth_tracker.hit_max_step s.depth_tracker }

let finish (s : t) : Status.t =
  match s.status with
  | `In_progress
  | `Diverged ->
    Finished_interpretation
      { pruned = s.depth_tracker.is_max_depth
      ; reached_max_step = s.depth_tracker.is_max_step
      ; stem = s.stem }
  | `Found_abort -> Found_abort (List.rev s.rev_inputs)
  | `Type_mismatch -> Type_mismatch (List.rev s.rev_inputs)
