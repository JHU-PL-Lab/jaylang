
open Core

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

  let with_options : (t, t) Options.Arrow.t =
    Options.Arrow.make
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

module T = struct
  type t =
    { stem           : Stem.t
    ; consts         : Session_consts.t
    ; status         : Status.In_progress.t
    ; rev_inputs     : Input.t list
    ; depth_tracker  : Depth_tracker.t }
end

include T

let empty : t =
  { stem           = Stem.empty
  ; consts         = Session_consts.default
  ; status         = Status.In_progress
  ; rev_inputs     = []
  ; depth_tracker  = Depth_tracker.empty }

let with_options : (t, t) Options.Arrow.t =
  Options.Arrow.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with depth_tracker = Options.Arrow.appl Depth_tracker.with_options r x.depth_tracker
    ; consts = { x.consts with max_step = r.global_max_step } }

let make (target : Target.t) (input_feeder : Input_feeder.t) : t =
  { empty with consts = { empty.consts with target = Some target ; input_feeder }
  ; stem = Stem.of_target target }

let get_max_step (s : t) : int =
  s.consts.max_step

let get_input (type a) (key : a Stepkey.t) (s : t) : t * Value.t =
  let v = s.consts.input_feeder.get key in
  match key with
  | I _ ->
    { s with rev_inputs = I v :: s.rev_inputs }
    , Value.M.VInt (v, Expression.key key)
  | B _ ->
    { s with rev_inputs = B v :: s.rev_inputs }
    , Value.M.VBool (v, Expression.key key)

let has_reached_target (s : t) : bool =
  match s.consts.target with
  | Some target -> s.depth_tracker.cur_depth >= (Target.path_n target)
  | None -> true

let set_lazy_stem (s : t) (e : 'a Expression.t) (lazy_stem : Stem.t Lazy.t) : t =
  if Expression.is_const e
  then s (* nothing to do if the branch cannot be solved for *)
  else
    let after_incr =
      { s with depth_tracker = Depth_tracker.incr_branch s.depth_tracker }
    in
    if not after_incr.depth_tracker.is_max_depth && has_reached_target s
    then { after_incr with stem = force lazy_stem }
    else after_incr (* we're too deep to track formulas, so don't bother to push the branch *)

let hit_branch (dir : bool Direction.t) (e : bool Expression.t) (s : t) : t =
  set_lazy_stem s e @@ lazy (Stem.push_branch s.stem dir e)

let hit_case (dir : int Direction.t) (e : int Expression.t) ~(other_cases : int list) (s : t) : t =
  set_lazy_stem s e @@ lazy (Stem.push_case s.stem dir e other_cases)

let finish (s : t) : Status.Eval.t =
  match s.status with
  | In_progress
  | Diverge ->
    Finished
      { pruned = s.depth_tracker.is_max_depth
      ; reached_max_step = s.depth_tracker.is_max_step
      ; stem = s.stem }
  | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as res -> res

let diverge (s : t) : Status.Eval.t =
  finish s

let abort (s : t) : Status.Eval.t =
  Found_abort (List.rev s.rev_inputs)

let type_mismatch (s : t) (reason : string) : Status.Eval.t =
  Type_mismatch (List.rev s.rev_inputs, reason)

let unbound_variable (s : t) (id : Lang.Ast.Ident.t) : Status.Eval.t =
  Unbound_variable (List.rev s.rev_inputs, id)

let reach_max_step (s : t) : Status.Eval.t =
  finish { s with depth_tracker = Depth_tracker.hit_max_step s.depth_tracker }
