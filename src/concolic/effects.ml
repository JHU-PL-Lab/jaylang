
open Core
open Lang.Ast

module Consts = struct
  type t =
    { target : Target.t
    ; options : Options.t
    ; input_feeder : Input_feeder.t }
end

module State = struct
  type t =
    { step : int
    ; path : Path.t
    ; targets : Target.t list (* is a log in the semantics, but it's more efficient in state *)
    ; rev_inputs : Interp_common.Input.t list }

  let empty : t =
    { step = 0
    ; path = Path.empty
    ; targets = []
    ; rev_inputs = [] }

  let inputs ({ rev_inputs ; _ } : t) : Interp_common.Input.t list =
    List.rev rev_inputs
end

module Read = struct
  include Status.Eval
  let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
    Status.Found_abort (State.inputs s, "Nondeterminism used when not allowed."), s
  let fail_on_fetch (id : Ident.t) (s : State.t) : t * State.t =
    Status.Unbound_variable (State.inputs s, id), s
end

module M = struct
  include Interp_common.Effects.Make (State) (Value.Env) (Read)

  let abort (msg : string) : 'a m =
    let%bind s = get in
    fail (Status.Found_abort (State.inputs s, msg))

  let type_mismatch (msg : string) : 'a m =
    let%bind s = get in
    fail (Status.Type_mismatch (State.inputs s, msg))

  (*
    For efficiency, we don't use a writer but instead thread a state of accumulated targets.
  *)
  let[@inline always][@specialise] tell (targets : Target.t list) : unit m =
    modify (fun s -> { s with targets = targets @ s.targets })
end

module type S = sig
  type 'a m = 'a M.m
  val vanish : 'a m
  val incr_step : int m
  val hit_branch : bool Direction.t -> bool Expression.t -> unit m
  val hit_case : int Direction.t -> int Expression.t -> other_cases:int list -> unit m
  val get_input : 'a Input_feeder.Stepkey.t -> Value.t m
  val run : 'a m -> Status.Eval.t * Target.t list
end

module Initialize (C : sig val c : Consts.t end) (*: S*) = struct
  let max_step = C.c.options.global_max_step
  let max_depth = C.c.options.max_tree_depth
  let input_feeder = C.c.input_feeder
  let target = C.c.target

  type 'a m = 'a M.m

  open M

  let vanish : 'a m =
    let%bind s = get in
    fail @@ Status.Finished { pruned = Path.length s.path > max_depth || s.step > max_step }

  (* this is hardcoded to the structure of the monad for efficiency *)
  let incr_step : int m =
    { run =
      fun ~reject ~accept s _ ->
        let step = s.step + 1 in
        if step > max_step
        then reject 
          (Status.Finished { pruned = Path.length s.path > max_depth || s.step > max_step })
          { s with step }
        else accept step { s with step }
    }

  let push_branch_and_tell (type a) (dir : a Direction.t) (e : a Expression.t) 
      (make_tape : a Claim.t -> Path.t -> Target.t list) : unit m =
    if Expression.is_const e then return () else
    let%bind s = get in
    let n = Path.length s.path in
    if n >= max_depth then return () else
    let claim = Claim.Equality (e, dir) in
    let%bind () = modify (fun s' -> { s' with path = Path.cons (Claim.to_expression claim) s'.path }) in
    if n < Target.path_n target
    then return ()
    else tell (make_tape claim s.path)

  let hit_branch (dir : bool Direction.t) (e : bool Expression.t) : unit m =
    push_branch_and_tell dir e (fun claim path ->
      [ Target.make
        (Path.cons (Claim.to_expression (Claim.flip claim)) path)
      ]
    )

  let hit_case (dir : int Direction.t) (e : int Expression.t) ~(other_cases : int list) : unit m =
    push_branch_and_tell dir e (fun _ path ->
      let other_dirs =
        match dir with
        | Case_default { not_in } -> List.map not_in ~f:Direction.of_int
        | Case_int i -> Case_default { not_in = i :: other_cases } :: List.map other_cases ~f:Direction.of_int
      in
      List.map other_dirs ~f:(fun d -> 
        Target.make
          (Path.cons (Claim.to_expression (Claim.Equality (e, d))) path)
      )
    )

  (*
    TODO: put this into interp_common effects
  *)
  let get_input (type a) (key : a Input_feeder.Stepkey.t) : Value.t m =
    let v = input_feeder.get key in
    match key with
    | I _ -> 
      let%bind () = modify (fun s -> { s with rev_inputs = I v :: s.rev_inputs }) in
      return @@ Value.M.VInt (v, Expression.key key)
    | B _ ->
      let%bind () = modify (fun s -> { s with rev_inputs = B v :: s.rev_inputs }) in
      return @@ Value.M.VBool (v, Expression.key key)

  let run (x : 'a m) : Status.Eval.t * Target.t list =
    match run x State.empty Read.empty with
    | Ok _, s ->
      Status.Finished { pruned = Path.length s.path >= max_depth || s.step > max_step }, s.targets
    | Error e, s -> e, s.targets
end
