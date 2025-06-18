
open Core
open Lang.Ast

module type ENV = sig
  type t
  val empty : t
  val fetch : Ident.t -> t -> Value.t option
end

module Make (State : T) (Env : ENV) (Err : sig
  type t
  val fail_on_nondeterminism_misuse : State.t -> t
  val fail_on_fetch : Ident.t -> State.t -> t
end) = struct
  module Read = struct
    type t = 
      { env : Env.t
      ; det_depth : [ `Escaped | `Depth of int ] } 

    let empty : t = { env = Env.empty ; det_depth = `Depth 0 }

    let is_determinism_allowed ({ det_depth ; _ } : t) : bool =
      match det_depth with
      | `Escaped -> true
      | `Depth i -> i = 0
  end

  (* 
    ------------
    MONAD BASICS 
    ------------
  *)
  type 'a m = {
    run : 'r. reject:(Err.t -> State.t -> 'r) -> accept:('a -> State.t -> 'r) -> State.t -> Read.t -> 'r
  }

  let[@inline always][@specialise][@landmark] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    { run =
      fun ~reject ~accept s r ->
        x.run s r ~reject ~accept:(fun x s ->
          (f x).run ~reject ~accept s r
        )
    }

  let[@inline always][@specialise] return (a : 'a) : 'a m =
    { run = fun ~reject:_ ~accept s _ -> accept a s }

  (*
    -------------
    STATE AND ENV
    -------------
  *)
  let read : (State.t * Read.t) m =
    { run = fun ~reject:_ ~accept s r -> accept (s, r) s }

  let read_env : Env.t m =
    let%bind (_, { env ; _ }) = read in
    return env

  let[@inline always][@specialise] modify (f : State.t -> State.t) : unit m =
    { run =
      fun ~reject:_ ~accept s _ ->
        accept () (f s)
    }

  let[@inline always][@specialise] local (f : Read.t -> Read.t) (x : 'a m) : 'a m =
    { run = fun ~reject ~accept s r -> x.run ~reject ~accept s (f r) }

  (*
    ------
    RESULT
    ------
  *)
  let[@inline always][@specialise] fail (e : Err.t) : 'a m =
    { run = fun ~reject ~accept:_ s _ -> reject e s }

  let fail_map f = 
    let%bind (state, _) : State.t * Read.t = read in
    fail @@ f state

  (*
    -------
    EXITING
    -------
  *)
  let run (x : 'a m) (init_state : State.t) (init_read : Read.t) : ('a, Err.t) result * State.t =
    x.run ~reject:(fun e s -> Error e, s) ~accept:(fun a s -> Ok a, s) init_state init_read

  (*
    --------------------
    INTERPRETATION STUFF
    --------------------
  *)
  let[@inline always][@specialise] local_env (f : Env.t -> Env.t) (x : 'a m) : 'a m =
    { run = fun ~reject ~accept s r -> x.run ~reject ~accept s { r with env = f r.env } }

  let[@inline always][@specialise] with_incr_depth (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth =
      match r.det_depth with
      | `Escaped -> `Escaped
      | `Depth i -> `Depth (i + 1)
      }
    ) x

  let[@inline always][@specialise] with_escaped_det (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth = `Escaped}) x

  let assert_nondeterminism : unit m =
    let%bind (_, e) = read in
    if Read.is_determinism_allowed e
    then return ()
    else fail_map Err.fail_on_nondeterminism_misuse

  let[@inline always] fetch (id : Ident.t) : Value.t m =
    let%bind env = read_env in
    match Env.fetch id env with
    | None -> fail_map @@ Err.fail_on_fetch id
    | Some v -> return v
end

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
    ; rev_inputs : Input.t list }

  let empty : t =
    { step = 0
    ; path = Path.empty
    ; targets = []
    ; rev_inputs = [] }

  let inputs ({ rev_inputs ; _ } : t) : Input.t list =
    List.rev rev_inputs
end

module Read = struct
  include Status.Eval
  let fail_on_nondeterminism_misuse (s : State.t) : t =
    Status.Found_abort (State.inputs s, "Nondeterminism used when not allowed.")
  let fail_on_fetch (id : Ident.t) (s : State.t) : t =
    Status.Unbound_variable (State.inputs s, id)
end

module M = struct
  include Make (State) (Value.Env) (Read)

  let abort (msg : string) : 'a m =
    let%bind s, _ = read in
    fail (Status.Found_abort (State.inputs s, msg))

  let type_mismatch (msg : string) : 'a m =
    let%bind s, _ = read in
    fail (Status.Type_mismatch (State.inputs s, msg))

  (*
    For efficiency, we don't use a writer but instead thread a state of accumulated targets.
  *)
  let[@inline always][@specialise] tell (targets : Target.t list) : unit m =
    modify (fun s -> { s with targets = targets @ s.targets })
end

module type S = sig
  type 'a m = 'a M.m
  val diverge : 'a m
  val incr_step : int m
  val hit_branch : bool Direction.t -> bool Expression.t -> unit m
  val hit_case : int Direction.t -> int Expression.t -> other_cases:int list -> unit m
  val get_input : 'a Stepkey.t -> Value.t m
  val run : 'a m -> Status.Eval.t * Target.t list
end

module Initialize (C : sig val c : Consts.t end) (*: S*) = struct
  let max_step = C.c.options.global_max_step
  let max_depth = C.c.options.max_tree_depth
  let input_feeder = C.c.input_feeder
  let target = C.c.target

  type 'a m = 'a M.m

  open M

  let diverge : 'a m =
    let%bind s, _ = read in
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
    let%bind s, _ = read in
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

  let get_input (type a) (key : a Stepkey.t) : Value.t m =
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
      Status.Finished { pruned = Path.length s.path > max_depth || s.step > max_step }, s.targets
    | Error e, s -> e, s.targets
end
