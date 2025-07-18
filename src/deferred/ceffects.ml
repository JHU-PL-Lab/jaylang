
(*
  Concolic effects for deferred interpreter,
  interlaced with regular deferred effects.
*)


(*
  Deferred effects
*)

open Core
open Interp_common

(* monad to handle all of the effects *)

(* FIXME: should use time key *)
module Feeder = Interp_common.Input_feeder.Make (Interp_common.Step)

module V = Value.Make (Concolic.Value.Concolic_value)

(*
  FIXME: this is a total hack and should not be used even short term.
*)
let time_to_step (type a) (timekey : a Interp_common.Key.Timekey.t) : a Interp_common.Key.Stepkey.t =
  match timekey with
  | I t ->
    let s = Interp_common.Step.Step (Hashtbl.hash t) in
    Interp_common.Key.Stepkey.int_ s
  | B t ->
    let s = Interp_common.Step.Step (Hashtbl.hash t) in
    Interp_common.Key.Stepkey.bool_ s

module Env = struct
  type value = V.t

  (* stuff to read from *)
  type t =
    { feeder : Feeder.t
    ; env    : V.env }

  let empty : t =
    { feeder = Feeder.zero
    ; env = V.Env.empty }

  let fetch : Lang.Ast.Ident.t -> t -> V.t option =
    fun id e ->
      V.Env.fetch id e.env
end

module State = struct
  (* This is getting a little long and will be expensive on every update
    to copy all the fields.
    Real effects are starting to seem tempting.
    Or we could group the fields into those that are more/less likely to change. *)
  type t =
    { time : Timestamp.t
    ; symbol_env : V.Symbol_map.t
    ; pending_proofs : V.Pending_proofs.t 
    ; n_stern_steps : Step.t
    ; path : Concolic.Path.t
    ; targets : Concolic.Target.t list
    ; inputs : Interpreter.Interp.Input_log.t }

  let empty : t =
    { time = Timestamp.initial
    ; symbol_env = V.Symbol_map.empty
    ; pending_proofs = V.Pending_proofs.empty
    ; n_stern_steps = Step.zero 
    ; path = Concolic.Path.empty
    ; targets = []
    ; inputs = [] }

  let remove_greater_symbols (s : t) : t =
    { s with
      symbol_env = V.Symbol_map.cut (VSymbol s.time) s.symbol_env
    ; pending_proofs = V.Pending_proofs.cut (VSymbol s.time) s.pending_proofs }

  let incr_stern_step (s : t) : t =
    { s with n_stern_steps = Step.next s.n_stern_steps }

  let inputs ({ inputs ; _ } : t) : Interp_common.Input.t list =
    List.sort inputs ~compare:(fun (_, t1) (_, t2) -> Interp_common.Timestamp.compare t1 t2)
    |> List.map ~f:Tuple2.get1
end

module Err = struct
  include Concolic.Status.Eval
  let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
    Concolic.Status.Found_abort (State.inputs s, "Nondeterminism used when not allowed."), s
  let fail_on_fetch (id : Lang.Ast.Ident.t) (s : State.t) : t * State.t =
    Concolic.Status.Unbound_variable (State.inputs s, id), s
  let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
    Concolic.Status.Finished { pruned = true }, s
end

module M = struct
  include Interp_common.Effects.Make (State) (Env) (Err)

  (*
    -------
    RUNNING
    -------
  *)

  (* let run_on_empty (x : 'a s) (feeder : Feeder.t) : 'a * State.t * Interp_common.Step.t =
    run_safe
      x
      State.empty
      { env = { Env.empty with feeder } ; det_depth = `Depth 0 } *)

  (*
    -----------
    ENVIRONMENT
    -----------
  *)

  let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : V.t) (x : ('a, 'e) t) : ('a, 'e) t =
    local (fun e -> { e with env = V.Env.add id v e.env }) x

  let[@inline always] local_env (f : V.env -> V.env) (x : ('a, 'e) t) : ('a, 'e) t =
    local (fun e -> { e with env = f e.env }) x

  (*
    -----
    STATE
    -----
  *)

  let push_deferred_proof (symb : V.symb) (work : V.closure) : (unit, 'e) t =
    modify (fun s -> { s with pending_proofs = V.Pending_proofs.push symb work s.pending_proofs })

  let pop_deferred_proof (symb : V.symb) : (V.closure, 'e) t =
    let%bind s = get in
    match V.Pending_proofs.pop symb s.pending_proofs with
    | Some (closure, pending) ->
      let%bind () = modify (fun s -> { s with pending_proofs = pending }) in
      return closure
    | None -> failwith "no deferred proof for given symbol" (* only happens if there is an implementation bug *)

  let remove_greater_symbols : (unit, 'e) t =
    modify State.remove_greater_symbols


  (*
    This is meant to be equivalent to

      let%bind s = get in
      let t = s.time in
      let%bind () = modify (fun s -> { s with time }) in
      let%bind a = x in
      let%bind () = modify (fun s -> { s with time = t }) in
      return a

    I sure hope it is.
  *)
  let local_time (time : Timestamp.t) (x : ('a, 'e) t) : ('a, 'e) t =
    { run = fun ~reject ~accept state step env ->
      x.run ~reject ~accept:(fun a s step ->
        accept a { s with time = state.time } step
      ) { state with time } step env
    }

  (*
    TODO: make this filtering much better
  *)
  let[@inline always] run_on_deferred_proof (symb : V.symb) (f : Lang.Ast.Embedded.t -> ('a, 'e) t) : ('a, 'e) t =
    let%bind closure = pop_deferred_proof symb in
    let%bind s = get in
    let VSymbol t = symb in
    let to_keep, _, to_add_back = Time_map.split t s.pending_proofs in
    let%bind () = modify (fun s -> { s with pending_proofs = to_keep }) in
    local_time (V.timestamp_of_symbol symb) (
      let%bind v = local (fun e -> { e with env = closure.env }) (f closure.body) in
      let%bind () = modify (fun s -> { s with pending_proofs = Time_map.union (fun _ _ _ -> failwith "unexpected duplicate") s.pending_proofs to_add_back }) in
      return v
  )

  let incr_time : unit m =
    modify (fun s -> { s with time = Timestamp.increment s.time })

  let push_time : unit m =
    modify (fun s -> { s with time = Timestamp.push s.time })

  (*
    We must count stern steps instead of using total step count to
    periodically decide to work on a deferred proof in case there is
    some pattern to the step count during stern eval.

    e.g. if we work on a deferred proof when step mod 10 is 0, but
      the step count is always odd at stern eval (just due to the nature
      of the program at hand), then we'd never work on a deferred proof.
  *)
  let incr_n_stern_steps : unit m =
    modify State.incr_stern_step

  let should_work_on_deferred : bool m =
    let%bind s = get in
    return (Step.to_int s.n_stern_steps land 31 = 0) (* quick way to check is 0 mod 32 -- works on deferred proof every 32nd stern eval *)

  (*
    ------
    RESULT
    ------
  *)

  let fail_and_filter (err : State.t -> Err.t) : 'a m =
    let%bind () = remove_greater_symbols in
    { run = fun ~reject ~accept:_ state step _ -> reject (err state) state step }

  (* timestamp payload on error is just for printing. It is not used in tracking at all *)
  let abort (msg : string) : 'a m =
    fail_and_filter (fun s -> Concolic.Status.Found_abort (State.inputs s, msg))

  let type_mismatch (msg : string) : 'a m =
    fail_and_filter (fun s -> Concolic.Status.Type_mismatch (State.inputs s, msg))

  (*
    ---------------
    DEFERRED VALUES
    ---------------
  *)

  let lookup (V.VSymbol t : V.symb) : V.whnf option m =
    { run = fun ~reject:_ ~accept state step _ -> accept (Time_map.find_opt t state.symbol_env) state step }

  (*
    For efficiency, we don't use a writer but instead thread a state of accumulated targets.
  *)
  let[@inline always][@specialise] tell (targets : Concolic.Target.t list) : unit m =
    modify (fun s -> { s with targets = targets @ s.targets })
end

(*
  Concolic stuff
*)

module Consts = struct
  type t =
    { target : Concolic.Target.t
    ; options : Concolic.Options.t
    ; input_feeder : Feeder.t }
end

module type S = sig
  type 'a m = 'a M.m
  val vanish : 'a m
  val incr_step : unit m
  val hit_branch : bool Concolic.Direction.t -> bool Concolic.Expression.t -> unit m
  val hit_case : int Concolic.Direction.t -> int Concolic.Expression.t -> other_cases:int list -> unit m
  val get_input : 'a Feeder.Key.t -> V.whnf m
  val run : 'a m -> Concolic.Status.Eval.t * Concolic.Target.t list
end

module Initialize (C : sig val c : Consts.t end) (*: S*) = struct
  let max_step = C.c.options.global_max_step
  let max_depth = C.c.options.max_tree_depth
  let input_feeder = C.c.input_feeder
  let target = C.c.target

  type 'a m = 'a M.m

  open M

  let incr_step : unit m = incr_step ~max_step (* comes from M *)

  let vanish : 'a m =
    let%bind Step n = step in
    fail_and_filter (fun s ->
      Concolic.Status.Finished { pruned = Concolic.Path.length s.path > max_depth || n > max_step } 
    )

  let push_branch_and_tell (type a) (dir : a Concolic.Direction.t) (e : a Concolic.Expression.t) 
      (make_tape : a Concolic.Claim.t -> Concolic.Path.t -> Concolic.Target.t list) : unit m =
    if Concolic.Expression.is_const e then return () else
    let%bind s = get in
    let n = Concolic.Path.length s.path in
    if n >= max_depth then return () else
    let claim = Concolic.Claim.Equality (e, dir) in
    let%bind () = modify (fun s' -> { s' with path = Concolic.Path.cons (Concolic.Claim.to_expression claim) s'.path }) in
    if n < Concolic.Target.path_n target
    then return ()
    else tell (make_tape claim s.path)

  let hit_branch (dir : bool Concolic.Direction.t) (e : bool Concolic.Expression.t) : unit m =
    push_branch_and_tell dir e (fun claim path ->
      [ Concolic.Target.make
        (Concolic.Path.cons (Concolic.Claim.to_expression (Concolic.Claim.flip claim)) path)
      ]
    )

  let hit_case (dir : int Concolic.Direction.t) (e : int Concolic.Expression.t) ~(other_cases : int list) : unit m =
    push_branch_and_tell dir e (fun _ path ->
      let other_dirs =
        match dir with
        | Case_default { not_in } -> List.map not_in ~f:Concolic.Direction.of_int
        | Case_int i -> Case_default { not_in = i :: other_cases } :: List.map other_cases ~f:Concolic.Direction.of_int
      in
      List.map other_dirs ~f:(fun d -> 
        Concolic.Target.make
          (Concolic.Path.cons (Concolic.Claim.to_expression (Concolic.Claim.Equality (e, d))) path)
      )
    )

  (* FIXME: symbols use hash, not actual time (which should use some uid or perfect hash). *)
  let get_input (type a) (make_key : Timestamp.t -> a Interp_common.Key.Timekey.t) : V.t m =
    let%bind () = assert_nondeterminism in
    let%bind state = get in
    let key = time_to_step (make_key state.time) in
    let v = input_feeder.get key in
    match key with
    | I _ -> 
      let%bind () = modify (fun s -> { s with inputs = (I v, state.time) :: s.inputs ; time = Timestamp.increment s.time }) in
      return @@ V.VInt (v, Concolic.Expression.key key)
    | B _ ->
      let%bind () = modify (fun s -> { s with inputs = (B v, state.time) :: s.inputs ; time = Timestamp.increment s.time }) in
      return @@ V.VBool (v, Concolic.Expression.key key)

  let run (x : 'a m) : Concolic.Status.Eval.t * Concolic.Target.t list =
    match run x State.empty Read.empty with
    | Ok _, state, Step step ->
      Concolic.Status.Finished { pruned = Concolic.Path.length state.path >= max_depth || step > max_step }, state.targets
    | Error e, state, _ -> e, state.targets
end
