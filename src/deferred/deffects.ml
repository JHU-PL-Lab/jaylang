(* 

open Core
open Interp_common

(* monad to handle all of the effects *)

(*
  I started writing this using the original deferred effects.
  But the concolic one is a bit more polished. I should take
  the concolic one, abstract out the stuff it needs that is on
  top of the original, and then use the underlying with nothing
  added for the original.
  
  But it works over different kinds of values, and the state
  has extra stuff.

  But if the concolic evaluator is only expected to produce a
  concolic path given an input feeder, then maybe we don't need
  so much.

  I probably want to track inputs given (no matter what because
  it's nice to see all that the interpreter gave when my input
  sequence is incomplete). So input tracking should be part of
  common interp monad.

  Then the only thing the concolic tracks more than the standard
  is it has a path. And the values are different, but that is
  not exactly tracking, and the monad shouldn't care that much
  besides having stuff that relates (mostly parametrically) to
  those values.

  And it has a few parameters and constants to help with reporting.

  All interpreters need an input feeder, so that should be part of
  the environment or should be fed into the input function. I think
  that last idea is best. It's literally only used at one function.
  let it be stored in a closure and passed in.

  Max step can be passed in too. Max depth matters when computing
  targets, so it can be known only to the concolic loop.
  This means I can ditch the Initialize thing.

  If interp common knows the key type (which it does because it knows
  the input feeder, which uses a key), then it can have a way to return
  ints and bools during the input function: the concolic one actually
  uses the key to make a formula, and the standard just discards the key.

  So a path should not be a list of formulas because there is no way to
  negate an int direction formula. I can just make the a list of directions,
  and targets are formulas (they can even be the And of formulas, so just one).
*)

module Feeder = Input_feeder.Make (Timestamp)

module type ENV = sig
  include Interp_common.Effects.ENV
  val symbol : Timestamp.t -> value
  val add : Lang.Ast.Ident.t -> value -> t -> t
end

module type STATE = sig
  type t

  val empty : t

  val time : t -> Timestamp.t
  val symbol_env : t -> Value.Symbol_map.t
  val pending_proofs : t -> Value.Pending_proofs.t
  val n_stern_steps : t -> Step.t

  (*
    This will be inefficient when more than one thing needs to be inserted.
  *)
  val with_time : Timestamp.t -> t -> t
  val with_symbol_env : Value.Symbol_map.t -> t -> t
  val with_pending_proofs : Value.Pending_proofs.t -> t -> t
  val with_stern_step : Step.t -> t -> t
end

(*
  Maybe because this is default, this should always be provided.
*)
module Default_state : STATE = struct
  type t =
    { time : Timestamp.t
    ; symbol_env : Value.Symbol_map.t
    ; pending_proofs : Value.Pending_proofs.t 
    ; n_stern_steps : Step.t } 
    [@@deriving fields ~getters]
  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)

  let empty : t =
    { time = Timestamp.initial
    ; symbol_env = Value.Symbol_map.empty
    ; pending_proofs = Value.Pending_proofs.empty
    ; n_stern_steps = Step.zero }

  let with_time time t = { t with time }
  let with_symbol_env symbol_env t = { t with symbol_env }
  let with_pending_proofs pending_proofs t = { t with pending_proofs }
  let with_stern_step n_stern_steps t = { t with n_stern_steps }
end

module Make (E : ENV) (S : STATE) = struct
  module Env = struct
    type value = E.value

    (* stuff to read from *)
    type t =
      { feeder : Feeder.t
      ; env    : E.t }

    let empty : t =
      { feeder = Feeder.zero
      ; env = E.empty }

    let fetch : Lang.Ast.Ident.t -> t -> value option =
      fun id e ->
        E.fetch id e.env

    let add : Lang.Ast.Ident.t -> value -> t -> t =
      fun id v e ->
        { e with env = E.add id v e.env }
  end

  module State = struct
    include S

    let map_time f t = S.with_time (f (S.time t)) t
    let map_symbol_env f t = S.with_symbol_env (f (S.symbol_env t)) t
    let map_pending_proofs f t = S.with_pending_proofs (f (S.pending_proofs t)) t
    let map_stern_step f t = S.with_stern_step (f (S.n_stern_steps t)) t

    (* TODO: not force symbol maps to work on symbols but rather on timestamps *)
    let remove_greater_symbols (s : t) : t =
      let symb = VSymbol (S.time s) in
      s
      |> map_symbol_env (Value.Symbol_map.cut symb)
      |> map_pending_proofs (Value.Pending_proofs.cut symb)

    let incr_stern_step (s : t) : t =
      map_stern_step Step.next s
  end

  include Interp_common.Effects.Make (State) (Env) (struct
    include Err

    let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
      `XAbort { msg = "Nondeterminism used when not allowed." ; body = State.time s }, State.remove_greater_symbols s

    let fail_on_fetch (id : Lang.Ast.Ident.t) (s : State.t) : t * State.t =
      `XUnbound_variable (id, State.time s), State.remove_greater_symbols s

    let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
      `XReach_max_step (State.time s), s
  end)

  (*
    -------
    RUNNING
    -------
  *)

  let run_on_empty (x : 'a s) (feeder : Feeder.t) : 'a * State.t * Interp_common.Step.t =
    run_safe
      x
      State.empty
      { env = { Env.empty with feeder } ; det_depth = `Depth 0 }

  (*
    -----------
    ENVIRONMENT
    -----------
  *)

  let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Env.value) (x : ('a, 'e) t) : ('a, 'e) t =
    local (Env.add id v) x

  let[@inline always] local_env (f : E.t -> E.t) (x : ('a, 'e) t) : ('a, 'e) t =
    local (fun e -> { e with env = f e.env }) x

  (* FIXME: this should be value, and value should parametrize this module *)
  (* In fact, the value needs to have quite a lot of detail to work with state *)
  let get_input (type a) (make_key : Timestamp.t -> a Feeder.Key.t) : (Value.t, 'e) t =
    let%bind () = assert_nondeterminism in
    { run = fun ~reject:_ ~accept state step e -> 
      accept (
        let key = make_key (State.time state) in
        let v = e.env.feeder.get key in
        match key with
        | I _ -> Value.VInt v
        | B _ -> Value.VBool v
      ) (State.map_time Timestamp.increment state) step
    }

  (*
    -----
    STATE
    -----
  *)

  let push_deferred_proof (symb : Value.symb) (work : Value.closure) : (unit, 'e) t =
    modify (State.map_pending_proofs (Value.Pending_proofs.push symb work))

  let pop_deferred_proof (symb : Value.symb) : (Value.closure, 'e) t =
    let%bind s = get in
    match Value.Pending_proofs.pop symb (State.pending_proofs s) with
    | Some (closure, pending) ->
      let%bind () = modify (State.with_pending_proofs pending) in
      return closure
    | None -> failwith "no deferred proof for given symbol" (* only happens if there is an implementation bug *)

  let remove_greater_symbols : (unit, 'e) t =
    modify State.remove_greater_symbols

  let local_time (time : Timestamp.t) (x : ('a, 'e) t) : ('a, 'e) t =
    { run = fun ~reject ~accept state step env ->
      x.run ~reject ~accept:(fun a s step ->
        accept a (State.with_time (State.time state) s) step
      ) (State.with_time time state) step env
    }

  (*
    TODO: make this filtering much better
  *)
  let[@inline always] run_on_deferred_proof (symb : Value.symb) (f : Lang.Ast.Embedded.t -> ('a, 'e) t) : ('a, 'e) t =
    let%bind closure = pop_deferred_proof symb in
    let%bind s = get in
    let VSymbol t = symb in
    let to_keep, _, to_add_back = Time_map.split t (State.pending_proofs s) in
    let%bind () = modify (State.with_pending_proofs to_keep) in
    local_time (Value.timestamp_of_symbol symb) (
      let%bind v = local (fun e -> { e with env = closure.env }) (f closure.body) in
      let%bind () = modify (fun s -> 
        State.with_pending_proofs 
          (Time_map.union (fun _ _ _ -> failwith "unexpected duplicate") (State.pending_proofs s) to_add_back)
          s
      ) in
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

  let fail_at_time (err : Timestamp.t -> Err.t) : 'a m =
    let%bind () = remove_greater_symbols in
    { run = fun ~reject ~accept:_ state step _ -> reject (err state.time) state step }

  (* timestamp payload on error is just for printing. It is not used in tracking at all *)
  let abort (msg : string) : 'a m =
    fail_at_time (fun t -> `XAbort { msg ; body = t })

  let type_mismatch (msg : string) : 'a m =
    fail_at_time (fun t -> `XType_mismatch { msg ; body = t })

  let vanish : 'a m =
    fail_at_time (fun t -> `XVanish t)

  let unbound_variable (id : Lang.Ast.Ident.t) : 'a m =
    fail_at_time (fun t -> `XUnbound_variable (id, t))

  (*
    ---------------
    DEFERRED VALUES
    ---------------
  *)

  let lookup (Value.VSymbol t : Value.symb) : Value.whnf option m =
    { run = fun ~reject:_ ~accept state step _ -> accept (Time_map.find_opt t state.symbol_env) state step }
end *)
