
open Core
open Interp_common

(* monad to handle all of the effects *)

module Feeder = Input_feeder.Make (Timestamp)

(*
  It can be a little confusing here because the thing we
  read from is not just the interpreter environment but
  also some other stuff. So note that `Env` is not only
  the interpreter's environment.
*)
module Env = struct
  type value = Value.t

  (* stuff to read from *)
  type t =
    { feeder : Feeder.t
    ; env    : Value.env }

  let empty : t =
    { feeder = Feeder.zero
    ; env = Value.Env.empty }

  let fetch : Lang.Ast.Ident.t -> t -> Value.t option =
    fun id e ->
      Value.Env.fetch id e.env
end

(*
  For some reason I don't know, Core.Map likes to take linear time to compute
  the size of a map. Who cares.

  Baby, on the other hand, offers an O(log n) cut operation, as opposed to
  O(m + log n) where m is the size of the resulting map, offered from Core.

  This is why we use Baby for the maps (see Time_map).
*)
module State = struct
  type t =
    { time : Timestamp.t
    ; symbol_env : Value.Symbol_map.t
    ; pending_proofs : Value.Pending_proofs.t 
    ; n_stern_steps : Step.t } 
  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)

  let empty : t =
    { time = Timestamp.initial
    ; symbol_env = Value.Symbol_map.empty
    ; pending_proofs = Value.Pending_proofs.empty
    ; n_stern_steps = Step.zero }

  let remove_greater_symbols (s : t) : t =
    { s with
      symbol_env = Value.Symbol_map.cut (VSymbol s.time) s.symbol_env
    ; pending_proofs = Value.Pending_proofs.cut (VSymbol s.time) s.pending_proofs }

  let incr_stern_step (s : t) : t =
    { s with n_stern_steps = Step.next s.n_stern_steps }
end

include Interp_common.Effects.Make (State) (Utils.Builder.Unit_builder) (Env) (struct
  include Err

  let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
    `XAbort { msg = "Nondeterminism used when not allowed." ; body = s.time }, State.remove_greater_symbols s

  let fail_on_fetch (id : Lang.Ast.Ident.t) (s : State.t) : t * State.t =
    `XUnbound_variable (id, s.time), State.remove_greater_symbols s

  let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
    `XReach_max_step s.time, s
end)

(*
  -------
  RUNNING
  -------
*)

let run_on_empty (x : 'a s) (feeder : Feeder.t) : 'a * State.t * Interp_common.Step.t =
  let a, state, step, () =
    run_safe
      x
      State.empty
      { env = { Env.empty with feeder } ; det_depth = Interp_common.Det_depth.zero }
  in
  a, state, step

(*
  -----------
  ENVIRONMENT
  -----------
*)

let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Value.t) (x : ('a, 'e) t) : ('a, 'e) t =
  local (fun e -> { e with env = Value.Env.add id v e.env }) x

let[@inline always] local_env (f : Value.env -> Value.env) (x : ('a, 'e) t) : ('a, 'e) t =
  local (fun e -> { e with env = f e.env }) x

let get_input (type a) (make_key : Timestamp.t -> a Feeder.Key.t) : (Value.t, 'e) t =
  let%bind () = assert_nondeterminism in
  { run = fun ~reject:_ ~accept state step () e -> 
    accept (
      let key = make_key state.time in
      let v = e.env.feeder.get key in
      match key with
      | I _ -> Value.VInt v
      | B _ -> Value.VBool v
    ) { state with time = Timestamp.increment state.time } step ()
  }

(*
  -----
  STATE
  -----
*)

let push_deferred_proof (symb : Value.symb) (work : Value.closure) : (unit, 'e) t =
  let%bind r = read in
  modify (fun s -> { s with pending_proofs = Value.Pending_proofs.push symb work r.det_depth s.pending_proofs })

let pop_deferred_proof (symb : Value.symb) : (Value.closure, 'e) t =
  let%bind s = get in
  match Value.Pending_proofs.pop symb s.pending_proofs with
  | Some (closure, _depth, pending) ->
    let%bind () = modify (fun s -> { s with pending_proofs = pending }) in (* FIXME: need to set depth and filter out greater proofs *)
    return closure
  | None -> failwith "no deferred proof for given symbol" (* only happens if there is an implementation bug *)

let remove_greater_symbols : (unit, 'e) t =
  modify State.remove_greater_symbols

let local_time (time : Timestamp.t) (x : ('a, 'e) t) : ('a, 'e) t =
  let%bind s = get in
  let t = s.time in
  let%bind () = modify (fun s -> { s with time }) in
  let%bind a = x in
  let%bind () = modify (fun s -> { s with time = t }) in
  return a

(*
  TODO: make this filtering much better
*)
let[@inline always] run_on_deferred_proof (symb : Value.symb) (f : Lang.Ast.Embedded.t -> ('a, 'e) t) : ('a, 'e) t =
  let%bind closure = pop_deferred_proof symb in
  let%bind s = get in
  let VSymbol t = symb in
  let to_keep, _, to_add_back = Time_map.split t s.pending_proofs in
  let%bind () = modify (fun s -> { s with pending_proofs = to_keep }) in
  local_time (Value.timestamp_of_symbol symb) (
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

let fail_at_time (err : Timestamp.t -> Err.t) : 'a m =
  let%bind () = remove_greater_symbols in
  { run = fun ~reject ~accept:_ state step () _ -> reject (err state.time) state step () }

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
  { run = fun ~reject:_ ~accept state step () _ -> accept (Time_map.find_opt t state.symbol_env) state step () }
