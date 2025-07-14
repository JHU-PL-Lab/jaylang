
open Core
open Interp_common

(* monad to handle all of the effects *)

module Feeder = Input_feeder.Using_timekey

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
    ; symbol_env : Symbol_map.t
    ; pending_proofs : Pending_proofs.t } 
  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)

  let empty : t =
    { time = Timestamp.empty
    ; symbol_env = Symbol_map.empty
    ; pending_proofs = Pending_proofs.empty }

  let remove_greater_symbols (s : t) : t =
    { s with
      symbol_env = Symbol_map.cut (VSymbol s.time) s.symbol_env
    ; pending_proofs = Pending_proofs.cut (VSymbol s.time) s.pending_proofs }
end

include Interp_common.Effects.Make (State) (Env) (struct
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
  run_safe
    x
    State.empty
    { env = { Env.empty with feeder } ; det_depth = `Depth 0 }

(*
  -----------
  ENVIRONMENT
  -----------
*)

let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Value.t) (x : ('a, 'e) t) : ('a, 'e) t =
  local (fun e -> { e with env = Value.Env.add id v e.env }) x

let[@inline always] local_env (f : Value.env -> Value.env) (x : ('a, 'e) t) : ('a, 'e) t =
  local (fun e -> { e with env = f e.env }) x

let get_input (type a) (make_key : Timestamp.t -> a Feeder.Timekey.t) : (Value.t, 'e) t =
  let%bind () = assert_nondeterminism in
  { run = fun ~reject:_ ~accept state step e -> 
    accept (
      let key = make_key state.time in
      let v = e.env.feeder.get key in
      match key with
      | I _ -> Value.VInt v
      | B _ -> Value.VBool v
    ) { state with time = Timestamp.increment state.time } step
  }

(*
  -----
  STATE
  -----
*)

let push_deferred_proof (symb : Value.symb) (work : Value.closure) : (unit, 'e) t =
  modify (fun s -> { s with pending_proofs = Pending_proofs.push symb work s.pending_proofs })

let pop_deferred_proof (symb : Value.symb) : (Value.closure, 'e) t =
  let%bind s = get in
  match Pending_proofs.pop symb s.pending_proofs with
  | Some (closure, pending) ->
    let%bind () = modify (fun s -> { s with pending_proofs = pending }) in
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
  TODO: filter the maps to only contain smaller symbols
*)
let[@inline always] run_on_deferred_proof (symb : Value.symb) (f : Lang.Ast.Embedded.t -> ('a, 'e) t) : ('a, 'e) t =
  let%bind closure = pop_deferred_proof symb in
  local_time (Value.timestamp_of_symbol symb) (
    local (fun e -> { e with env = closure.env }) (f closure.body)
  )

let incr_time : unit m =
  modify (fun s -> { s with time = Timestamp.increment s.time })

let push_time : unit m =
  modify (fun s -> { s with time = Timestamp.push s.time })

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
