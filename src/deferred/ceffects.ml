
(*
  Concolic effects for deferred interpreter,
  interlaced with regular deferred effects.
*)

(*
  Deferred effects.

  TODO: extract the common deferred effects and allow it to
    be instrumented with the concolic-specific effects.
*)

open Core
open Interp_common
open Concolic_common

(* monad to handle all of the effects *)

type k = Interp_common.Timestamp.t

module Feeder = Input_feeder.Make (Timestamp)

module CV = Concolic.Value.Make (Timestamp)

module V = Value.Make (CV.Concolic_value)

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
    ; path : k Path.t
    ; inputs : Interpreter.Interp.Input_log.t }

  let empty : t =
    { time = Timestamp.initial
    ; symbol_env = V.Symbol_map.empty
    ; pending_proofs = V.Pending_proofs.empty
    ; n_stern_steps = Step.zero 
    ; path = Path.empty
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
  include Status.Eval
  let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
    Status.Found_abort (State.inputs s, "Nondeterminism used when not allowed."), s
  let fail_on_fetch (id : Lang.Ast.Ident.t) (s : State.t) : t * State.t =
    Status.Unbound_variable (State.inputs s, id), s
  let fail_on_max_step (step : int) (s : State.t) : t * State.t =
    Status.Finished { final_step = Step step }, s
end

include Interp_common.Effects.Make (State) (Env) (Err)

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
  TODO: make this filtering much better. We currently actually split the map and union in back,
    but we should be able to add some filters on it that just pretend to split the map.
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
  fail_and_filter (fun s -> Status.Found_abort (State.inputs s, msg))

let type_mismatch (msg : string) : 'a m =
  fail_and_filter (fun s -> Status.Type_mismatch (State.inputs s, msg))

(*
  ---------------
  DEFERRED VALUES
  ---------------
*)

let lookup (V.VSymbol t : V.symb) : V.whnf option m =
  { run = fun ~reject:_ ~accept state step _ -> accept (Time_map.find_opt t state.symbol_env) state step }


let vanish : 'a m =
  let%bind final_step = step in
  fail_and_filter (fun _ -> Status.Finished { final_step })

let push_branch (dir : k Direction.t) : unit m =
  if match dir with
    | Bool_direction (_, expr) -> Overlays.Typed_smt.is_const expr
    | Int_direction { expr ; _ } -> Overlays.Typed_smt.is_const expr
  then return ()
  else modify (fun s -> { s with path = Path.cons dir s.path })

module Time_symbol = Overlays.Typed_smt.Make_symbol (Timestamp)

let get_input (type a) (make_key : Timestamp.t -> a Key.Timekey.t) (feeder : Timestamp.t Input_feeder.t) : V.t m =
  let%bind () = assert_nondeterminism in
  let%bind state = get in
  let key = make_key state.time in
  let v = feeder.get key in
  match key with
  | I k -> 
    let%bind () = modify (fun s -> { s with inputs = (I v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ V.VInt (v, Overlays.Typed_smt.symbol (Time_symbol.make_int k))
  | B k ->
    let%bind () = modify (fun s -> { s with inputs = (B v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ V.VBool (v, Overlays.Typed_smt.symbol (Time_symbol.make_bool k))

let run (x : 'a m) : Status.Eval.t * k Path.t =
  match run x State.empty Read.empty with
  | Ok _, state, final_step ->
    Status.Finished { final_step }, state.path
  | Error e, state, _ -> e, state.path
