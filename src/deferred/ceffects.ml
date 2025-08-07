
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
  let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
    Status.Reached_max_step, s
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
  { run = fun ~reject ~accept state step r ->
    x.run ~reject ~accept:(fun a s step ->
      accept a { s with time = state.time } step
    ) { state with time } step r
  }

(*
  TODO: make this filtering much better. We currently actually split the map and union in back,
    but we should be able to add some filters on it that just pretend to split the map, and then
    we remove the filter instead of union.

  Maps the deferred proof for the given symbol and moves it from a pending proof into the symbol environment.
*)
let[@inline always] map_deferred_proof (VSymbol t as symb : V.symb) (f : Lang.Ast.Embedded.t -> (V.whnf, 'e) t) : (V.whnf, 'e) t =
  { run = fun ~reject ~accept state step r ->
    (* Get the deferred proof for the symbol from the current state. *)
    match V.Pending_proofs.pop symb state.pending_proofs with   
    | None -> failwith "Invariant failure: popping symbol that does not exist in the symbol map"
    | Some (closure, remaining_pending_proofs) ->
      (* When we go to work on a deferred proof, we only let is see the lesser symbols *)
      let to_keep, _, to_add_back = Time_map.split t remaining_pending_proofs in
      (* We will locally run with the time from the symbol and only the lesser pending proofs. *)
      (f closure.body).run ~reject ~accept:(fun v final_state final_step ->
        accept v { final_state with
          time = state.time (* Restore original time now that f is done. *)
        ; pending_proofs = 
          Time_map.union (fun _ _ _ -> failwith "Invariant failure: duplicate timestamp when adding back hidden symbols") 
            final_state.pending_proofs (* Keep all the proofs after f finished running ... *)
            to_add_back (* ... and put back the proofs we hid from f *)
        ; symbol_env = Time_map.add t v final_state.symbol_env
        } final_step
      ) { state with time = t ; pending_proofs = to_keep } step { r with env = { r.env with env = closure.env } }
  }

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
  Maps an arbitrary deferred proof dictated by the number of stern steps.
  We use bitwise arithmetic to work on a deferred proof every 32nd stern step.

  Uses the monad structure to partially eval a few binds.
*)
let[@inline always] optionally_map_some_deferred_proof (f : Lang.Ast.Embedded.t -> (V.whnf, 'e) t) : (unit, 'e) t =
  { run = fun ~reject ~accept state step r ->
    if Step.to_int state.n_stern_steps land 31 = 0 (* quick way to check it is 0 mod 32 *)
    && not (Time_map.is_empty state.pending_proofs) (* ... and there is some pending proof we can work on *)
    then 
      let (t, _) = Time_map.choose state.pending_proofs in
      (map_deferred_proof (VSymbol t) f).run ~reject ~accept:(fun _ final_state final_step ->
        accept () final_state final_step
      ) state step r
    else
      accept () state step
  }

(*
  ------
  RESULT
  ------
*)

let fail_and_filter (err : State.t -> Err.t) : 'a m =
  { run = fun ~reject ~accept:_ state step _ -> reject (err state) (State.remove_greater_symbols state) step }

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
  fail_and_filter (fun _ -> Status.Finished)

let push_branch (dir : k Direction.t) : unit m =
  let is_const =
    match dir with
    | Bool_direction (_, expr) -> Smt.Formula.is_const expr
    | Int_direction { expr ; _ } -> Smt.Formula.is_const expr
  in
  if is_const
  then return ()
  else modify (fun s -> { s with path = Path.cons dir s.path })


let[@inline always] defer (body : Lang.Ast.Embedded.t) : V.t m =
  { run =
    fun ~reject:_ ~accept state step r ->
      let symb = V.VSymbol (Interp_common.Timestamp.push state.time) in
      accept (V.cast_up symb) { state with 
        time = Interp_common.Timestamp.increment state.time
      ; pending_proofs = V.Pending_proofs.push symb { body ; env = r.env.env } state.pending_proofs 
      } step
  }

module Time_symbol = Smt.Symbol.Make (Timestamp)

let get_input (type a) (make_key : Timestamp.t -> a Key.Timekey.t) (feeder : Timestamp.t Input_feeder.t) : V.t m =
  let%bind () = assert_nondeterminism in
  let%bind state = get in
  let key = make_key state.time in
  let v = feeder.get key in
  match key with
  | I k -> 
    let%bind () = modify (fun s -> { s with inputs = (I v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ V.VInt (v, Smt.Formula.symbol (Time_symbol.make_int k))
  | B k ->
    let%bind () = modify (fun s -> { s with inputs = (B v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ V.VBool (v, Smt.Formula.symbol (Time_symbol.make_bool k))

let run (x : 'a m) : Status.Eval.t * k Path.t =
  match run x State.empty Read.empty with
  | Ok _, state, _ ->
    Status.Finished, state.path
  | Error e, state, _ -> e, state.path
