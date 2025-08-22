
(*
  Concolic effects for deferred interpreter, interlaced with regular deferred effects.
*)

open Core
open Interp_common
open Concolic_common

(* monad to handle all of the effects *)

type k = Interp_common.Timestamp.t

module Feeder = Input_feeder.Make (Timestamp)

module State = struct
  (* This is getting a little long and will be expensive on every update
    to copy all the fields.
    Real effects are starting to seem tempting.
    Or we could group the fields into those that are more/less likely to change. *)
  (*
    The symbol maps could go together (but don't have a _great_ reason to), and
    the input log could be an add-on only when logging is needed (it's sort of just
    a debugging thing).
  *)
  type t =
    { time : Timestamp.t
    ; symbol_env : Value.Symbol_map.t
    ; pending_proofs : Value.Pending_proofs.t 
    ; n_stern_steps : Step.t
    ; path : k Path.t
    ; inputs : Interpreter.Interp.Input_log.t }

  let empty : t =
    { time = Timestamp.initial
    ; symbol_env = Value.Symbol_map.empty
    ; pending_proofs = Value.Pending_proofs.empty
    ; n_stern_steps = Step.zero 
    ; path = Path.empty
    ; inputs = [] }

  let remove_greater_symbols (s : t) : t =
    { s with
      symbol_env = Value.Symbol_map.cut (VSymbol s.time) s.symbol_env
    ; pending_proofs = Value.Pending_proofs.cut (VSymbol s.time) s.pending_proofs }

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

include Interp_common.Effects.Make (State) (Utils.Builder.Unit_builder) (Value.Env) (Err)

(*
  -----------
  ENVIRONMENT
  -----------
*)

let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Value.t) (x : ('a, 'e) t) : ('a, 'e) t =
  local (Value.Env.add id v) x

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
  { run = fun ~reject ~accept state step () r ->
    x.run ~reject ~accept:(fun a s step () ->
      accept a { s with time = state.time } step ()
    ) { state with time } step () r
  }

(*
  TODO: make this filtering much better. We currently actually split the map and union in back,
    but we should be able to add some filters on it that just pretend to split the map, and then
    we remove the filter instead of union.

  Maps the deferred proof for the given symbol and moves it from a pending proof into the symbol environment.
*)
let[@inline always] map_deferred_proof (VSymbol t as symb : Value.symb) (f : Lang.Ast.Embedded.t -> (Value.whnf, 'e) t) : (Value.whnf, 'e) t =
  { run = fun ~reject ~accept state step () _ ->
    (* Get the deferred proof for the symbol from the current state. *)
    match Value.Pending_proofs.pop symb state.pending_proofs with   
    | None -> failwith "Invariant failure: popping symbol that does not exist in the symbol map"
    | Some (closure, depth, remaining_pending_proofs) ->
      (* When we go to work on a deferred proof, we only let it see the lesser symbols *)
      let to_keep, _, to_add_back = Time_map.split t remaining_pending_proofs in
      (* We will locally run with the time from the symbol and only the lesser pending proofs. *)
      (f closure.body).run 
        { state with time = t ; pending_proofs = to_keep } 
        step
        ()
        { env = closure.env ; det_depth = depth } (* locally uses det depth from when symbol was pushed *)
        ~reject ~accept:(fun v final_state final_step () ->
          accept v { final_state with
            time = state.time (* Restore original time now that f is done. *)
          ; pending_proofs = 
            Time_map.union (fun _ _ _ -> failwith "Invariant failure: duplicate timestamp when adding back hidden symbols") 
              final_state.pending_proofs (* Keep all the proofs after f finished running ... *)
              to_add_back (* ... and put back the proofs we hid from f *)
          ; symbol_env = Time_map.add t v final_state.symbol_env
          } final_step ()
        ) 
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

(*
  Maps an arbitrary deferred proof dictated by the number of stern steps.
  We use bitwise arithmetic to work on a deferred proof every 32nd stern step.

  Uses the monad structure to partially eval a few binds.
*)
let[@inline always] optionally_map_some_deferred_proof (f : Lang.Ast.Embedded.t -> (Value.whnf, 'e) t) : (unit, 'e) t =
  { run = fun ~reject ~accept state step () r ->
    if Step.to_int state.n_stern_steps land 31 = 0 (* quick way to check it is 0 mod 32 *)
    && not (Time_map.is_empty state.pending_proofs) (* ... and there is some pending proof we can work on *)
    then 
      let (t, _) = Time_map.choose state.pending_proofs in
      (map_deferred_proof (VSymbol t) f).run ~reject ~accept:(fun _ final_state final_step () ->
        accept () final_state final_step ()
      ) state step () r
    else
      accept () state step ()
  }

(*
  ------
  RESULT
  ------
*)

let fail_and_filter (err : State.t -> Err.t) : 'a m =
  { run = fun ~reject ~accept:_ state step () _ -> reject (err state) (State.remove_greater_symbols state) step () }

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

let lookup (Value.VSymbol t : Value.symb) : Value.whnf option m =
  { run = fun ~reject:_ ~accept state step () _ -> accept (Time_map.find_opt t state.symbol_env) state step () }

let vanish : 'a m =
  fail_and_filter (fun _ -> Status.Finished)

let push_branch (dir : k Direction.t) : unit m =
  if Smt.Formula.is_const @@ Direction.to_formula dir
  then return ()
  else modify (fun s -> { s with path = Path.cons dir s.path })

let[@inline always] defer (body : Lang.Ast.Embedded.t) : Value.t m =
  { run =
    fun ~reject:_ ~accept state step () r ->
      let symb = Value.VSymbol (Interp_common.Timestamp.push state.time) in
      accept (Value.cast_up symb) { state with 
        time = Interp_common.Timestamp.increment state.time
      ; pending_proofs = Value.Pending_proofs.push symb { body ; env = r.env } r.det_depth state.pending_proofs 
      } step ()
  }

let get_input (type a) (make_key : Timestamp.t -> a Key.Timekey.t) (feeder : Timestamp.t Input_feeder.t) : Value.t m =
  let%bind () = assert_nondeterminism in
  let%bind state = get in
  let key = make_key state.time in
  let v = feeder.get key in
  match key with
  | I k -> 
    let%bind () = modify (fun s -> { s with inputs = (I v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ Value.symbolic_int v k
  | B k ->
    let%bind () = modify (fun s -> { s with inputs = (B v, s.time) :: s.inputs ; time = Timestamp.increment s.time }) in
    return @@ Value.symbolic_bool v k

let run (x : 'a m) : 'a option * Value.Symbol_map.t * Status.Eval.t * k Path.t =
  match run x State.empty Read.empty with
  | Ok a, state, _, () ->
    Some a, state.symbol_env, Status.Finished, state.path
  | Error e, state, _, () -> None, state.symbol_env, e, state.path
