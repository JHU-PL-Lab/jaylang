
open Core
open Lang.Ast
open Interp_common
open Concolic_common

type k = Step.t

module Feeder = Input_feeder.Make (Step)

module State = struct
  type t =
    { path : k Path.t
    ; rev_inputs : Input.t list }

  let empty : t =
    { path = Path.empty
    ; rev_inputs = [] }

  let inputs ({ rev_inputs ; _ } : t) : Input.t list =
    List.rev rev_inputs
end

module Err = struct
  include Status.Eval
  let fail_on_nondeterminism_misuse (s : State.t) : t * State.t =
    Status.Found_abort (State.inputs s, "Nondeterminism used when not allowed."), s
  let fail_on_fetch (id : Ident.t) (s : State.t) : t * State.t =
    Status.Unbound_variable (State.inputs s, id), s
  let fail_on_max_step (_step : int) (s : State.t) : t * State.t =
    Status.Reached_max_step, s
end

include Interp_common.Effects.Make (State) (Value.Env) (Err)

let abort (msg : string) : 'a m =
  let%bind s = get in
  fail (Status.Found_abort (State.inputs s, msg))

let type_mismatch (msg : string) : 'a m =
  let%bind s = get in
  fail (Status.Type_mismatch (State.inputs s, msg))

let vanish : 'a m =
  fail Status.Finished

let push_branch (dir : k Direction.t) : unit m =
  if match dir with
    | Bool_direction (_, expr) -> Smt.Formula.is_const expr
    | Int_direction { expr ; _ } -> Smt.Formula.is_const expr
  then return ()
  else modify (fun s -> { s with path = Path.cons dir s.path })

module Step_symbol = Smt.Symbol.Make (Step)

let get_input (type a) (make_key : Step.t -> a Feeder.Key.t) (feeder : Step.t Input_feeder.t) : Value.t m =
  let%bind () = assert_nondeterminism in
  let%bind s = step in
  let key = make_key s in
  let v = feeder.get key in
  match key with
  | I k -> 
    let%bind () = modify (fun s -> { s with rev_inputs = I v :: s.rev_inputs }) in
    return @@ Value.M.VInt (v, Smt.Formula.symbol (Step_symbol.make_int k))
  | B k ->
    let%bind () = modify (fun s -> { s with rev_inputs = B v :: s.rev_inputs }) in
    return @@ Value.M.VBool (v, Smt.Formula.symbol (Step_symbol.make_bool k))

let run (x : 'a m) : Status.Eval.t * k Path.t =
  match run x State.empty Read.empty with
  | Ok _, state, _ ->
    Status.Finished, state.path
  | Error e, state, _ -> e, state.path
