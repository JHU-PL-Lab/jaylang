(*
The implementation of this monad includes the following features, ordered from
outermost to innermost.

  * Metadata via writer (e.g. number of steps executed)
  * Suspension via coroutine
  * Stateful nondeterminism via functions and enumerations (for e.g. formulae)

Note that the stateful nondeterminism monad cannot be constructed using standard
transformers.  Using transformers, we could produce a nondeterministic set of
stateful but deterministic computations; we could also produce a stateful
computation with a nondeterministic answer (but deterministic state).  We want a
stateful computation whose nondeterminism includes its state.
*)

open Batteries;;
open Odefa_ast;;

open Ast;;
open Interpreter_types;;
(* open Relative_stack;; *)
open Sat_types;;

(* **** Supporting types **** *)

type decision_map =
  (Ident.t * clause * Ident.t) Symbol_map.t
;;

type state = {
  st_formulae : Formulae.t;
  st_decisions : decision_map;
};;

(* **** Initial values **** *)

let initial_state = {
  st_formulae = Formulae.empty;
  st_decisions = Symbol_map.empty;
};;

(* **** Monad types **** *)

type 'a m =
  | M of (state -> 'a result Enum.t) Enum.t

(** A result is either a completed expression (with its resulting state) or a
    suspended function which, when invoked, will step to the next result.  The
    suspended case carries the state of the computation at the time it was
    suspended, but this state value is for *reference only*. *)
and 'a result =
  | Completed : 'a * state -> 'a result
  | Suspended : (unit -> 'a result Enum.t) * state -> 'a result
;;

(* **** Monadic operations **** *)

let return (type a) (v : a) : a m =
  M(Enum.singleton(fun state -> Enum.singleton (Completed (v, state))))
;;

let bind (type a) (type b) (x : a m) (f : a -> b m) : b m =
  let rec bind_one_a_result (a_result : a result) =
    match a_result with
    | Completed((v : a),(state' : state)) ->
      let M(b_worlds) = f v in
      b_worlds
      |> Enum.map (fun b_world -> b_world state')
      |> Enum.concat
    | Suspended((thunk : (unit -> a result Enum.t)),
                (reference_state : state)) ->
      let thunk' : (unit -> b result Enum.t) =
        fun () ->
          let a_results = thunk () in
          let b_results = Enum.map bind_one_a_result a_results in
          Enum.concat b_results
      in
      Enum.singleton @@ Suspended(thunk', reference_state)
  in
  let bind_one_world
      (world : state -> a result Enum.t)
    : (state -> b result Enum.t) =
    fun (state : state) ->
      let a_results : a result Enum.t = world state in
      let b_results : b result Enum.t =
        a_results
        |> Enum.map bind_one_a_result
        |> Enum.concat
      in
      b_results
  in
  let M(xworlds) = x in
  M(Enum.map bind_one_world xworlds)
;;

let zero (type a) () : a m = M(Enum.singleton(fun (_ : state) -> Enum.empty ()))

let pick (type a) (items : a Enum.t) : a m =
  M(
    items
    |> Enum.map
      (fun item ->
         fun state -> Enum.singleton (Completed(item, state))
      )
  )
;;

let pause () : unit m =
  M(Enum.singleton(fun state ->
      Enum.singleton @@ Suspended(
        (fun () -> Enum.singleton @@ Completed ((), state)), state)
    ))
;;

let _update_state (fn : state -> state option) : unit m =
  M(Enum.singleton(fun state ->
      match fn state with
      | None -> Enum.empty ()
      | Some state' -> Enum.singleton(Completed((), state'))
    ))
;;

let record_decision (s : Symbol.t) (x : Ident.t) (c : clause) (x' : Ident.t)
  : unit m =
  let state_fn state =
    let current = Symbol_map.Exceptionless.find s state.st_decisions in
    match current with
    | None ->
      Some { state with
             st_decisions = Symbol_map.add s (x,c,x') state.st_decisions
           }
    | Some(x_,c_,x'_) ->
      if equal_ident x x_ && equal_clause c c_ && equal_ident x' x'_ then
        Some state
      else
        None
  in
  _update_state state_fn
;;

let record_formula (formula : Formula.t) : unit m =
  let state_fn state =
    try
      let state' = { state with
                     st_formulae = Formulae.add formula state.st_formulae
                   }
      in
      Some state'
    with
    | Formulae.SymbolTypeContradiction _  ->
      None
  in
  _update_state state_fn
;;

let check_formulae () : unit m =
  M(Enum.singleton(fun state ->
      if Solve.solve state.st_formulae then
        Enum.singleton(Completed((), state))
      else
        Enum.empty ()
    ))
;;

(* **** Evaluation types **** *)

(**
   An evaluation is either a completed value together with the state at the time
   it was reached or an unevaluated value waiting to be pulled on.  The state
   value in an unevaluated value is taken from the time it was suspended.
*)
type 'a evaluation =
  | Evaluated of 'a * state
  | Unevaluated of (unit -> 'a evaluation Enum.t) * state
;;

(* **** Evaluation operations **** *)

let _step_m (type a) (state : state) (x : a m) : a evaluation Enum.t =
  let rec result_to_evaluation (result : a result) : a evaluation =
    match result with
    | Completed(value, state') ->
      Evaluated(value, state')
    | Suspended(thunk, state') ->
      Unevaluated(
        (fun () -> Enum.map result_to_evaluation @@ thunk ()),
        state'
      )
  in
  let M(worlds) = x in
  worlds
  |> Enum.map
    (fun world ->
       let a_results = world state in
       a_results
       |> Enum.map result_to_evaluation
    )
  |> Enum.concat
;;

let start (type a) (x : a m) : a evaluation =
  Unevaluated((fun () -> _step_m initial_state x), initial_state)
;;

let step (type a) (e : a evaluation) : a evaluation Enum.t =
  match e with
  | Evaluated _ -> Enum.singleton e
  | Unevaluated(fn, _) -> fn ()
;;

let get_formulae (type a) (e : a evaluation) : Formulae.t =
  match e with
  | Evaluated(_,state)
  | Unevaluated(_,state) ->
    state.st_formulae
;;

let get_result (type a) (e : a evaluation) : a option =
  match e with
  | Evaluated(result,_) -> Some result
  | Unevaluated _ -> None
;;
