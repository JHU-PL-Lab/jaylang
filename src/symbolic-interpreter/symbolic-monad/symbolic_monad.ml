(*
The implementation of this monad includes the following features:

  * Suspension via coroutine
  * Logging via writer (with sanity checking via a "listen"-like mechanism)
  * Nondeterminism via lists

Note that nondeterminism doesn't play nicely with most other features.  Using
transformers, it is possible to produce nondeterminism *independent* of other
features, but we want the features to interact.  In the event that incoherent
logs are written (e.g. conflicting decisions in function wiring choices), for
example, we want computation to zero.  This requires us to customize the monad
rather than relying on transformer definitions.
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

type log = {
  log_formulae : Formulae.t;
  log_decisions : decision_map;
};;

(* **** Log utilities **** *)

let empty_log = {
  log_formulae = Formulae.empty;
  log_decisions = Symbol_map.empty;
};;

exception MergeFailure;;

let merge_logs (log1 : log) (log2 : log) : log option =
  let open Option.Monad in
  let%bind merged_formulae =
    try
      Some(Formulae.union log1.log_formulae log2.log_formulae)
    with
    | Formulae.SymbolTypeContradiction _ -> None
  in
  let merge_fn _key a b =
    match a,b with
    | None,None -> None
    | Some x,None -> Some x
    | None,Some x -> Some x
    | Some(x1,c1,x1'),Some(x2,c2,x2') ->
      if equal_ident x1 x2 && equal_ident x1' x2' && equal_clause c1 c2 then
        Some(x1,c1,x1')
      else
        raise MergeFailure
  in
  let%bind merged_decisions =
    try
      Some(Symbol_map.merge merge_fn log1.log_decisions log2.log_decisions)
    with
    | MergeFailure -> None
  in
  return
    { log_formulae = merged_formulae;
      log_decisions = merged_decisions;
    }
;;

(* **** Monad types **** *)

type 'a m =
  | M of 'a result list

(** A result is either a completed expression (with its resulting state) or a
    suspended function which, when invoked, will step to the next result.  The
    suspended case carries the log of the computation at the time it was
    suspended. *)
and 'a result =
  | Completed : 'a * log -> 'a result
  | Suspended : (unit -> 'a result list) * log -> 'a result
;;

(* **** Monadic operations **** *)

let return (type a) (v : a) : a m =
  M([Completed (v, empty_log)])
;;

let bind (type a) (type b) (x : a m) (f : a -> b m) : b m =
  let rec append_log_in_result (log : log) (result : b result)
    : b result option =
    match result with
    | Completed(value, log') ->
      begin
        match merge_logs log' log with
        | None -> None
        | Some log'' -> Some(Completed(value, log''))
      end
    | Suspended(fn, log') ->
      let fn' () =
        fn ()
        |> List.filter_map (append_log_in_result log)
      in
      Some(Suspended(fn', log'))
  in
  let rec bind_one (a_world : a result) : b result list =
    match a_world with
    | Completed(value,log) ->
      let M(b_worlds) = f value in
      b_worlds
      |> List.filter_map (append_log_in_result log)
    | Suspended(fn,log) ->
      let fn' () =
        let a_worlds = fn () in
        let b_worlds = List.map bind_one a_worlds in
        List.concat b_worlds
      in
      [Suspended(fn', log)]
  in
  let M(a_worlds) = x in
  M(List.concat @@ List.map bind_one a_worlds)
;;

let zero (type a) () : a m = M([]);;

let pick (type a) (items : a Enum.t) : a m =
  M(items
    |> Enum.map (fun x -> Completed(x, empty_log))
    |> List.of_enum
   )
;;

let pause () : unit m =
  M([Suspended((fun () -> [Completed((), empty_log)]), empty_log)])
;;

let record_decision (s : Symbol.t) (x : Ident.t) (c : clause) (x' : Ident.t)
  : unit m =
  M([Completed((),
               { log_formulae = Formulae.empty;
                 log_decisions = Symbol_map.singleton s (x,c,x');
               }
              )
    ])
;;

let record_formula (formula : Formula.t) : unit m =
  M([Completed((),
               { log_formulae = Formulae.singleton formula;
                 log_decisions = Symbol_map.empty;
               }
              )
    ])
;;

let check_formulae (type a) (x : a m) : a m =
  let check_world (world : a result) : a result option =
    match world with
    | Completed(value,log) ->
      if Solver.solvable log.log_formulae then
        Some(Completed(value,log))
      else
        None
    | Suspended(fn,log) ->
      if Solver.solvable log.log_formulae then
        Some(Suspended(fn,log))
      else
        None
  in
  let M(worlds) = x in
  M(List.filter_map check_world worlds)
;;

(* **** Evaluation types **** *)

(**
   An evaluation is either a completed value together with the state at the time
   it was reached or an unevaluated value waiting to be pulled on.  The state
   value in an unevaluated value is taken from the time it was suspended.
*)
type 'a evaluation =
  | Evaluated of 'a * log
  | Unevaluated of (unit -> 'a evaluation Enum.t) * log
;;

(* **** Evaluation operations **** *)

let _step_m (type a) (x : a m) : a evaluation Enum.t =
  let rec result_to_evaluation (result : a result) : a evaluation =
    match result with
    | Completed(value, log') ->
      Evaluated(value, log')
    | Suspended(thunk, log') ->
      Unevaluated(
        (fun () -> Enum.map result_to_evaluation @@ List.enum @@ thunk ()),
        log'
      )
  in
  let M(worlds) = x in
  worlds
  |> List.enum
  |> Enum.map result_to_evaluation
;;

let start (type a) (x : a m) : a evaluation =
  Unevaluated((fun () -> _step_m x), empty_log)
;;

let step (type a) (e : a evaluation) : a evaluation Enum.t =
  match e with
  | Evaluated _ -> Enum.singleton e
  | Unevaluated(fn, _) -> fn ()
;;

let get_formulae (type a) (e : a evaluation) : Formulae.t =
  match e with
  | Evaluated(_,log)
  | Unevaluated(_,log) ->
    log.log_formulae
;;

let get_result (type a) (e : a evaluation) : a option =
  match e with
  | Evaluated(result,_) -> Some result
  | Unevaluated _ -> None
;;
