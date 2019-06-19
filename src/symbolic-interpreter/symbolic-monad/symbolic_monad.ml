(*
The implementation of this monad includes the following features:

  * Suspension via coroutine
  * Logging via writer (with sanity checking via a "listen"-like mechanism)
  * Nondeterminism via lists
  * State for caching common computations

Note that nondeterminism doesn't play nicely with most other features.  Using
transformers, it is possible to produce nondeterminism *independent* of other
features, but we want the features to interact.  In the event that incoherent
logs are written (e.g. conflicting decisions in function wiring choices), for
example, we want computation to zero.  This requires us to customize the monad
rather than relying on transformer definitions.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Interpreter_types;;
(* open Relative_stack;; *)
open Sat_types;;

let lazy_logger =
  Logger_utils.make_lazy_logger "Symbolic_monad"
;;

module type Spec = sig
  module Cache_key : Interfaces.OrderedType;;
end;;

module type S = sig
  module Spec : Spec;;

  type 'a m;;

  val return : 'a -> 'a m;;
  val bind : 'a m -> ('a -> 'b m) -> 'b m;;
  val zero : unit -> 'a m;;
  val pick : 'a Enum.t -> 'a m;;
  val pause : unit -> unit m
  val record_decision : Symbol.t -> Ident.t -> clause -> Ident.t -> unit m;;
  val record_formula : Formula.t -> unit m;;
  val check_formulae : 'a m -> 'a m;;

  type 'a evaluation;;

  val start : 'a m -> 'a evaluation;;
  val step : 'a evaluation -> ('a * Formulae.t) Enum.t * 'a evaluation;;
  val is_complete : 'a evaluation -> bool;;
end;;

(** The interface of the functor producing symbolic monads. *)
module Make(Spec : Spec) : S with module Spec = Spec
=
struct
  module Spec = Spec;;

  (* **** Supporting types **** *)

  type decision_map =
    (Ident.t * clause * Ident.t) Symbol_map.t
  ;;

  type log = {
    log_formulae : Formulae.t;
    log_decisions : decision_map;
  };;

  type cache = {
    (* TODO: actually useful cache info *)
    cache_unit : unit;
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
    | M of (cache -> 'a result list * cache)

  (** A result is either a completed expression (with its resulting state) or a
      suspended function which, when invoked, will step to the next result.  The
      suspended case carries the log of the computation at the time it was
      suspended. *)
  and 'a result =
    | Completed : 'a * log -> 'a result
    | Suspended : 'a m * log -> 'a result
  ;;

  (* **** Monadic operations **** *)

  let return (type a) (v : a) : a m =
    M(fun cache -> ([Completed (v, empty_log)], cache))
  ;;

  let bind (type a) (type b) (x : a m) (f : a -> b m) : b m =
    let rec append_log (log : log) (x : b result) : b result option =
      match x with
      | Completed(value,log') ->
        begin
          match merge_logs log' log with
          | None -> None
          | Some log'' -> Some(Completed(value,log''))
        end
      | Suspended(m,log') ->
        let M(fn) = m in
        let fn' cache =
          let results,cache' = fn cache in
          let results' = List.filter_map (append_log log) results in
          results',cache'
        in
        let m' = M(fn') in
        begin
          match merge_logs log' log with
          | None -> None
          | Some log'' -> Some(Suspended(m',log''))
        end
    in
    let rec bind_worlds_fn
        (worlds_fn : cache -> a result list * cache)
        (cache : cache)
      : b result list * cache =
      let worlds, cache' = worlds_fn cache in
      let bound_worlds, cache'' =
        worlds
        |> List.fold_left
          (fun (result_worlds, fold_cache) world ->
             match world with
             | Completed(value,log) ->
               let M(fn) = f value in
               let results, fold_cache' = fn fold_cache in
               let results' = List.filter_map (append_log log) results in
               (results'::result_worlds, fold_cache')
             | Suspended(m,log) ->
               let M(fn) = m in
               let m' = M(bind_worlds_fn fn) in
               ([Suspended(m',log)]::result_worlds, fold_cache)
          )
          ([], cache')
      in
      (List.concat bound_worlds, cache'')
    in
    let M(worlds_fn) = x in
    M(bind_worlds_fn worlds_fn)
  ;;

  let zero (type a) () : a m = M(fun cache -> ([], cache));;

  let pick (type a) (items : a Enum.t) : a m =
    M(fun cache ->
       (items
        |> Enum.map (fun x -> Completed(x, empty_log))
        |> List.of_enum
       ),
       cache
     )
  ;;

  let pause () : unit m =
    M(fun cache ->
       [Suspended(M(fun cache -> ([Completed((), empty_log)], cache)),
                  empty_log)
       ],
       cache
     )
  ;;

  let record_decision (s : Symbol.t) (x : Ident.t) (c : clause) (x' : Ident.t)
    : unit m =
    M(fun cache ->
       [Completed((),
                  { log_formulae = Formulae.empty;
                    log_decisions = Symbol_map.singleton s (x,c,x');
                  }
                 )
       ],
       cache
     )
  ;;

  let record_formula (formula : Formula.t) : unit m =
    M(fun cache ->
       [Completed((),
                  { log_formulae = Formulae.singleton formula;
                    log_decisions = Symbol_map.empty;
                  }
                 )
       ],
       cache
     )
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
    let M(worlds_fn) = x in
    M(fun cache ->
       let worlds, cache' = worlds_fn cache in
       (List.filter_map check_world worlds, cache')
     )
  ;;

  (* **** Evaluation types **** *)

  (**
     An evaluation is a monadic value for which evaluation has started.  It is
     representative of all of the concurrent, non-deterministic computations
     being performed as well as all metadata (such as caching).
  *)
  type 'a evaluation =
    { ev_cache : cache;
      ev_remaining : 'a m Deque.t;
    }
  ;;

  (* **** Evaluation operations **** *)

  let _step_m (type a) (cache : cache) (x : a m)
    : (a * log) list * a m list * cache =
    let M(world_fn) = x in
    let (results, cache') = world_fn cache in
    let complete, incomplete =
      List.fold_left
        (fun (complete,incomplete) result ->
           match result with
           | Completed(value,log) -> ((value,log)::complete,incomplete)
           | Suspended(m,_log) -> (complete,m::incomplete)
        )
        ([],[])
        results
    in
    (complete, incomplete, cache')
  ;;

  let start (type a) (x : a m) : a evaluation =
    let initial_cache =
      { cache_unit = () }
    in
    { ev_cache = initial_cache;
      ev_remaining = Deque.of_list [ x ]
    };
  ;;

  let step (type a) (e : a evaluation)
    : (a * Formulae.t) Enum.t * a evaluation =
    (* TODO: parameterize queuing strategy *)
    lazy_logger `trace
      (fun () ->
         Printf.sprintf "Stepping evaluation with %d alternative%s."
           (Deque.size e.ev_remaining)
           (if Deque.size e.ev_remaining = 1 then "" else "s")
      );
    match Deque.front e.ev_remaining with
    | None -> (Enum.empty (), e)
    | Some(m, ms) ->
      let (results, ms', new_cache) = _step_m e.ev_cache m in
      lazy_logger `trace
        (fun () ->
           Printf.sprintf
             "Evaluation stepping produced %d continuations(s): %d complete, %d incomplete"
             (List.length results + List.length ms')
             (List.length results) (List.length ms')
        );
      let results' =
        results
        |> List.enum
        |> Enum.map (fun (value,log) -> (value,log.log_formulae))
      in
      let all_threads = Deque.append ms @@ Deque.of_list ms' in
      ( results',
        { ev_cache = new_cache;
          ev_remaining = all_threads;
        }
      )
  ;;

  let is_complete (e : 'a evaluation) : bool =
    Deque.is_empty e.ev_remaining
  ;;
end;;
