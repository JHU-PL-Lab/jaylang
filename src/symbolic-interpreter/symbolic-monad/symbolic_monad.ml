(*
The implementation of this monad includes the following features, ordered from
outermost to innermost.

  * Nondeterminism via enumerations
  * Annotatedal metadata (e.g. number of steps executed)
  * Suspended annotated
  * Annotatedal state (e.g. solver equations)
*)

open Batteries;;
open Odefa_ast;;

open Ast;;
open Interpreter_types;;
(* open Relative_stack;; *)
open Sat_types;;
open Symbolic_monad_types;;

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

let initial_metadata = {
  md_steps = 0;
};;

(* **** Monad types **** *)

type 'a computation = {
  co_state : state;
  co_metadata : metadata;
  co_suspendable : 'a suspendable;
}
and 'a suspendable =
  | Suspended : 'b suspendable * ('b -> 'a m) -> 'a suspendable
  | Evaluated : 'a -> 'a suspendable
and 'a nondeterminism = Nondeterminism of 'a Enum.t [@@ocaml.unboxed]
and 'a m = 'a computation nondeterminism;;

(* **** Monadic operations **** *)

let _return_universe (type a) (v : a) : a computation =
  { co_metadata = initial_metadata;
    co_state = initial_state;
    co_suspendable = Evaluated v;
  }
;;

let return (type a) (v : a) : a m =
  Nondeterminism(Enum.singleton (_return_universe v))
;;

let bind (type a) (type b) (x : a m) (f : a -> b m) : b m =
  let Nondeterminism(universes) = x in
  let universes' =
    universes
    |> Enum.map
      (fun universe ->
         { universe with
           co_suspendable = Suspended(universe.co_suspendable, f)
         }
      )
  in
  Nondeterminism(universes')
;;

let zero (type a) () : a m = Nondeterminism(Enum.empty ());;

let pick (type a) (items : a Enum.t) : a m =
  let universes = Enum.map _return_universe items in
  Nondeterminism(universes)
;;

let record_decision (s : Symbol.t) (x : Ident.t) (c : clause) (x' : Ident.t)
  : unit m =
  Nondeterminism(
    Enum.singleton
      { co_state = { st_formulae = Formulae.empty;
                     st_decisions = Symbol_map.singleton s (x,c,x')
                   };
        co_metadata = initial_metadata;
        co_suspendable = Evaluated ();
      })
;;

let record_formula (formula : Formula.t) : unit m =
  Nondeterminism(
    Enum.singleton
      { co_state = { st_formulae = Formulae.singleton formula;
                     st_decisions = Symbol_map.empty;
                   };
        co_metadata = initial_metadata;
        co_suspendable = Evaluated ();
      })
;;

let merge_decisions (d1 : decision_map) (d2 : decision_map)
  : decision_map option =
  try
    Some (Symbol_map.merge
            (fun _ v1o v2o ->
               match v1o,v2o with
               | None, None -> None
               | Some v1, None -> Some v1
               | None, Some v2 -> Some v2
               | Some v1, Some v2 ->
                 let (x1,c1,x1') = v1 in
                 let (x2,c2,x2') = v2 in
                 if equal_ident x1 x2 &&
                    equal_ident x1' x2' &&
                    equal_clause c1 c2 then
                   Some v1
                 else
                   raise (Failure "decision mismatch")
            )
            d1 d2)
  with
  | Failure _ -> None
;;

let merge_metadata (md1 : metadata) (md2 : metadata) : metadata =
  { md_steps = md1.md_steps + md2.md_steps }
;;

let merge_state (st1 : state) (st2 : state) : state option =
  match merge_decisions st1.st_decisions st2.st_decisions with
  | Some merged_decisions ->
    Some
      { st_formulae = Formulae.union st1.st_formulae st2.st_formulae;
        st_decisions = merged_decisions;
      }
  | None -> None
;;

let rec step_computation : 'a. 'a computation -> 'a computation Enum.t =
  fun computation ->
  match computation.co_suspendable with
  | Evaluated _ ->
    (* No steps can be taken on this value; it is finished. *)
    Enum.singleton computation
  | Suspended(y,f) ->
    (* Is y a finished value? *)
    begin
      match y with
      | Suspended _ ->
        (* y is not finished.  Take a step there. *)
        let computation' = { computation with co_suspendable = y } in
        step_computation computation'
        |> Enum.map
          (fun universe ->
             let suspendable = Suspended(universe.co_suspendable, f) in
             { universe with co_suspendable = suspendable; }
          )
      | Evaluated v ->
        (* y is already finished.  Apply f to y to generate the next suspendable
           value.  This is nondeterministic and generates its own state and
           metadata that must be merged with the current context. *)
        let Nondeterminism(universes) = f v in
        universes
        |> Enum.filter_map
          (fun universe ->
             let metadata' =
               merge_metadata universe.co_metadata computation.co_metadata
             in
             let metadata'' =
               { md_steps = metadata'.md_steps + 1 }
             in
             match merge_state universe.co_state computation.co_state with
             | Some state'' ->
               Some
                 { co_metadata = metadata'';
                   co_state = state'';
                   co_suspendable = universe.co_suspendable;
                 }
             | None -> None
          )
    end
;;

let step (type a) (x : a m) : a m =
  let Nondeterminism(universes) = x in
  let universes' =
    universes
    |> Enum.map step_computation
    |> Enum.concat
  in
  Nondeterminism(universes')

let unpack (type a) (x : a m) : a computation Enum.t =
  let Nondeterminism(universes) = x in universes
;;

let pack (type a) (universes : a computation Enum.t) : a m =
  Nondeterminism(universes)
;;

let examine (type a) (x : a computation) : a computation_state =
  match x.co_suspendable with
  | Evaluated value ->
    Computation_finished({ rs_value = value;
                           rs_formulae = x.co_state.st_formulae;
                           rs_metadata = x.co_metadata;
                         })
  | Suspended _ ->
    Computation_suspended
;;
