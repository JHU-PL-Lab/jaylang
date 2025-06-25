
open Core

(* monad to handle all of the effects *)

module Env = struct
  (* stuff to read from *)
  type t =
    { time   : Timestamp.t
    ; feeder : Grammar.Input_feeder.t
    ; env    : Value.env }

  let empty : t =
    { time = Timestamp.zero
    ; feeder = Grammar.Input_feeder.zero
    ; env = Lang.Ast.Ident.Map.empty }
end

(*
  For some reason I don't know, Core.Map likes to take linear time to compute
  the size of a map. Who cares.

  Baby, on the other hand, offers an O(log n) cut operation, as opposed to
  O(m + log n) where m is the size of the resulting map, offered from Core.

  This is why we use Baby in the maps in Grammar.
*)
module State = struct
  type t =
    { symbol_env : Grammar.Symbol_map.t
    ; pending_proofs : Grammar.Pending.t } 

  let empty : t =
    { symbol_env = Grammar.Symbol_map.empty
    ; pending_proofs = Grammar.Pending.empty }

  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)
end

(*
  ------
  BASICS
  ------
*)

(* I don't think errors will be this simple because we need to be ready to continue on younger proofs *)
(* The error needs to produce new state *)
type 'a m = {
  run : 'r. reject:(Value.err -> State.t -> 'r) -> accept:('a -> State.t -> 'r) -> Env.t -> State.t -> 'r
}

let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  { run = fun ~reject ~accept e s ->
    x.run e s ~reject ~accept:(fun a s ->
      (f a).run ~accept ~reject e s
    )
  }

let return (a : 'a) : 'a m =
  { run = fun ~reject:_ ~accept _ s -> accept a s }


(*
  ------
  RESULT
  ------
*)

let fail (err : Timestamp.t -> Value.err) : 'a m =
  { run = fun ~reject ~accept:_ e s -> 
    reject (err e.time) s
  }

let abort : Value.err m =
  fail (fun t -> Value.VAbort t)

let type_mismatch (msg : string) : Value.err m =
  fail (fun t -> Value.VTypeMismatch (t, msg))

let diverge : Value.err m =
  fail (fun t -> Value.VDiverge t)

(*
  -----------
  ENVIRONMENT
  -----------
*)

let read_env : Value.env m =
  { run = fun ~reject:_ ~accept e s -> accept e.env s }

let local (f : Env.t -> Env.t) (x : 'a m) : 'a m =
  { run = fun ~reject ~accept e s -> x.run ~reject ~accept (f e) s }

let local_env (f : Value.env -> Value.env) : 'a m -> 'a m =
  local (fun e -> { e with env = f e.env })

let fetch (id : Lang.Ast.Ident.t) : Value.safe_t option m =
  { run = fun ~reject:_ ~accept e s -> accept (Map.find e.env id) s }

let with_binding (id : Lang.Ast.Ident.t) (v : Value.nonerr) (x : 'a m) : 'a m =
  local_env (Map.set ~key:id ~data:(Value.Safe v)) x

(*
  -----
  STATE
  -----
*)

let get : State.t m =
  { run = fun ~reject:_ ~accept _ s -> accept s s }

let modify (f : State.t -> State.t) : unit m =
  { run = fun ~reject:_ ~accept _ s -> accept () (f s) }

let push_deferred_proof (symb : Value.symb) (work : Value.closure) : unit m =
  modify (fun s -> { s with pending_proofs = Grammar.Pending.push symb work s.pending_proofs })

let pop_deferred_proof (symb : Value.symb) : Value.closure m =
  let%bind s = get in
  match Grammar.Pending.pop symb s.pending_proofs with
  | Some (closure, pending) ->
    let%bind () = modify (fun s -> { s with pending_proofs = pending }) in
    return closure
  | None -> failwith "no deferred proof for given symbol"

let remove_greater_symbols (symb : Value.symb) : unit m =
  modify (fun s -> 
    { symbol_env = Grammar.Symbol_map.cut symb s.symbol_env
    ; pending_proofs = Grammar.Pending.cut symb s.pending_proofs }
  )

let run_on_deferred_proof (symb : Value.symb) (f : Lang.Ast.Embedded.With_program_points.t -> 'a m) : 'a m =
  let%bind closure = pop_deferred_proof symb in
  local (fun e ->
    (* sets concrete environment and the timestamp *)
    { e with env = closure.env ; time = Value.timestamp_of_symbol symb }
  ) (f closure.body)


(*
  ---------------
  DEFERRED VALUES
  ---------------
*)

let lookup (Value.VSymbol t : Value.symb) : Value.whnf option m =
  { run = fun ~reject:_ ~accept _ s -> accept (Timestamp.Map.find_opt t s.symbol_env) s }

(*
  -------
  RUNNING
  -------
*)

let run_on_empty (x : 'a m) : ('a, Value.err) result * State.t =
  x.run
    ~reject:(fun a s -> Error a, s)
    ~accept:(fun a s -> Ok a, s)
    Env.empty
    State.empty

