
open Core
open Interp_common

(* monad to handle all of the effects *)

module Feeder = Input_feeder.Using_stackkey

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
    { time   : Callstack.t
    ; feeder : Feeder.t
    ; env    : Value.env }

  let empty : t =
    { time = Callstack.empty
    ; feeder = Feeder.zero
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

  This is why we use Baby in the maps in the grammar.
*)
module State = struct
  type t =
    { symbol_env : Symbol_map.t
    ; pending_proofs : Pending_proofs.t } 

  let empty : t =
    { symbol_env = Symbol_map.empty
    ; pending_proofs = Pending_proofs.empty }

  (* If we end up logging inputs, then I'll just add a label to the state and cons them there *)
end

include Interp_common.Effects.Make (State) (Env) (struct
  include Err
  let fail_on_nondeterminism_misuse (_ : State.t) : t =
    XAbort ("Nondeterminism used when not allowed.", Callstack.empty) (* FIXME : use actual callstack *)
  let fail_on_fetch (id : Lang.Ast.Ident.t) (_ : State.t) : t =
    XUnboundVariable (id, Callstack.empty) (* FIXME : same here *)
end)

(*
  -------
  RUNNING
  -------
*)

let run_on_empty (x : 'a m) : ('a, Err.t) result * State.t =
  run
    x
    State.empty
    { env = Env.empty ; det_depth = `Depth 0 }

(*
  ------
  RESULT
  ------
*)

let fail_at_time (err : Callstack.t -> Err.t) : 'a m =
  { run = fun ~reject ~accept:_ s e -> 
    reject (err e.env.time) s
  }

let abort (msg : string) (p : Lang.Ast.Program_point.t) : 'a m =
  fail_at_time (fun t -> Err.XAbort (msg, Callstack.cons p t))

let type_mismatch (msg : string) : 'a m =
  fail_at_time (fun t -> Err.XTypeMismatch (msg, t))

let diverge (p : Lang.Ast.Program_point.t) : 'a m =
  fail_at_time (fun t -> Err.XDiverge (Callstack.cons p t))

let unbound_variable (id : Lang.Ast.Ident.t) : 'a m =
  fail_at_time (fun t -> Err.XUnboundVariable (id, t))

(*
  -----------
  ENVIRONMENT
  -----------
*)

let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Value.t) (x : 'a m) : 'a m =
  local (fun e -> { e with env = Value.Env.add id v e.env }) x

let[@inline always][@specialise][@landmark] with_program_point (p : Lang.Ast.Program_point.t) (x : 'a m) : 'a m =
  local (fun e -> { e with time = Callstack.cons p e.time }) x

let[@inline always] local_env (f : Value.env -> Value.env) (x : 'a m) : 'a m =
  local (fun e -> { e with env = f e.env }) x

let get_input (type a) (key : a Key.Stackkey.t) : Value.t m =
  { run = fun ~reject:_ ~accept s e -> 
    let v = e.env.feeder.get key in
    match key with
    | I _ -> accept (Value.VInt v) s
    | B _ -> accept (Value.VBool v) s
  }

(*
  -----
  STATE
  -----
*)

let push_deferred_proof (symb : Value.symb) (work : Value.closure) : unit m =
  modify (fun s -> { s with pending_proofs = Pending_proofs.push symb work s.pending_proofs })

let pop_deferred_proof (symb : Value.symb) : Value.closure m =
  let%bind s = get in
  match Pending_proofs.pop symb s.pending_proofs with
  | Some (closure, pending) ->
    let%bind () = modify (fun s -> { s with pending_proofs = pending }) in
    return closure
  | None -> failwith "no deferred proof for given symbol"

let remove_greater_symbols (symb : Value.symb) : unit m =
  modify (fun s -> 
    { symbol_env = Symbol_map.cut symb s.symbol_env
    ; pending_proofs = Pending_proofs.cut symb s.pending_proofs }
  )

let[@inline always] run_on_deferred_proof (symb : Value.symb) (f : Lang.Ast.Embedded.With_program_points.t -> 'a m) : 'a m =
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
  { run = fun ~reject:_ ~accept s _ -> accept (Stack_map.find_opt t s.symbol_env) s }

