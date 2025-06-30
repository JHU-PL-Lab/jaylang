
open Core

(* monad to handle all of the effects *)

module Env = struct
  (* stuff to read from *)
  type t =
    { time   : Timestamp.t
    ; feeder : Input_feeder.t
    ; env    : Value.env }

  let empty : t =
    { time = Timestamp.zero
    ; feeder = Input_feeder.zero
    ; env = Lang.Ast.Ident.Map.empty }
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

(*
  ------
  BASICS
  ------
*)

type 'a m = {
  run : 'r. reject:(Err.t -> State.t -> 'r) -> accept:('a -> State.t -> 'r) -> Env.t -> State.t -> 'r
}

let[@inline always] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
  { run = fun ~reject ~accept e s ->
    x.run e s ~reject ~accept:(fun a s ->
      (f a).run ~accept ~reject e s
    )
  }

let[@inline always] return (a : 'a) : 'a m =
  { run = fun ~reject:_ ~accept _ s -> accept a s }


(*
  -------
  RUNNING
  -------
*)

let run_on_empty (x : 'a m) : ('a, Err.t) result * State.t =
  x.run
    ~reject:(fun a s -> Error a, s)
    ~accept:(fun a s -> Ok a, s)
    Env.empty
    State.empty

(*
  ------
  RESULT
  ------
*)

let fail (e : Err.t) : 'a m =
  { run = fun ~reject ~accept:_ _ s -> reject e s }

let fail_at_time (err : Timestamp.t -> Err.t) : 'a m =
  { run = fun ~reject ~accept:_ e s -> 
    reject (err e.time) s
  }

let abort (msg : string) (p : Lang.Ast.Program_point.t) : 'a m =
  fail_at_time (fun t -> Err.XAbort (msg, Timestamp.cons p t))

let type_mismatch (msg : string) : 'a m =
  fail_at_time (fun t -> Err.XTypeMismatch (msg, t))

let diverge (p : Lang.Ast.Program_point.t) : 'a m =
  fail_at_time (fun t -> Err.XDiverge (Timestamp.cons p t))

let unbound_variable (id : Lang.Ast.Ident.t) : 'a m =
  fail_at_time (fun t -> Err.XUnboundVariable (id, t))

let[@inline always] handle_error (x : 'a m) (ok : 'a -> 'b m) (err : Err.t -> 'b m) : 'b m =
  { run = fun ~reject ~accept e s ->
    x.run e s 
      ~reject:(fun a s ->
        (err a).run ~reject ~accept e s
      )
      ~accept:(fun a s ->
        (ok a).run ~reject ~accept e s
      )
  }

(*
  -----------
  ENVIRONMENT
  -----------
*)

let read : Env.t m =
  { run = fun ~reject:_ ~accept e s -> accept e s }

let read_env : Value.env m =
  { run = fun ~reject:_ ~accept e s -> accept e.env s }

let[@inline always] local (f : Env.t -> Env.t) (x : 'a m) : 'a m =
  { run = fun ~reject ~accept e s -> x.run ~reject ~accept (f e) s }

let[@inline always] local_env (f : Value.env -> Value.env) : 'a m -> 'a m =
  local (fun e -> { e with env = f e.env })

let[@inline always] fetch (id : Lang.Ast.Ident.t) : Value.t option m =
  { run = fun ~reject:_ ~accept e s -> accept (Value.Env.find id e.env) s }

let[@inline always] with_binding (id : Lang.Ast.Ident.t) (v : Value.t) (x : 'a m) : 'a m =
  local_env (Value.Env.add id v) x

let[@inline always] with_program_point (p : Lang.Ast.Program_point.t) (x : 'a m) : 'a m =
  local (fun e -> { e with time = Timestamp.cons p e.time }) x

let get_input (p : Lang.Ast.Program_point.t) : Value.t m =
  { run = fun ~reject:_ ~accept e s -> accept (Value.VInt (e.feeder (Timestamp.cons p e.time))) s }

(*
  -----
  STATE
  -----
*)

let get : State.t m =
  { run = fun ~reject:_ ~accept _ s -> accept s s }

let[@inline always] modify (f : State.t -> State.t) : unit m =
  { run = fun ~reject:_ ~accept _ s -> accept () (f s) }

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
  { run = fun ~reject:_ ~accept _ s -> accept (Timestamp.Map.find_opt t s.symbol_env) s }

