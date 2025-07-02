
open Core
open Lang

module type ENV = sig
  type value
  type t
  val empty : t
  val fetch : Ast.Ident.t -> t -> value option
end

module Make (State : T) (Env : ENV) (Err : sig
  type t
  val fail_on_nondeterminism_misuse : State.t -> t
  val fail_on_fetch : Ast.Ident.t -> State.t -> t
end) = struct
  module Read = struct
    type t = 
      { env : Env.t
      ; det_depth : [ `Escaped | `Depth of int ] } 

    let empty : t = { env = Env.empty ; det_depth = `Depth 0 }

    let is_determinism_allowed ({ det_depth ; _ } : t) : bool =
      match det_depth with
      | `Escaped -> true
      | `Depth i -> i = 0
  end

  (* 
    ------------
    MONAD BASICS 
    ------------

    CPS monad (mostly because it is an effecient way to error: just jump straight out instead
    of pattern matching you're way out) with error, state, and environment.

    The error case returns state, too.
  *)
  type 'a m = {
    run : 'r. reject:(Err.t -> State.t -> 'r) -> accept:('a -> State.t -> 'r) -> State.t -> Read.t -> 'r
  } 

  let[@inline always][@specialise] bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    { run =
      fun ~reject ~accept s r ->
        x.run s r ~reject ~accept:(fun x s ->
          (f x).run ~reject ~accept s r
        )
    }

  let[@inline always][@specialise] return (a : 'a) : 'a m =
    { run = fun ~reject:_ ~accept s _ -> accept a s }
  (*
    -----------
    ENVIRONMENT
    -----------
  *)
  let read : Read.t m =
    { run = fun ~reject:_ ~accept s r -> accept r s }

  let read_env : Env.t m =
    let%bind { env ; _ } = read in
    return env

  let[@inline always][@specialise] local (f : Read.t -> Read.t) (x : 'a m) : 'a m =
    { run = fun ~reject ~accept s r -> x.run ~reject ~accept s (f r) }

  let[@inline always][@specialise] local_env (f : Env.t -> Env.t) (x : 'a m) : 'a m =
    local (fun r -> { r with env = f r.env }) x

  (*
    -----
    STATE
    -----
  *)

  let get : State.t m =
    { run = fun ~reject:_ ~accept s _ -> accept s s }

  let[@inline always][@specialise] modify (f : State.t -> State.t) : unit m =
    { run =
      fun ~reject:_ ~accept s _ ->
        accept () (f s)
    }

  (*
    -----
    ERROR
    -----
  *)

  let[@inline always][@specialise] fail (e : Err.t) : 'a m =
    { run = fun ~reject ~accept:_ s _ -> reject e s }

  let fail_map (f : State.t -> Err.t) : 'a m = 
    let%bind state = get in
    fail @@ f state

  (*
    ------------------
    ESCAPING THE MONAD
    ------------------
  *)
  (* May prefer to pass in only init_env, but init_read gives more flexibility *)
  let run (x : 'a m) (init_state : State.t) (init_read : Read.t) : ('a, Err.t) result * State.t =
    x.run ~reject:(fun e s -> Error e, s) ~accept:(fun a s -> Ok a, s) init_state init_read

  (*
    -----------------
    INTERPRETER STUFF
    -----------------
  *)

  let[@inline always][@specialise] with_incr_depth (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth =
      match r.det_depth with
      | `Escaped -> `Escaped
      | `Depth i -> `Depth (i + 1)
      }
    ) x

  let[@inline always][@specialise] with_escaped_det (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth = `Escaped}) x

  let assert_nondeterminism : unit m =
    let%bind r = read in
    if Read.is_determinism_allowed r
    then return ()
    else fail_map Err.fail_on_nondeterminism_misuse

  let[@inline always] fetch (id : Ast.Ident.t) : Env.value m =
    let%bind env = read_env in
    match Env.fetch id env with
    | None -> fail_map @@ Err.fail_on_fetch id
    | Some v -> return v
end
