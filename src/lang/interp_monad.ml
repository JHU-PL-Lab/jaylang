
open Core

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

  include Utils.State_read_result.Make (State) (Read) (Err)

  let[@inline always][@specialise] local_env (f : Env.t -> Env.t) (x : 'a m) : 'a m =
    local (fun r -> { r with env = f r.env }) x

  let[@inline always][@specialise] with_incr_depth (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth =
      match r.det_depth with
      | `Escaped -> `Escaped
      | `Depth i -> `Depth (i + 1)
      }
    ) x

  let[@inline always][@specialise] with_escaped_det (x : 'a m) : 'a m =
    local (fun r -> { r with det_depth = `Escaped}) x

  let read_env : Env.t m =
    let%bind (_, { env ; _ }) = read in
    return env

  let fail_map f = 
    let%bind (state, _) : State.t * Read.t = read in
    fail @@ f state

  let assert_nondeterminism : unit m =
    let%bind (_, e) = read in
    if Read.is_determinism_allowed e
    then return ()
    else fail_map Err.fail_on_nondeterminism_misuse

  let[@inline always] fetch (id : Ast.Ident.t) : Env.value m =
    let%bind env = read_env in
    match Env.fetch id env with
    | None -> fail_map @@ Err.fail_on_fetch id
    | Some v -> return v
end
