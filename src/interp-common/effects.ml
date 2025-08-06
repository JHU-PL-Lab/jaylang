
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
  val fail_on_nondeterminism_misuse : State.t -> t * State.t
  val fail_on_fetch : Ast.Ident.t -> State.t -> t * State.t
  val fail_on_max_step : int -> State.t -> t * State.t
end) = struct
  module Read = struct
    type t = 
      { env : Env.t
      ; det_depth : Det_depth.t } 

    let empty : t = { env = Env.empty ; det_depth = Det_depth.zero }

    let is_determinism_allowed ({ det_depth ; _ } : t) : bool =
      Det_depth.is_determinism_allowed det_depth
  end

  type empty_err = private | (* uninhabited type *)
  let absurd (type a) (e : empty_err) : a =
    match e with _ -> . (* this function can never run *)

  (* 
    ------------
    MONAD BASICS 
    ------------

    CPS monad (mostly because it is an efficient way to error: just jump straight out instead
    of pattern matching your way out) with error, state, and environment.

    The error case returns state, too.

    This monad is inspired by one created for Binary Analysis Platform out of CMU.

    This is an indexed monad so that it can be parametrized to necessarily not error.

    All interpreters will have some max step count because we don't want nontermination.
    For efficiency, we build it into the monad instead of wrapping it up with state.
  *)
  type ('a, 'e) t = {
    run : 'r. reject:('e -> State.t -> Step.t -> 'r) -> accept:('a -> State.t -> Step.t -> 'r) -> State.t -> Step.t -> Read.t -> 'r
  } 

  let[@inline always][@specialise] bind (x : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    { run =
      fun ~reject ~accept state step r ->
        x.run state step r ~reject ~accept:(fun x state step ->
          (f x).run ~reject ~accept state step r
        )
    }

  let[@inline always][@specialise] return (a : 'a) : ('a, 'e) t =
    { run = fun ~reject:_ ~accept state step _ -> accept a state step }

  type 'a m = ('a, Err.t) t

  type 'a s = ('a, empty_err) t (* s for "safe" *)

  let make_unsafe (x : 'a s) : ('a, 'e) t =
    { run = fun ~reject:_ ~accept s r ->
      x.run s r ~reject:absurd ~accept
    }

  (*
    -----------
    ENVIRONMENT
    -----------
  *)

  let read : (Read.t, 'e) t =
    { run = fun ~reject:_ ~accept state step r -> accept r state step }

  let read_env : (Env.t, 'e) t =
    let%bind { env ; _ } = read in
    return env

  let[@inline always][@specialise] local_read (f : Read.t -> Read.t) (x : ('a, 'e) t) : ('a, 'e) t =
    { run = fun ~reject ~accept state step r -> x.run ~reject ~accept state step (f r) }

  let[@inline always][@specialise] local (f : Env.t -> Env.t) (x : ('a, 'e) t) : ('a, 'e) t =
    local_read (fun r -> { r with env = f r.env }) x

  (*
    -----
    STATE
    -----
  *)

  let get : (State.t, 'e) t =
    { run = fun ~reject:_ ~accept state step _ -> accept state state step }

  let[@inline always][@specialise] modify (f : State.t -> State.t) : (unit, 'e) t =
    { run =
      fun ~reject:_ ~accept state step _ ->
        accept () (f state) step
    }

  (*
    -----
    ERROR
    -----
  *)

  let[@inline always][@specialise] fail (e : Err.t) : 'a m =
    { run = fun ~reject ~accept:_ state step _ -> reject e state step }

  let fail_map (f : State.t -> Err.t * State.t) : 'a m = 
    { run = fun ~reject ~accept:_ state step _ -> Tuple2.uncurry reject (f state) step }

  let[@inline always] handle_error (x : ('a, 'e1) t) (ok : 'a -> ('b, 'e2) t) (err : Err.t -> ('b, 'e2) t) : ('b, 'e2) t =
    { run = fun ~reject ~accept state step e ->
      x.run state step e 
        ~reject:(fun a state step ->
          (err a).run ~reject ~accept state step e 
        )
        ~accept:(fun a state step ->
          (ok a).run ~reject ~accept state step e
        )
  }

  (*
    ------------------
    ESCAPING THE MONAD
    ------------------
  *)

  (* May prefer to pass in only init_env, but init_read gives more flexibility *)
  let run (x : 'a m) (init_state : State.t) (init_read : Read.t) : ('a, Err.t) result * State.t * Step.t =
    x.run ~reject:(fun e state step -> Error e, state, step) ~accept:(fun a state step -> Ok a, state, step) init_state Step.zero init_read

  let run_safe (x : 'a s) (init_state : State.t) (init_read : Read.t) : 'a * State.t * Step.t =
    x.run ~reject:absurd ~accept:(fun a state step -> a, state, step) init_state Step.zero init_read

  (*
    -----------------
    INTERPRETER STUFF
    -----------------
  *)

  let step : (Step.t, 'e) t =
    { run = fun ~reject:_ ~accept state step _ -> accept step state step }

  let[@inline always] incr_step ~(max_step : Step.t) : unit m = 
    { run =
      fun ~reject ~accept state step _ ->
        let (Step step_n) as step = Step.next step in
        if step_n > Step.to_int max_step
        then Tuple2.uncurry reject (Err.fail_on_max_step step_n state) step
        else accept () state step
    }


  let[@inline always][@specialise] with_incr_depth (x : ('a, 'e) t) : ('a, 'e) t =
    local_read (fun r -> { r with det_depth = Det_depth.incr r.det_depth }) x

  let[@inline always][@specialise] with_escaped_det (x : 'a m) : 'a m =
    local_read (fun r -> { r with det_depth = Det_depth.escaped }) x

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
