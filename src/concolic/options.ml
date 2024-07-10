open Core

module T =
  struct
    type t =
      { quit_on_abort      : bool  [@default false]
      ; global_timeout_sec : float [@default 30.0]
      ; solver_timeout_sec : float [@default 1.0]
      ; global_max_step    : int   [@default Int.(50 * 10**3)]
      ; max_tree_depth     : int   [@default 50]
      ; random             : bool  [@default false]}
      [@@deriving sexp]
  end

include T
let default : t = T.t_of_sexp @@ Sexp.of_string "()"

module Refs =
  struct
    type t =
      { quit_on_abort      : bool ref
      ; global_timeout_sec : float ref
      ; solver_timeout_sec : float ref
      ; global_max_step    : int ref
      ; max_tree_depth     : int ref
      ; random             : bool ref }

    let create_default () : t =
      { quit_on_abort      = ref default.quit_on_abort
      ; global_timeout_sec = ref default.global_timeout_sec
      ; solver_timeout_sec = ref default.solver_timeout_sec
      ; global_max_step    = ref default.global_max_step
      ; max_tree_depth     = ref default.max_tree_depth
      ; random             = ref default.random }

    let without_refs (x : t) : T.t =
      { quit_on_abort      = !(x.quit_on_abort)
      ; global_timeout_sec = !(x.global_timeout_sec)
      ; solver_timeout_sec = !(x.solver_timeout_sec)
      ; global_max_step    = !(x.global_max_step)
      ; max_tree_depth     = !(x.max_tree_depth)
      ; random             = !(x.random) }
  end

(* `Fun` for optional arguments on functions *)
module Fun =
  struct
    type 'a t =
      ?global_timeout_sec    : float
      -> ?solver_timeout_sec : float
      -> ?quit_on_abort      : bool
      -> ?global_max_step    : int
      -> ?max_tree_depth     : int
      -> ?random             : bool
      -> 'a (* 'a = 'b -> 'c *) (* so maybe make this a ('a, 'b) t *)

    let appl (x : 'a t) (r : T.t) : 'a =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~quit_on_abort:r.quit_on_abort
        ~global_max_step:r.global_max_step
        ~max_tree_depth:r.max_tree_depth
        ~random:r.random

    let make (f : T.t -> 'a) : 'a t =
      fun
      ?(global_timeout_sec : float = default.global_timeout_sec)
      ?(solver_timeout_sec : float = default.solver_timeout_sec)
      ?(quit_on_abort      : bool  = default.quit_on_abort)
      ?(global_max_step    : int   = default.global_max_step)
      ?(max_tree_depth     : int   = default.max_tree_depth)
      ?(random             : bool  = default.random)
      ->
      { global_timeout_sec
      ; solver_timeout_sec
      ; quit_on_abort
      ; global_max_step
      ; max_tree_depth
      ; random }
      |> f 

    let map (x : ('a -> 'b) t) (f : 'b -> 'c) : ('a -> 'c) t =
      let g = fun r -> fun a -> f (appl x r a)
      in
      make g

    let compose (x : ('a -> 'b) t) (f : 'c -> 'a) : ('c -> 'b) t =
      let g = fun r -> fun c -> appl x r @@ f c
      in
      make g

    let (@.) = compose

    let (||>) a b = b @. a

  end