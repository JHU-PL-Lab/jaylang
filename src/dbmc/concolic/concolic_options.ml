
module T =
  struct
    type t =
      { quit_on_abort      : bool
      ; global_timeout_sec : float
      ; solver_timeout_sec : float
      ; global_max_step    : int
      ; print_solver       : bool
      ; max_tree_depth     : int }
  end

include T

let default : t =
  { quit_on_abort      = false
  ; global_timeout_sec = 120.0
  ; solver_timeout_sec = 1.0
  ; global_max_step    = Core.Int.(2 * 10 ** 3)
  ; print_solver       = false
  ; max_tree_depth     = 25 }

module Refs =
  struct
    type t =
      { quit_on_abort      : bool ref
      ; global_timeout_sec : float ref
      ; solver_timeout_sec : float ref
      ; global_max_step    : int ref
      ; print_solver       : bool ref
      ; max_tree_depth     : int ref }

    let create_default () : t =
      { quit_on_abort      = ref default.quit_on_abort
      ; global_timeout_sec = ref default.global_timeout_sec
      ; solver_timeout_sec = ref default.solver_timeout_sec
      ; global_max_step    = ref default.global_max_step
      ; print_solver       = ref default.print_solver
      ; max_tree_depth     = ref default.max_tree_depth }

    let without_refs (x : t) : T.t =
      { quit_on_abort      = !(x.quit_on_abort)
      ; global_timeout_sec = !(x.global_timeout_sec)
      ; solver_timeout_sec = !(x.solver_timeout_sec)
      ; global_max_step    = !(x.global_max_step)
      ; print_solver       = !(x.print_solver)
      ; max_tree_depth     = !(x.max_tree_depth) }
  end

module With_options =
  struct
    type 'a t =
      ?global_timeout_sec    : float (* default 120.0 seconds *)
      -> ?solver_timeout_sec : float (* default 1.0 seconds *)
      -> ?quit_on_abort      : bool  (* default false *)
      -> ?global_max_step    : int   (* default 2000 steps *)
      -> ?print_solver       : bool  (* default false *)
      -> ?max_tree_depth     : int   (* default 25 *)
      -> 'a (* 'a = 'b -> 'c *) (* so maybe make this a ('a, 'b) t *)

    let appl (x : 'a t) (r : T.t) : 'a =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~quit_on_abort:r.quit_on_abort
        ~global_max_step:r.global_max_step
        ~print_solver:r.print_solver
        ~max_tree_depth:r.max_tree_depth

    let make (f : T.t -> 'a) : 'a t =
      fun
      ?(global_timeout_sec : float = default.global_timeout_sec)
      ?(solver_timeout_sec : float = default.solver_timeout_sec)
      ?(quit_on_abort      : bool  = default.quit_on_abort)
      ?(global_max_step    : int   = default.global_max_step)
      ?(print_solver       : bool  = default.print_solver)
      ?(max_tree_depth     : int   = default.max_tree_depth)
      ->
      { global_timeout_sec
      ; solver_timeout_sec
      ; quit_on_abort
      ; global_max_step
      ; print_solver
      ; max_tree_depth }
      |> f 

    let map (x : 'a t) ~(f : 'a -> 'b) : 'b t =
      let g = fun r -> f (appl x r)
      in
      make g

    let bind (x : 'a t) ~(f : 'a -> 'b t) : 'b t =
      let g =
        fun r ->
          appl (f (appl x r)) r
      in
      make g
  end