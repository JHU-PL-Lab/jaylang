
module T =
  struct
    type t =
      { quit_on_abort : bool
      ; global_timeout_sec : float
      ; solver_timeout_sec : float
      ; global_max_step : int
      ; print_solver : bool }
  end

include T

let default : t =
  { quit_on_abort      = true
  ; global_timeout_sec = 120.0
  ; solver_timeout_sec = 1.0
  ; global_max_step    = Core.Int.(2 * 10 ** 3)
  ; print_solver       = false }


module With_options =
  struct
    type 'a t =
      ?global_timeout_sec    : float (* default 120.0 seconds *)
      -> ?solver_timeout_sec : float (* default 1.0 seconds *)
      -> ?quit_on_abort      : bool  (* default true *)
      -> ?global_max_step    : int   (* default 2000 steps *)
      -> ?print_solver       : bool  (* default false *)
      -> 'a (* 'a = 'b -> 'c *) (* so maybe make this a ('a, 'b) t *)

    let appl (x : 'a t) (r : T.t) : 'a =
      x
        ~global_timeout_sec:r.global_timeout_sec
        ~solver_timeout_sec:r.solver_timeout_sec
        ~quit_on_abort:r.quit_on_abort
        ~global_max_step:r.global_max_step
        ~print_solver:r.print_solver

    let make (f : T.t -> 'a) : 'a t =
      fun
      ?(global_timeout_sec : float = default.global_timeout_sec)
      ?(solver_timeout_sec : float = default.solver_timeout_sec)
      ?(quit_on_abort      : bool  = default.quit_on_abort)
      ?(global_max_step    : int   = default.global_max_step)
      ?(print_solver       : bool  = default.print_solver)
      ->
      { global_timeout_sec
      ; solver_timeout_sec
      ; quit_on_abort
      ; global_max_step
      ; print_solver }
      |> f 

    let map (x : 'a t) (f : 'a -> 'b) : 'b t =
      let g = fun r -> f (appl x r)
      in
      make g
      
  end