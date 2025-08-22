
open Core

(*
  This is an alternative to the `Stat` module that describes
  a single statistic, like the time alone.
*)

type t = 
  { interp_time  : Mtime.Span.t
  ; solve_time   : Mtime.Span.t
  ; total_time   : Mtime.Span.t
  ; n_interps    : int
  ; n_solves     : int
  (* ; n_vanishes   : int *)
  ; target_depth : int option
  ; error_depth  : int option
  }

let create () = 
  { interp_time  = Mtime.Span.zero
  ; solve_time   = Mtime.Span.zero
  ; total_time   = Mtime.Span.zero
  ; n_interps    = 0
  ; n_solves     = 0
  (* ; n_vanishes   = 0 *)
  ; target_depth = None
  ; error_depth  = None
  }

let sum_opts x y = Option.merge x y ~f:(+)

let combine a b =
  { interp_time = Mtime.Span.add a.interp_time b.interp_time
  ; solve_time = Mtime.Span.add a.solve_time b.solve_time
  ; total_time = Mtime.Span.add a.total_time b.total_time
  ; n_interps = a.n_interps + b.n_interps
  ; n_solves = a.n_solves + b.n_solves
  ; target_depth = sum_opts a.target_depth b.target_depth
  ; error_depth = sum_opts a.error_depth b.error_depth
  }
