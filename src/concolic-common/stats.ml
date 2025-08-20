
open Core

type t = 
  { interp_time  : Mtime.Span.t
  ; solve_time   : Mtime.Span.t
  ; total_time   : Mtime.Span.t
  ; n_interps    : int
  ; n_solves     : int
  (* ; mutable n_vanishes   : int *)
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

(* Creates a new stats record without mutating the arguments *)
let combine a b =
  { interp_time = Mtime.Span.add a.interp_time b.interp_time
  ; solve_time = Mtime.Span.add a.solve_time b.solve_time
  ; total_time = Mtime.Span.add a.total_time b.total_time
  ; n_interps = a.n_interps + b.n_interps
  ; n_solves = a.n_solves + b.n_solves
  ; target_depth = sum_opts a.target_depth b.target_depth
  ; error_depth = sum_opts a.error_depth b.error_depth
  }

let time f x =
  let t0 = Mtime_clock.now () in
  let res = f x in
  let t1 = Mtime_clock.now () in
  Mtime.span t0 t1, res

let span_to_ms =
  let ms_over_ns = Mtime.Span.to_float_ns Mtime.Span.ms /. Mtime.Span.to_float_ns Mtime.Span.ns in
  fun span ->
    Mtime.Span.to_float_ns span *. ms_over_ns
