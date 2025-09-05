
open Core

let time f x =
  let t0 = Mtime_clock.now () in
  let res = f x in
  let t1 = Mtime_clock.now () in
  Mtime.span t0 t1, res

let span_to_ms =
  let ms_over_ns = Mtime.Span.to_float_ns Mtime.Span.ms /. Mtime.Span.to_float_ns Mtime.Span.ns in
  fun span ->
    Mtime.Span.to_float_ns span /. ms_over_ns

let divide_span span n =
  Option.value_exn @@ 
  Mtime.Span.of_float_ns (Mtime.Span.to_float_ns span /. Int.to_float n)
