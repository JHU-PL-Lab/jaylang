open Core

let no_matter sec f =
  match sec with
  | Some sec -> Lwt_unix.with_timeout (Time_float.Span.to_sec sec) f
  | None -> f ()
