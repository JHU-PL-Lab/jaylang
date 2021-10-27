open Core

let phi_log = Logs.Src.create "phi"

let log_choices_complete debug choices_complete_z3 =
  if debug then
    Logs.debug ~src:phi_log (fun m ->
        m "Z3_choices_complete: %a"
          Fmt.(Dump.list string)
          (List.map ~f:Z3.Expr.to_string choices_complete_z3))
  else
    ()
